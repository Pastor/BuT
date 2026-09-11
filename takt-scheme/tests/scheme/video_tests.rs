//! Растр и видео: PNG, GIF и MP4 читаются обратно.
//!
//! Проверяется то, что видит машина: размер растра равен `viewBox`, фон - заливка
//! либо прозрачность, начертания найдены без системной базы, число кадров и
//! задержка, устройство контейнера MP4. Открытие MP4 плеерами проверяет человек.

use std::path::{Path, PathBuf};

use takt_scheme::raster::{Background, Tree, png};
use takt_scheme::run::{Active, Tick};
use takt_scheme::style::View;
use takt_scheme::svg::{Options, render_all};
use takt_scheme::video::{Film, gif, mp4};

fn data() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/data")
}

fn read(name: &str) -> String {
    std::fs::read_to_string(data().join(name)).expect("фикстура")
}

/// Лист `key` модели `line` в виде `options`.
fn sheet(key: &str, options: &Options) -> String {
    render_all(&read("line.takt"), &read("line.takt-ui"), options)
        .expect("чертёж")
        .into_iter()
        .find(|(k, _)| k == key)
        .map(|(_, svg)| svg)
        .expect("лист")
}

fn draft() -> Options {
    Options {
        view: View::Draft,
        legend: true,
        tick: None,
        trace: None,
        fonts: false,
    }
}

/// Растр PNG обратно: сторона и пиксели RGBA.
fn decode_png(bytes: &[u8]) -> (u32, u32, Vec<u8>) {
    let mut reader = png::Decoder::new(std::io::Cursor::new(bytes))
        .read_info()
        .expect("PNG читается");
    let mut buf = vec![0; reader.output_buffer_size().expect("размер")];
    let info = reader.next_frame(&mut buf).expect("кадр PNG");
    assert_eq!(info.color_type, png::ColorType::Rgba, "PNG несёт альфу");
    buf.truncate(info.buffer_size());
    (info.width, info.height, buf)
}

#[test]
fn a_png_has_the_size_of_the_view_box_and_the_chosen_background() {
    let svg = sheet("/", &draft());
    let size = Tree::parse(&svg).expect("svg").size();
    let filled = png(&svg, Background::Fill).expect("PNG");
    let clear = png(&svg, Background::None).expect("PNG");
    assert_eq!(
        filled,
        png(&svg, Background::Fill).expect("PNG"),
        "детерминизм"
    );

    let (w, h, pixels) = decode_png(&filled);
    assert_eq!((w, h), size, "размер растра - стороны viewBox");
    assert_eq!(
        &pixels[..4],
        &[0xF7, 0xF7, 0xF2, 0xFF],
        "угол залит полем листа"
    );
    let (_, _, pixels) = decode_png(&clear);
    assert_eq!(pixels[3], 0, "угол прозрачен");
    assert!(
        pixels.chunks(4).any(|p| p[3] == 0xFF),
        "рисунок на прозрачном фоне непрозрачен"
    );
}

/// Непрозрачных пикселей на растре текста.
fn inked(svg: &str) -> usize {
    let (_, _, pixels) = decode_png(&png(svg, Background::None).expect("PNG"));
    pixels.chunks(4).filter(|p| p[3] > 0).count()
}

#[test]
fn both_faces_are_found_without_system_fonts() {
    let text = |family: &str| {
        format!(
            r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 200 40" width="200" height="40"><text x="4" y="28" font-family="{family}" font-size="20">Ожидание S1</text></svg>"#
        )
    };
    // Семейства, которых нет ни в чертеже, ни среди вшитых: без запасного
    // шрифта текст не рисуется вовсе, и контроль видит нулевое число пикселей.
    let gost = inked(&text("'ГОСТ 2.304-81'"));
    let mono = inked(&text("'Fira Code'"));
    assert!(gost > 100 && mono > 100, "ГОСТ {gost}, моно {mono}");
    assert_ne!(
        png(&text("'ГОСТ 2.304-81'"), Background::None).unwrap(),
        png(&text("'Fira Code'"), Background::None).unwrap(),
        "гарнитуры различаются - растеризатор нашёл обе, а не одну запасную"
    );
}

/// Кадры прогона: цветной вид корня и строки трассы разной длины - третья
/// переносится на две строки, и кадр выше соседей.
fn frames() -> Vec<String> {
    let lines = [
        "1: Line [t=0]".to_string(),
        "2: Line [t=1]".to_string(),
        format!("3: Line [{}]", "t=2, ".repeat(60)),
    ];
    lines
        .iter()
        .map(|line| {
            sheet(
                "/",
                &Options {
                    view: View::Run,
                    legend: false,
                    tick: Some(Tick {
                        active: vec![Active {
                            path: vec![],
                            model: None,
                            state: "Line".into(),
                            done: false,
                        }],
                        next: vec![],
                    }),
                    trace: Some(line.clone()),
                    fonts: false,
                },
            )
        })
        .collect()
}

fn film(frames: &[String]) -> Film<impl Fn(usize) -> Result<String, String> + '_> {
    Film {
        count: frames.len(),
        draw: |i| Ok(frames[i].clone()),
    }
}

#[test]
fn a_gif_has_a_frame_per_tick_one_delay_and_the_largest_side() {
    let frames = frames();
    let sizes: Vec<(u32, u32)> = frames
        .iter()
        .map(|f| Tree::parse(f).expect("svg").size())
        .collect();
    assert!(
        sizes[2].1 > sizes[0].1,
        "длинная строка переносится: {sizes:?}"
    );
    let mut bytes = Vec::new();
    gif(&film(&frames), 300, &mut bytes).expect("GIF");

    let mut options = gif::DecodeOptions::new();
    options.set_color_output(gif::ColorOutput::RGBA);
    let mut decoder = options
        .read_info(std::io::Cursor::new(&bytes))
        .expect("GIF читается");
    assert_eq!(
        (u32::from(decoder.width()), u32::from(decoder.height())),
        sizes[2],
        "сторона ленты - наибольшая"
    );
    let mut count = 0;
    while let Some(frame) = decoder.read_next_frame().expect("кадр") {
        assert_eq!(frame.delay, 30, "пауза 300 мс - 30 сотых");
        count += 1;
    }
    assert_eq!(count, 3, "кадр на такт");

    let mut again = Vec::new();
    gif(&film(&frames), 300, &mut again).expect("GIF");
    assert_eq!(bytes, again, "детерминизм");
    assert!(
        gif(&film(&[]), 300, Vec::new()).is_err(),
        "без кадров - отказ"
    );
}

/// Коробка MP4: тип и тело.
fn boxes(mut data: &[u8]) -> Vec<([u8; 4], &[u8])> {
    let mut out = Vec::new();
    while data.len() >= 8 {
        let len = u32::from_be_bytes(data[..4].try_into().unwrap()) as usize;
        assert!(len >= 8 && len <= data.len(), "длина коробки");
        out.push((data[4..8].try_into().unwrap(), &data[8..len]));
        data = &data[len..];
    }
    assert!(data.is_empty(), "хвост после коробок");
    out
}

/// Тело коробки по пути вложенности.
fn find<'a>(data: &'a [u8], path: &[&[u8; 4]]) -> &'a [u8] {
    let (head, rest) = path.split_first().expect("путь");
    let body = boxes(data)
        .into_iter()
        .find(|(kind, _)| kind == *head)
        .map(|(_, body)| body)
        .unwrap_or_else(|| panic!("нет коробки {}", String::from_utf8_lossy(*head)));
    if rest.is_empty() {
        body
    } else {
        find(body, rest)
    }
}

fn be32(data: &[u8], at: usize) -> u32 {
    u32::from_be_bytes(data[at..at + 4].try_into().unwrap())
}

#[test]
fn an_mp4_holds_a_sample_per_tick_after_its_index() {
    let frames = frames();
    let bytes = mp4(&film(&frames), 250).expect("MP4");
    let kinds: Vec<[u8; 4]> = boxes(&bytes).into_iter().map(|(k, _)| k).collect();
    assert_eq!(
        kinds,
        [*b"ftyp", *b"moov", *b"mdat"],
        "оглавление перед данными"
    );

    let stbl: &[&[u8; 4]] = &[b"moov", b"trak", b"mdia", b"minf", b"stbl"];
    let at = |last: &[u8; 4]| find(&bytes, &[stbl, &[last]].concat());
    let stsz = at(b"stsz");
    let count = be32(stsz, 8) as usize;
    assert_eq!(count, 3, "выборка на такт");
    let sizes: Vec<usize> = (0..count)
        .map(|i| be32(stsz, 12 + 4 * i) as usize)
        .collect();
    let stts = at(b"stts");
    assert_eq!(
        (be32(stts, 8), be32(stts, 12)),
        (3, 250),
        "длительность кадра - пауза"
    );
    let stss = at(b"stss");
    assert_eq!(
        (be32(stss, 4), be32(stss, 8)),
        (1, 1),
        "первый кадр ключевой"
    );

    let stsd = at(b"stsd");
    let entry = &stsd[8..];
    assert_eq!(&entry[4..8], b"av01", "дорожка AV1");
    let (w, h) = (
        u16::from_be_bytes([entry[32], entry[33]]),
        u16::from_be_bytes([entry[34], entry[35]]),
    );
    assert!(w % 2 == 0 && h % 2 == 0, "стороны чётные: {w} × {h}");
    let av1c = find(&entry[8 + 78..], &[b"av1C"]);
    assert_eq!(av1c[0], 0x81, "маркер и версия записи av1C");
    assert_eq!(
        (av1c[4] >> 3) & 0x0F,
        1,
        "в av1C - заголовок последовательности"
    );

    // Кусок начинается там, куда указывает `stco`, и выборки занимают его целиком.
    let offset = be32(at(b"stco"), 8) as usize;
    let mdat_at = bytes.len() - find(&bytes, &[b"mdat"]).len();
    assert_eq!(offset, mdat_at, "смещение куска - начало данных");
    assert_eq!(
        sizes.iter().sum::<usize>(),
        bytes.len() - offset,
        "данные - выборки"
    );
    assert_eq!(
        (bytes[offset] >> 3) & 0x0F,
        1,
        "первая выборка - с заголовком"
    );

    assert_eq!(bytes, mp4(&film(&frames), 250).expect("MP4"), "детерминизм");
}
