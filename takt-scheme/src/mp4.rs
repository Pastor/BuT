//! Минимальный контейнер MP4 для одной дорожки AV1.
//!
//! Раскладка - `ftyp`, `moov`, `mdat`: оглавление стоит перед данными, и плеер
//! начинает показ, не дочитав файл. Одна дорожка, один кусок данных, выборки идут
//! подряд. Готовые муксеры не годятся: разбор заголовка последовательности, который
//! пишет `rav1e`, у них съезжает на бит (флаг модели декодера читается без флага
//! времени), и первый же кадр отвергается.
//!
//! Поля коробок - по ISO/IEC 14496-12 (контейнер) и спецификации AV1 в ISOBMFF
//! (`av01`, `av1C`).

/// Выборка дорожки: OBU кадра без разделителя времени и признак ключевого кадра.
pub struct Sample {
    pub data: Vec<u8>,
    pub key: bool,
}

/// Коробка: длина, тип, тело.
fn plain(kind: &[u8; 4], body: &[u8]) -> Vec<u8> {
    let mut out = ((body.len() + 8) as u32).to_be_bytes().to_vec();
    out.extend_from_slice(kind);
    out.extend_from_slice(body);
    out
}

/// Полная коробка: версия и флаги перед телом.
fn full(kind: &[u8; 4], version: u8, flags: u32, body: &[u8]) -> Vec<u8> {
    let mut out = vec![version];
    out.extend_from_slice(&flags.to_be_bytes()[1..]);
    out.extend_from_slice(body);
    plain(kind, &out)
}

/// Единичная матрица преобразования кадра (числа 16.16 и 2.30).
fn matrix() -> Vec<u8> {
    [0x0001_0000u32, 0, 0, 0, 0x0001_0000, 0, 0, 0, 0x4000_0000]
        .iter()
        .flat_map(|m| m.to_be_bytes())
        .collect()
}

/// Единиц времени в секунде: длительность выборки задаётся в миллисекундах.
const TIMESCALE: u32 = 1000;

/// MP4 из выборок AV1; `av1c` - тело записи `av1C` целиком (четыре байта
/// конфигурации и OBU заголовка последовательности), `delta_ms` - длительность
/// каждой выборки.
pub fn write(width: u32, height: u32, delta_ms: u32, av1c: &[u8], samples: &[Sample]) -> Vec<u8> {
    let count = samples.len() as u32;
    let duration = delta_ms.saturating_mul(count);
    let ftyp = plain(
        b"ftyp",
        &[
            b"isom".as_slice(),
            &0x200u32.to_be_bytes(),
            b"isom",
            b"iso6",
            b"av01",
            b"mp41",
        ]
        .concat(),
    );

    let mut mvhd = vec![0u8; 8]; // время создания и изменения
    mvhd.extend_from_slice(&TIMESCALE.to_be_bytes());
    mvhd.extend_from_slice(&duration.to_be_bytes());
    mvhd.extend_from_slice(&0x0001_0000u32.to_be_bytes()); // скорость 1.0
    mvhd.extend_from_slice(&0x0100u16.to_be_bytes()); // громкость 1.0
    mvhd.extend_from_slice(&[0; 10]);
    mvhd.extend_from_slice(&matrix());
    mvhd.extend_from_slice(&[0; 24]);
    mvhd.extend_from_slice(&2u32.to_be_bytes()); // номер следующей дорожки
    let mvhd = full(b"mvhd", 0, 0, &mvhd);

    let mut tkhd = vec![0u8; 8];
    tkhd.extend_from_slice(&1u32.to_be_bytes()); // номер дорожки
    tkhd.extend_from_slice(&[0; 4]);
    tkhd.extend_from_slice(&duration.to_be_bytes());
    tkhd.extend_from_slice(&[0; 16]);
    tkhd.extend_from_slice(&matrix());
    tkhd.extend_from_slice(&(width << 16).to_be_bytes());
    tkhd.extend_from_slice(&(height << 16).to_be_bytes());
    // Флаги: дорожка включена и участвует в показе.
    let tkhd = full(b"tkhd", 0, 3, &tkhd);

    let mut mdhd = vec![0u8; 8];
    mdhd.extend_from_slice(&TIMESCALE.to_be_bytes());
    mdhd.extend_from_slice(&duration.to_be_bytes());
    mdhd.extend_from_slice(&0x55C4u16.to_be_bytes()); // язык "und"
    mdhd.extend_from_slice(&[0; 2]);
    let mdhd = full(b"mdhd", 0, 0, &mdhd);
    let hdlr = full(
        b"hdlr",
        0,
        0,
        &[&[0u8; 4][..], b"vide", &[0; 12], b"VideoHandler\0"].concat(),
    );
    let vmhd = full(b"vmhd", 0, 1, &[0; 8]);
    let dref = full(
        b"dref",
        0,
        0,
        &[&1u32.to_be_bytes()[..], &full(b"url ", 0, 1, &[])].concat(),
    );
    let dinf = plain(b"dinf", &dref);

    let mut entry = vec![0u8; 6];
    entry.extend_from_slice(&1u16.to_be_bytes()); // индекс ссылки данных
    entry.extend_from_slice(&[0; 16]);
    entry.extend_from_slice(&(width as u16).to_be_bytes());
    entry.extend_from_slice(&(height as u16).to_be_bytes());
    entry.extend_from_slice(&0x0048_0000u32.to_be_bytes()); // 72 точки на дюйм
    entry.extend_from_slice(&0x0048_0000u32.to_be_bytes());
    entry.extend_from_slice(&[0; 4]);
    entry.extend_from_slice(&1u16.to_be_bytes()); // кадров в выборке
    entry.extend_from_slice(&[0; 32]); // имя кодировщика
    entry.extend_from_slice(&0x0018u16.to_be_bytes()); // глубина цвета
    entry.extend_from_slice(&0xFFFFu16.to_be_bytes());
    entry.extend_from_slice(&plain(b"av1C", av1c));
    let stsd = full(
        b"stsd",
        0,
        0,
        &[&1u32.to_be_bytes()[..], &plain(b"av01", &entry)].concat(),
    );
    let stts = full(
        b"stts",
        0,
        0,
        &[
            1u32.to_be_bytes(),
            count.to_be_bytes(),
            delta_ms.to_be_bytes(),
        ]
        .concat(),
    );
    let keys: Vec<u32> = samples
        .iter()
        .enumerate()
        .filter(|(_, s)| s.key)
        .map(|(i, _)| i as u32 + 1)
        .collect();
    let mut stss = (keys.len() as u32).to_be_bytes().to_vec();
    for key in &keys {
        stss.extend_from_slice(&key.to_be_bytes());
    }
    let stss = full(b"stss", 0, 0, &stss);
    // Один кусок со всеми выборками.
    let stsc = full(
        b"stsc",
        0,
        0,
        &[
            1u32.to_be_bytes(),
            1u32.to_be_bytes(),
            count.to_be_bytes(),
            1u32.to_be_bytes(),
        ]
        .concat(),
    );
    let mut stsz = vec![0u8; 4]; // размер не общий: у каждой выборки свой
    stsz.extend_from_slice(&count.to_be_bytes());
    for sample in samples {
        stsz.extend_from_slice(&(sample.data.len() as u32).to_be_bytes());
    }
    let stsz = full(b"stsz", 0, 0, &stsz);

    // Смещение куска зависит от длины `moov`, а та от смещения - нет: у `stco`
    // длина постоянна. Поэтому оглавление строится дважды.
    let moov_at = |offset: u32| {
        let stco = full(
            b"stco",
            0,
            0,
            &[1u32.to_be_bytes(), offset.to_be_bytes()].concat(),
        );
        let stbl = plain(
            b"stbl",
            &[&stsd[..], &stts, &stss, &stsc, &stsz, &stco].concat(),
        );
        let minf = plain(b"minf", &[&vmhd[..], &dinf, &stbl].concat());
        let mdia = plain(b"mdia", &[&mdhd[..], &hdlr, &minf].concat());
        let trak = plain(b"trak", &[&tkhd[..], &mdia].concat());
        plain(b"moov", &[&mvhd[..], &trak].concat())
    };
    let offset = (ftyp.len() + moov_at(0).len() + 8) as u32;
    let moov = moov_at(offset);
    let payload: Vec<u8> = samples
        .iter()
        .flat_map(|s| s.data.iter().copied())
        .collect();
    [ftyp, moov, plain(b"mdat", &payload)].concat()
}

/// Длина первого OBU целиком: заголовок, поле длины и тело.
pub fn obu_len(data: &[u8]) -> Option<usize> {
    let header = *data.first()?;
    let extension = header & 0x04 != 0;
    let mut at = 1 + usize::from(extension);
    let (mut size, mut shift) = (0usize, 0u32);
    loop {
        let byte = *data.get(at)?;
        size |= usize::from(byte & 0x7F).checked_shl(shift)?;
        at += 1;
        shift += 7;
        if byte & 0x80 == 0 {
            break;
        }
    }
    Some(at + size)
}

/// Тип OBU по байту заголовка.
pub fn obu_type(data: &[u8]) -> Option<u8> {
    data.first().map(|h| (h >> 3) & 0x0F)
}

/// Тип OBU разделителя времени.
pub const OBU_TEMPORAL_DELIMITER: u8 = 2;
/// Тип OBU заголовка последовательности.
pub const OBU_SEQUENCE_HEADER: u8 = 1;

/// Пакет без ведущего разделителя времени: в выборку MP4 он не кладётся.
pub fn strip_delimiter(data: &[u8]) -> &[u8] {
    if obu_type(data) == Some(OBU_TEMPORAL_DELIMITER)
        && let Some(len) = obu_len(data)
    {
        &data[len.min(data.len())..]
    } else {
        data
    }
}
