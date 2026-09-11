//! Видео прогона: GIF и MP4 (AV1) из кадров-чертежей.
//!
//! Кадр - SVG, который рисует вызывающий (цветной вид листа и строка трассы
//! такта); здесь он растеризуется ([`crate::raster`]) и кодируется. Запись
//! потоковая: кадр рисуется, растеризуется и кодируется, и его пиксели больше не
//! нужны - в памяти не копятся ни растры, ни тексты кадров. Сторона у всех кадров
//! одна - наибольшая по ленте: строка трассы переносится по-разному от такта к
//! такту, и кадр ниже соседа дополняется полем листа, а не растягивается.
//!
//! Фон у видео всегда залит: прозрачности нет ни у AV1, ни у честного GIF (у того
//! она однобитна и дала бы рваные края сглаживания).

use std::io::Write;

use rav1e::prelude::*;

use crate::mp4::{self, OBU_SEQUENCE_HEADER, Sample};
use crate::raster::{Background, Tree};

/// Пауза между кадрами по умолчанию, миллисекунд.
pub const PAUSE_MS: u32 = 500;

/// Лента кадров: число кадров и способ нарисовать кадр по номеру.
///
/// Кадр рисуется по требованию, а не хранится: у прогона в тысячи тактов тексты
/// всех кадров заняли бы память, которой у модуля под браузер нет.
pub struct Film<F: Fn(usize) -> Result<String, String>> {
    pub count: usize,
    pub draw: F,
}

impl<F: Fn(usize) -> Result<String, String>> Film<F> {
    /// Сторона ленты: наибольшая ширина и высота кадров.
    ///
    /// Сторону называет корневой элемент чертежа (`width`, `height`), и первый проход
    /// читает только его - растеризатор разбирает кадр один раз, во втором проходе.
    fn canvas(&self) -> Result<(u32, u32), String> {
        if self.count == 0 {
            return Err("кадров нет: прогон не дал ни одного такта".to_string());
        }
        let (mut width, mut height) = (1u32, 1u32);
        for i in 0..self.count {
            let svg = (self.draw)(i)?;
            let (w, h) = match root_size(&svg) {
                Some(size) => size,
                None => Tree::parse(&svg)?.size(),
            };
            width = width.max(w);
            height = height.max(h);
        }
        Ok((width, height))
    }

    /// Кадр `i` растром RGBA на холсте ленты.
    fn frame(&self, i: usize, width: u32, height: u32) -> Result<Vec<u8>, String> {
        let svg = (self.draw)(i)?;
        Ok(Tree::parse(&svg)?
            .render(width, height, Background::Fill)?
            .take())
    }
}

/// Сторона чертежа по атрибутам `width` и `height` корневого элемента, округлённая
/// вверх - так же, как её считает растеризатор.
fn root_size(svg: &str) -> Option<(u32, u32)> {
    let start = svg.find("<svg")?;
    let end = start + svg[start..].find('>')?;
    let root = &svg[start..end];
    let attr = |name: &str| -> Option<u32> {
        let key = format!(" {name}=\"");
        let from = root.find(&key)? + key.len();
        let value: f64 = root[from..].split('"').next()?.parse().ok()?;
        (value > 0.0).then(|| value.ceil() as u32)
    };
    Some((attr("width")?, attr("height")?))
}

/// Задержка кадра GIF в сотых секунды: пауза округляется до десяти миллисекунд, но
/// не до нуля - нулевую задержку плееры заменяют своей.
fn gif_delay(pause_ms: u32) -> u16 {
    u16::try_from(pause_ms.saturating_add(5) / 10)
        .unwrap_or(u16::MAX)
        .max(1)
}

/// GIF ленты: кадр на номер, задержка одна, повтор бесконечный.
///
/// # Ошибки
/// Кадров нет, кадр не рисуется либо не разбирается, сторона больше 65535.
pub fn gif<F, W>(film: &Film<F>, pause_ms: u32, out: W) -> Result<(), String>
where
    F: Fn(usize) -> Result<String, String>,
    W: Write,
{
    let (width, height) = film.canvas()?;
    let side = |v: u32| u16::try_from(v).map_err(|_| format!("кадр шире предела GIF: {v}"));
    let (w, h) = (side(width)?, side(height)?);
    let mut encoder =
        gif::Encoder::new(out, w, h, &[]).map_err(|e| format!("GIF не пишется: {e}"))?;
    encoder
        .set_repeat(gif::Repeat::Infinite)
        .map_err(|e| format!("GIF не пишется: {e}"))?;
    let delay = gif_delay(pause_ms);
    for i in 0..film.count {
        let mut rgba = film.frame(i, width, height)?;
        // Скорость квантования 10 - в разы быстрее наилучшей при незаметной
        // разнице: у схемы мало цветов.
        let mut frame = gif::Frame::from_rgba_speed(w, h, &mut rgba, 10);
        frame.delay = delay;
        encoder
            .write_frame(&frame)
            .map_err(|e| format!("кадр GIF не пишется: {e}"))?;
    }
    // Хвост файла и сброс буфера - с ответом: при сбросе в деструкторе ошибка
    // записи потерялась бы, и неполный файл сошёл бы за готовый.
    encoder
        .into_inner()
        .map_err(|e| format!("GIF не пишется: {e}"))?
        .flush()
        .map_err(|e| format!("GIF не пишется: {e}"))
}

/// Настройка кодировщика: скорость 10 (кадры схемы почти неподвижны, и файл остаётся
/// малым), постоянный квантователь, ключевой кадр не реже раза в 60 кадров, один
/// поток - вывод одинаков у командной строки и модуля.
fn encoder(width: u32, height: u32, pause_ms: u32) -> Result<Context<u8>, String> {
    let config = EncoderConfig {
        width: width as usize,
        height: height as usize,
        time_base: Rational::new(u64::from(pause_ms), 1000),
        speed_settings: SpeedSettings::from_preset(10),
        low_latency: true,
        min_key_frame_interval: 0,
        max_key_frame_interval: 60,
        quantizer: 100,
        still_picture: false,
        ..EncoderConfig::default()
    };
    Config::new()
        .with_encoder_config(config)
        .with_threads(1)
        .new_context()
        .map_err(|e| format!("кодировщик AV1 не настраивается: {e}"))
}

/// RGBA -> YUV 4:2:0 (BT.601, узкий диапазон); цветность - по левому верхнему
/// пикселю квадрата 2 на 2. Стороны чётные.
fn to_yuv(rgba: &[u8], width: usize, height: usize, frame: &mut Frame<u8>) {
    let (cw, ch) = (width / 2, height / 2);
    let mut y = vec![0u8; width * height];
    let mut u = vec![0u8; cw * ch];
    let mut v = vec![0u8; cw * ch];
    for row in 0..height {
        for col in 0..width {
            let at = (row * width + col) * 4;
            let (r, g, b) = (
                f32::from(rgba[at]),
                f32::from(rgba[at + 1]),
                f32::from(rgba[at + 2]),
            );
            y[row * width + col] = (16.0 + 0.257 * r + 0.504 * g + 0.098 * b).round() as u8;
            if row % 2 == 0 && col % 2 == 0 {
                let c = (row / 2) * cw + col / 2;
                u[c] = (128.0 - 0.148 * r - 0.291 * g + 0.439 * b).round() as u8;
                v[c] = (128.0 + 0.439 * r - 0.368 * g - 0.071 * b).round() as u8;
            }
        }
    }
    frame.planes[0].copy_from_raw_u8(&y, width, 1);
    frame.planes[1].copy_from_raw_u8(&u, cw, 1);
    frame.planes[2].copy_from_raw_u8(&v, cw, 1);
}

/// Забирает готовые пакеты кодировщика.
fn drain(ctx: &mut Context<u8>, packets: &mut Vec<Sample>) -> Result<(), String> {
    loop {
        match ctx.receive_packet() {
            Ok(packet) => packets.push(Sample {
                key: packet.frame_type == FrameType::KEY,
                data: packet.data,
            }),
            Err(EncoderStatus::Encoded) => continue,
            Err(EncoderStatus::NeedMoreData | EncoderStatus::LimitReached) => return Ok(()),
            Err(e) => return Err(format!("кадр AV1 не кодируется: {e}")),
        }
    }
}

/// MP4 ленты: AV1, кадр на номер, длительность кадра - пауза.
///
/// Стороны кадра чётные (цветность 4:2:0 делит их пополам): нечётная сторона
/// дополняется полем листа.
///
/// # Ошибки
/// Кадров нет, кадр не рисуется либо не разбирается, кодировщик отказал.
pub fn mp4<F>(film: &Film<F>, pause_ms: u32) -> Result<Vec<u8>, String>
where
    F: Fn(usize) -> Result<String, String>,
{
    let pause_ms = pause_ms.max(1);
    let (width, height) = film.canvas()?;
    let (width, height) = (width + width % 2, height + height % 2);
    let mut ctx = encoder(width, height, pause_ms)?;
    let mut packets = Vec::with_capacity(film.count);
    for i in 0..film.count {
        let rgba = film.frame(i, width, height)?;
        let mut frame = ctx.new_frame();
        to_yuv(&rgba, width as usize, height as usize, &mut frame);
        ctx.send_frame(frame)
            .map_err(|e| format!("кадр AV1 не принят: {e}"))?;
        drain(&mut ctx, &mut packets)?;
    }
    ctx.flush();
    drain(&mut ctx, &mut packets)?;

    // Разделитель времени в выборку не кладётся; первый OBU первого кадра -
    // заголовок последовательности, и он же едет в `av1C`.
    let samples: Vec<Sample> = packets
        .into_iter()
        .map(|p| Sample {
            data: mp4::strip_delimiter(&p.data).to_vec(),
            key: p.key,
        })
        .collect();
    let first = samples
        .first()
        .ok_or_else(|| "кодировщик не отдал ни одного кадра".to_string())?;
    let header_len = mp4::obu_len(&first.data)
        .filter(|_| mp4::obu_type(&first.data) == Some(OBU_SEQUENCE_HEADER))
        .ok_or_else(|| "первый кадр AV1 без заголовка последовательности".to_string())?;
    let mut av1c = ctx.container_sequence_header();
    av1c.extend_from_slice(&first.data[..header_len.min(first.data.len())]);
    Ok(mp4::write(width, height, pause_ms, &av1c, &samples))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_root_size_is_read_from_the_attributes() {
        let svg = r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="8 8 100.5 60" width="100.5" height="60"><rect/></svg>"#;
        assert_eq!(root_size(svg), Some((101, 60)));
        assert_eq!(root_size("<svg/>"), None);
    }

    #[test]
    fn the_pause_rounds_to_hundredths_but_not_to_zero() {
        assert_eq!(gif_delay(500), 50);
        assert_eq!(gif_delay(4), 1);
        assert_eq!(gif_delay(15), 2);
        assert_eq!(gif_delay(u32::MAX), u16::MAX);
    }
}
