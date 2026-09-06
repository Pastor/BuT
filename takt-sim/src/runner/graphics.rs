//! Запись прогона в кадры: GIF и SVG.

use super::SimulationRunner;
use crate::context::Context;
use crate::gif::GifRecorder;
use crate::graphics_config::{GraphicsConfig, OutputMode};
use crate::port_names::PortNames;
use crate::svg::SvgRecorder;
use crate::trace::format_value;
use crate::unit::Unit;
use crate::unit::viewport::{LegendData, compute_layout, render_from_layout};

/// Рекордер кадров и размер кадра - то, чем прогон обзаводится при запросе графики.
///
/// Именованный тип, а не кортеж в сигнатуре: `clippy::type_complexity` прав -
/// `Result<(Option<_>, Option<(u32, u32)>), String>` не читается.
pub(super) type Frames = (Option<GraphicsRecorder>, Option<(u32, u32)>);

/// Заводит рекордер кадров и размер кадра, если прогон просит графику.
///
/// Каталог создаётся здесь же: без него первая запись упала бы в середине прогона,
/// когда часть работы уже сделана.
pub(super) fn recorder_of(
    output_dir: Option<&std::path::PathBuf>,
    input_stem: &str,
    output_mode: OutputMode,
    gif_config: &GraphicsConfig,
) -> Result<Frames, String> {
    let Some(dir) = output_dir else {
        return Ok((None, None));
    };
    std::fs::create_dir_all(dir)
        .map_err(|e| format!("Не удалось создать директорию {}: {e}", dir.display()))?;
    let size = (
        (gif_config.canvas.width + gif_config.legend.width) as u32,
        gif_config.canvas.height as u32,
    );
    let recorder = match output_mode {
        OutputMode::Gif => {
            let gif_path = dir.join(format!("{input_stem}.gif"));
            GraphicsRecorder::Gif(GifRecorder::new(
                &gif_path,
                gif_config.canvas.frame_delay_cs,
            ))
        }
        OutputMode::Svg => GraphicsRecorder::Svg(SvgRecorder::new(
            dir.clone(),
            input_stem.to_string(),
            gif_config,
        )),
    };
    Ok((Some(recorder), Some(size)))
}

// -- Диспетчер режимов записи графики -----------------------------------------

pub(crate) enum GraphicsRecorder {
    Gif(GifRecorder),
    Svg(SvgRecorder),
}

impl GraphicsRecorder {
    pub(super) fn add_frame(
        &mut self,
        viewport: &crate::unit::viewport::Viewport,
        w: u32,
        h: u32,
        delay: Option<u16>,
    ) -> Result<crate::gif::FrameTiming, String> {
        match self {
            Self::Gif(r) => r.add_frame(viewport, w, h, delay),
            Self::Svg(r) => r.add_frame(viewport, w, h, delay),
        }
    }

    pub(super) fn save(self) -> Result<(), String> {
        match self {
            Self::Gif(r) => r.save(),
            Self::Svg(r) => r.save(),
        }
    }

    pub(super) fn is_svg(&self) -> bool {
        matches!(self, Self::Svg(_))
    }
}

impl SimulationRunner {
    pub(super) fn capture_frame(&mut self) -> Result<(), String> {
        self.capture_frame_impl(None)
    }

    pub(super) fn capture_frame_with_highlight(
        &mut self,
        edge: Option<(&str, &str)>,
    ) -> Result<(), String> {
        // Гарантируем, что раскладка вычислена до поиска индексов
        if self.cached_layout.is_none() && self.gif_frame_size.is_some() {
            self.cached_layout = Some(compute_layout(&self.unit, &self.gif_config));
        }
        let highlighted = edge.and_then(|(from, to)| {
            let layout = self.cached_layout.as_ref()?;
            let fi = layout.node_labels.iter().position(|n| n == from)?;
            let ti = layout.node_labels.iter().position(|n| n == to)?;
            Some((fi, ti))
        });
        self.capture_frame_impl(highlighted)
    }

    fn capture_frame_impl(
        &mut self,
        highlighted_edge: Option<(usize, usize)>,
    ) -> Result<(), String> {
        let (w, h) = match self.gif_frame_size {
            Some(s) => s,
            None => return Ok(()),
        };

        // Раскладка вычисляется один раз: имитация отжига дорогая, но структура модели
        // не меняется в ходе симуляции.
        let layout_ms = if self.cached_layout.is_none() {
            let t = std::time::Instant::now();
            self.cached_layout = Some(compute_layout(&self.unit, &self.gif_config));
            Some(t.elapsed().as_millis())
        } else {
            None
        };

        let active = self.unit.active_states();
        let active_refs: Vec<&str> = active.iter().map(String::as_str).collect();
        let legend = build_legend(&self.unit, &self.port_names);

        let t_vp = std::time::Instant::now();
        let is_svg = self
            .graphics_recorder
            .as_ref()
            .map(|r| r.is_svg())
            .unwrap_or(false);
        let viewport = render_from_layout(
            self.cached_layout.as_ref().unwrap(),
            &self.gif_config,
            &active_refs,
            Some(&legend),
            self.model_name.as_deref(),
            highlighted_edge,
            is_svg,
        )
        .map_err(|d| format!("Ошибка viewport: {}", d.message))?;
        let vp_ms = t_vp.elapsed().as_millis();

        // Highlight-кадры показываются дольше - задержка из конфигурации.
        let delay = if highlighted_edge.is_some() {
            Some(self.gif_config.canvas.highlight_frame_delay_cs)
        } else {
            None
        };
        let frame_timing = if let Some(rec) = &mut self.graphics_recorder {
            Some((rec.is_svg(), rec.add_frame(&viewport, w, h, delay)?))
        } else {
            None
        };

        // Вывод тайминга
        if let Some((is_svg, ft)) = frame_timing {
            if is_svg {
                if let Some(lms) = layout_ms {
                    println!(
                        "           SVG:  раскладка={lms} мс  viewport={vp_ms} мс  запись={} мс",
                        ft.serial_ms
                    );
                } else {
                    println!(
                        "           SVG:  viewport={vp_ms} мс  запись={} мс",
                        ft.serial_ms
                    );
                }
            } else {
                let rast_detail = format!(
                    "svg={} мс  usvg={} мс  render={} мс  quant={} мс",
                    ft.serial_ms, ft.parse_ms, ft.render_ms, ft.quant_ms
                );
                if let Some(lms) = layout_ms {
                    println!(
                        "           GIF:  раскладка={lms} мс  viewport={vp_ms} мс  {rast_detail}"
                    );
                } else {
                    println!("           GIF:  viewport={vp_ms} мс  {rast_detail}");
                }
            }
        }

        Ok(())
    }
}

fn build_legend(unit: &Unit, port_names: &PortNames) -> LegendData {
    let to_entries = |names: &[String]| -> Vec<(String, String)> {
        names
            .iter()
            .map(|n| {
                let v = unit
                    .get_value(n)
                    .map(|v| format_value(&v))
                    .unwrap_or_else(|| "?".to_string());
                (n.clone(), v)
            })
            .collect()
    };
    LegendData {
        in_ports: to_entries(&port_names.in_ports),
        out_ports: to_entries(&port_names.out_ports),
        inout_ports: to_entries(&port_names.inout_ports),
        vars: to_entries(&port_names.vars),
    }
}
