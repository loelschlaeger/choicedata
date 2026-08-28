# Recreate the package logo

output_file <- file.path("man", "figures", "logo.png")
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

choice_rows <- data.frame(
  alternative = c("A", "B", "C"),
  y = c(3, 2, 1),
  chosen = c(FALSE, TRUE, FALSE)
)

covariate_cells <- data.frame(
  x = rep(c(2.25, 2.78, 3.31), each = 3),
  y = rep(c(3, 2, 1), times = 3),
  value = c(
    0.25, 0.70, 0.45,
    0.80, 0.35, 0.60,
    0.45, 0.90, 0.25
  )
)

choice_icon <- ggplot2::ggplot() +
  ggplot2::geom_segment(
    data = choice_rows,
    ggplot2::aes(x = 0.20, xend = 3.65, y = y, yend = y),
    linewidth = 9,
    lineend = "round",
    colour = "#FFFFFF",
    alpha = 0.14
  ) +
  ggplot2::geom_tile(
    data = covariate_cells,
    ggplot2::aes(x = x, y = y, alpha = value),
    width = 0.43,
    height = 0.43,
    fill = "#A9E5DE"
  ) +
  ggplot2::scale_alpha_continuous(range = c(0.38, 1), guide = "none") +
  ggplot2::geom_point(
    data = subset(choice_rows, !chosen),
    ggplot2::aes(x = 0.38, y = y),
    shape = 21,
    size = 5.2,
    stroke = 1.3,
    colour = "#F7FAFC",
    fill = "#0B2545"
  ) +
  ggplot2::geom_point(
    data = subset(choice_rows, chosen),
    ggplot2::aes(x = 0.38, y = y),
    shape = 21,
    size = 5.2,
    stroke = 1.3,
    colour = "#FFCB47",
    fill = "#FFCB47"
  ) +
  ggplot2::geom_point(
    data = subset(choice_rows, chosen),
    ggplot2::aes(x = 0.38, y = y),
    size = 1.6,
    colour = "#0B2545"
  ) +
  ggplot2::geom_text(
    data = choice_rows,
    ggplot2::aes(x = 1.40, y = y, label = alternative),
    family = "sans",
    fontface = "bold",
    size = 5.2,
    colour = "#F7FAFC"
  ) +
  ggplot2::coord_cartesian(xlim = c(0, 3.9), ylim = c(0.55, 3.45), clip = "off") +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    plot.margin = ggplot2::margin(0, 0, 0, 0)
  )

set.seed(1)

hexSticker::sticker(
  subplot = choice_icon,
  package = "choicedata",
  s_x = 1,
  s_y = 0.78,
  s_width = 1.02,
  s_height = 0.70,
  p_x = 1,
  p_y = 1.40,
  p_color = "#F7FAFC",
  p_family = "sans",
  p_fontface = "bold",
  p_size = 7.4,
  h_size = 1.2,
  h_fill = "#0B2545",
  h_color = "#2EC4B6",
  url = "",
  white_around_sticker = FALSE,
  filename = output_file,
  dpi = 139
)
