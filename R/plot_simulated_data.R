plot_simulated_data <- function(sim_data, dimVals = seq(-.5, +.5, .05)){

  layers <- list(
      ggplot2::geom_line(size = 1.5),
      ggplot2::geom_point(fill = "white", size = 2),
      ggplot2::geom_vline(xintercept = 0, linetype = "dotted", colour = "grey"),
      ggplot2::scale_x_continuous(limits = c(min(dimVals), max(dimVals)), breaks = dimVals, labels = c(min(dimVals), rep("", (length(dimVals) - 3)/2), "CS+", rep("", (length(dimVals) - 3)/2), max(dimVals))),
      ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)),
      ggplot2::scale_colour_manual(values = c("#301A4B", "#6DB1BF", "#D4775E")),
      ggplot2::labs(x = "stimulus", y = "mean response"),
      ggplot2::theme_classic()
  )

  gradients <- sim_data |>
    dplyr::arrange(.data$subj, .data$group, .data$x) |>
    dplyr::group_by(.data$group, .data$x) |>
    dplyr::summarise(y = mean(.data$y))

  fig <- ggplot2::ggplot(gradients, ggplot2::aes(x = .data$x, y = .data$y, group = .data$group, colour = .data$group,
                                                 shape = .data$group)) + layers

  return(fig)
}


