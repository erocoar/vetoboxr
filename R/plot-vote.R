#' Create a `ggplot2` plot detailing voter positions, status quo, voting outcome, voter indifference circles and winset.
#'
#' @param vote An object of class `Vote`.
#' @param iter An iteration for which the plot should be generated.
#' @param xlim Limits of the x-axis. Defaults to `NULL`.
#' @param ylim Limits of the y-axis. Defaults to `NULL`.
#' @param trace Integer indicating how many iterations back the status quo should be traced via a line segment. Defaults to `NULL`.
#'
#' @importFrom ggpol geom_circle geom_half_circle
#' @importFrom ggplot2 ggplot aes geom_point geom_polygon coord_fixed scale_color_manual scale_colour_manual xlab ylab ggtitle scale_shape_manual scale_alpha_manual xlim ylim labs
#' @export
plot.Vote <- function(vote, iter = 1, xlim = NULL, ylim = NULL, trace = NULL, ...) {
  stopifnot(length(iter) == 1)

  if (vote$dimension == 2) {
    x_idx <- seq(1, ncol(vote$voter_position), vote$dimension)
    y_idx <- x_idx + 1
    plt <- ggplot() +
      geom_polygon(
        aes(x = vote$winsets[[iter]]@polygons[[1]]@Polygons[[1]]@coords[, 1],
            y = vote$winsets[[iter]]@polygons[[1]]@Polygons[[1]]@coords[, 2]),
        alpha = 0.4, color = "red") +
      geom_circle(
        aes(x = vote$voter_position[iter, x_idx],
            y = vote$voter_position[iter, y_idx],
            r = vote$voter_radii[iter, ])) +
      geom_point(
        aes(x = c(vote$voter_position[iter, x_idx], vote$status_quo[iter, 1], vote$outcome[iter, 1]),
            y = c(vote$voter_position[iter, y_idx], vote$status_quo[iter, 2], vote$outcome[iter, 2]),
            color = c(vote$voter_roles[iter, ], "SQ", "Outcome"),
            shape = c(vote$voter_roles[iter, ], "SQ", "Outcome")),
        size = 2) +
      scale_color_manual(
        name = "Vote",
        values = c("Veto" = "red", "Normal" = "Grey", "AS" = "blue",
                   "SQ" = "orange", "Outcome" = "darkgreen")) +
      scale_shape_manual(
        name = "Vote",
        values = c("Veto" = 19, "Normal" = 19, "AS" = 19,
                   "SQ" = 17, "Outcome" = 17)) +
      coord_fixed() +
      xlab("x") +
      ylab("y")

    if (!is.null(trace) && iter > 1) {
      trace <- min(trace, iter - 1)
      trace_seq <- seq(iter, iter - trace, -1)
      cat(trace, trace_seq)

      plt <- plt + geom_segment(
        aes(
          x = vote$status_quo[trace_seq, 1],
          y = vote$status_quo[trace_seq, 2],
          xend = vote$outcome[trace_seq, 1],
          yend = vote$outcome[trace_seq, 2],
          alpha = gl(trace + 1, 1)),
        show.legend = FALSE,
        size = 1.5,
        color = "green"
      ) +
        scale_alpha_manual(values = seq(1, 0.2, length.out = trace + 1))
    }
  } else if (vote$dimension == 1) {
    plt <- ggplot() +
      geom_half_circle(
        aes(x = vote$voter_position[iter, ],
            y = 0,
            r = vote$voter_radii[iter, ])) +
      geom_point(
        aes(x = c(vote$voter_position[iter, ], vote$status_quo[iter, ], vote$outcome[iter, ]),
            y = 0,
            color = c(vote$voter_roles[iter, ], "SQ", "Outcome"),
            shape = c(vote$voter_roles[iter, ], "SQ", "Outcome")),
        size = 3.5) +
      scale_color_manual(
        name = "Vote",
        values = c("Veto" = "red", "Normal" = "Grey", "AS" = "blue",
                   "SQ" = "orange", "Outcome" = "darkgreen")) +
      scale_shape_manual(
        name = "Vote",
        values = c("Veto" = 19, "Normal" = 19, "AS" = 19,
                   "SQ" = 17, "Outcome" = 17)) +
      coord_fixed() +
      xlab("x") +
      ylab("y")
  }

  if (!is.null(xlim)) {
    plt <- plt + xlim(xlim)
  }
  if (!is.null(ylim)) {
    plt <- plt + ylim(ylim)
  }
  plt + labs(subtitle = paste0("Iter: ", iter))
}
