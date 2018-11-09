#' @param vote
#' @param iter
#'
#' @importFrom ggpol geom_circle
#' @importFrom ggplot2 ggplot aes geom_point geom_polygon coord_fixed scale_color_manual scale_colour_manual xlab ylab ggtitle
#' @export
plot.Vote <- function(vote, iter = 1, xlim = NULL, ylim = NULL, ...) {
  stopifnot(length(iter) == 1)

  if (vote$dimension == 2) {
    x_idx <- seq(1, ncol(vote$voter_position), vote$dimension)
    y_idx <- x_idx + 1
    plt <- ggplot() +
      geom_polygon(aes(x = vote$winsets[[iter]]@polygons[[1]]@Polygons[[1]]@coords[, 1],
                       y = vote$winsets[[iter]]@polygons[[1]]@Polygons[[1]]@coords[, 2]),
                   alpha = 0.4, color = "red") +
      geom_circle(aes(x = vote$voter_position[iter, x_idx],
                      y = vote$voter_position[iter, y_idx],
                      r = vote$voter_radii[iter, ])) +
      geom_point(aes(x = c(vote$voter_position[iter, x_idx], vote$status_quo[iter, 1], vote$outcome[iter, 1]),
                     y = c(vote$voter_position[iter, y_idx], vote$status_quo[iter, 2], vote$outcome[iter, 2]),
                     color = c(vote$voter_roles[iter, ], "SQ", "Outcome")), size = 2) +
      scale_color_manual(
        name = "Vote",
        values = c("Veto" = "red", "Normal" = "Grey", "AS" = "blue", "SQ" = "orange", "Outcome" = "darkgreen")) +
      coord_fixed() +
      xlab("x") + ylab("y")
  } else if (vote$dimension == 1) {
    plt <- ggplot() +
      geom_half_circle(aes(x = vote$voter_position[iter, ],
                           y = 0,
                           r = vote$voter_radii[iter, ])) +
      geom_point(aes(x = c(vote$voter_position[iter, ], vote$status_quo[iter, ], vote$outcome[iter, ]),
                     y = 0, color = c(vote$voter_roles[iter, ], "SQ", "Outcome")),
                 size = 3.5) +
      scale_color_manual(
        name = "Vote",
        values = c("Veto" = "red", "Normal" = "Grey", "AS" = "blue", "SQ" = "orange", "Outcome" = "darkgreen")) +
      coord_fixed() +
      xlab("x") + ylab("y")
  }

  if (!is.null(xlim)) {
    plt <- plt + xlim(xlim)
  }
  if (!is.null(ylim)) {
    plt <- plt + ylim(ylim)
  }
  plt + labs(subtitle = paste0("Iter: ", iter))
}
