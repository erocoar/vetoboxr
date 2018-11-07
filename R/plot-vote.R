#' @importFrom ggpol geom_circle
#' @importFrom ggplot2 ggplot aes geom_point geom_polygon coord_fixed
plot.Vote <- function(Vote, iter = 1, ...) {
  x_idx <- seq(1, ncol(Vote$voter_position), Vote$dimension)
  y_idx <- x_idx + 1
  ggplot() +
    geom_polygon(aes(x = Vote$winsets[[iter]]@polygons[[1]]@Polygons[[1]]@coords[, 1],
                     y = Vote$winsets[[iter]]@polygons[[1]]@Polygons[[1]]@coords[, 2]),
                 alpha = 0.4, color = "red") +
    geom_circle(aes(x = Vote$voter_position[iter, x_idx],
                    y = Vote$voter_position[iter, y_idx],
                    r = Vote$voter_radii[iter, ])) +
    geom_point(aes(x = c(Vote$voter_position[iter, x_idx], Vote$status_quo[iter, 1], Vote$outcome[iter, 1]),
                   y = c(Vote$voter_position[iter, y_idx], Vote$status_quo[iter, 2], Vote$outcome[iter, 2]),
                   color = c(Vote$voter_roles[iter, ], "SQ", "Outcome")), size = 2) +
    scale_color_manual(
      name = "Vote",
      values = c("Veto" = "red", "Normal" = "Grey", "AS" = "blue", "SQ" = "orange", "Outcome" = "darkgreen")) +
    coord_fixed() +
    xlab("x") + ylab("y")
}
