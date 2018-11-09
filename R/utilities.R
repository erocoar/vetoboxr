create_index <- function(idx, dimension) {
  if (dimension > 1) sort(c(idx, idx + seq(dimension - 1))) else idx
}

get_range_x <- function(vote) {
  x_idx <- seq(1, ncol(vote$voter_position), vote$dimension)
  xmin <- min(
    vote$voter_position[, x_idx] - vote$voter_radii,
    vote$voter_position[, x_idx] + vote$voter_radii,
    vote$status_quo[, 1],
    vote$outcome[, 1]
  ) * 1
  xmax <- max(
    vote$voter_position[, x_idx] - vote$voter_radii,
    vote$voter_position[, x_idx] + vote$voter_radii,
    vote$status_quo[, 1],
    vote$outcome[, 1]
  ) * 1
  c(xmin, xmax)
}

get_range_y <- function(vote) {
  if (vote$dimension == 2) {
    y_idx <- seq(1, ncol(vote$voter_position), vote$dimension) + 1
    ymin <- min(
      vote$voter_position[, y_idx] - vote$voter_radii,
      vote$voter_position[, y_idx] + vote$voter_radii,
      vote$status_quo[, 2],
      vote$outcome[, 2]
    ) * 1
    ymax <- max(
      vote$voter_position[, y_idx] - vote$voter_radii,
      vote$voter_position[, y_idx] + vote$voter_radii,
      vote$status_quo[, 2],
      vote$outcome[, 2]
    ) * 1
  } else {
    ymin <- 0
    ymax <- max(
      vote$voter_position + vote$voter_radii / 2
    ) * 1
  }
  c(ymin, ymax)
}
