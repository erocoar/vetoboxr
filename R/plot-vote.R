#' Visualize votes.
#'
#' Plot one iteration of a \code{Vote} object, detailing voter positions, status quo, voting outcome, voter indifference circles and the winset.
#'
#' @name plot_vote
#'
#' @param x An object of class \code{Vote}.
#' @param iter The iteration for which the plot should be generated.
#' @param xlim Limits of the x-axis, defaults to \code{NULL}.
#' @param ylim Limits of the y-axis, defaults to \code{NULL}.
#' @param trace Integer indicating how many iterations back the status quo should be traced via a line segment, defaults to \code{NULL}.
#' @param ... Additional keyword arguments.
NULL

#' @include voting.R
NULL

#' @importFrom graphics plot
NULL

#' @rdname plot_vote
#' @importFrom ggpol geom_circle
#' @importFrom ggplot2 ggplot aes geom_point geom_polygon coord_fixed scale_color_manual scale_colour_manual xlab ylab ggtitle scale_shape_manual scale_alpha_manual xlim ylim labs geom_segment
#' @export
plot.Vote <- function(x, iter = 1, xlim = NULL, ylim = NULL, trace = NULL, ...) {
  stopifnot(length(iter) == 1)

  if (x$dimension == 2) {
    x_idx <- seq(1, ncol(x$voter_position), x$dimension)
    y_idx <- x_idx + 1
    plt <- ggplot()
    if (!is.null(x$winsets[[iter]])) {
      plt <- plt +
        geom_polygon(
          aes(x = x$winsets[[iter]]@polygons[[1]]@Polygons[[1]]@coords[, 1],
              y = x$winsets[[iter]]@polygons[[1]]@Polygons[[1]]@coords[, 2]),
          alpha = 0.4)
    }
    plt <- plt +
      geom_circle(
        aes(x = x$voter_position[iter, x_idx],
            y = x$voter_position[iter, y_idx],
            r = x$voter_radii[iter, ])) +
      geom_point(
        aes(x = c(x$voter_position[iter, x_idx], x$status_quo[iter, 1], x$outcome[iter, 1]),
            y = c(x$voter_position[iter, y_idx], x$status_quo[iter, 2], x$outcome[iter, 2]),
            fill = c(x$voter_roles[iter, ], "SQ", "Outcome"),
            shape = c(x$voter_roles[iter, ], "SQ", "Outcome")),
        size = 2, color = "black") +
      scale_fill_manual(
        name = "Vote",
        values = c("Veto" = "red", "Normal" = "Grey", "AS" = "blue",
                   "SQ" = "orange", "Outcome" = "darkgreen")) +
      scale_shape_manual(
        name = "Vote",
        values = c("Veto" = 21, "Normal" = 21, "AS" = 21,
                   "SQ" = 24, "Outcome" = 24)) +
      coord_fixed() +
      xlab("x") +
      ylab("y")

    if (!is.null(trace) && iter > 1) {
      trace <- min(trace, iter - 1)
      trace_seq <- seq(iter, iter - trace, -1)
      cat(trace, trace_seq)

      plt <- plt + geom_segment(
        aes(
          x = x$status_quo[trace_seq, 1],
          y = x$status_quo[trace_seq, 2],
          xend = x$outcome[trace_seq, 1],
          yend = x$outcome[trace_seq, 2],
          alpha = gl(trace + 1, 1)),
        show.legend = FALSE,
        size = 1.5,
        color = "green"
      ) +
        scale_alpha_manual(values = seq(1, 0.2, length.out = trace + 1))
    }
  } else if (x$dimension == 1) {
    plt <- ggplot() +
      geom_half_circle(
        aes(x = x$voter_position[iter, ],
            y = 0,
            r = x$voter_radii[iter, ])) +
      geom_point(
        aes(x = c(x$voter_position[iter, ], x$status_quo[iter, ], x$outcome[iter, ]),
            y = 0,
            fill = c(x$voter_roles[iter, ], "SQ", "Outcome"),
            shape = c(x$voter_roles[iter, ], "SQ", "Outcome")),
        size = 3.5, color = "black") +
      scale_fill_manual(
        name = "Vote",
        values = c("Veto" = "red", "Normal" = "Grey", "AS" = "blue",
                   "SQ" = "orange", "Outcome" = "darkgreen")) +
      scale_shape_manual(
        name = "Vote",
        values = c("Veto" = 21, "Normal" = 21, "AS" = 21,
                   "SQ" = 24, "Outcome" = 24)) +
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

#' @rdname plot_vote
#' @importFrom ggplot2 geom_tile scale_fill_manual ylab xlab coord_fixed
#' @export
plot_roles <- function(x, iter = NULL) {
  if (is.null(iter)) {
    iter <- seq(1, x$iter)
  }
  roles <- x$voter_roles[iter, ]
  vnames <- paste("Voter", seq(x$voter_count))
  roles <- as.vector(t(roles))

  ggplot() +
    geom_tile(aes(y = rep(vnames, length(iter)),
                  x = factor(rep(seq(length(iter)), each = x$voter_count)),
                  fill = roles), color = "black") +
    scale_fill_manual(
      name = "Vote",
      values = c("AS" = "blue", "Veto" = "red", "Normal" = "grey")) +
    ylab("Voters") +
    xlab("Iter") +
    coord_fixed()
}

#' @rdname plot_vote
#' @importFrom ggplot2 geom_tile scale_fill_manual ylab xlab coord_fixed
#' @export
plot_coalition <- function(x, iter = NULL) {
  if (is.null(iter)) {
    iter <- seq(1, x$iter)
  }
  roles <- x$voter_roles[iter, , drop = FALSE]
  vnames <- paste("Voter", seq(x$voter_count))
  coalition_idx <- !sapply(x$coalitions, is.null)

  for (i in iter[coalition_idx]) {
    idx <- which(roles[i, ] == "Normal")
    idx <- idx[idx %in% x$coalitions[[i]]]
    roles[i, idx] <- "Coalition"
  }

  roles <- as.vector(t(roles))
  ggplot() +
    geom_tile(aes(y = rep(vnames, length(iter)),
                  x = factor(rep(seq(length(iter)), each = x$voter_count)),
                  fill = roles), color = "black") +
    scale_fill_manual(
      name = "Vote",
      values = c("AS" = "blue", "Veto" = "red",
                 "Coalition" = "darkgreen", "Normal" = "darkgrey")) +
    xlab("Iter") +
    ylab("Voters") +
    coord_fixed()
}
