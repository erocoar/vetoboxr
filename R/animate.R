animate <- function (voters, movie.name, iter = NULL, ...) {
  UseMethod("animate", voters)
}

#' Animate a series of votes from a `Vote` object.
#'
#' `animate()` calls `animation::saveGIF()` to create a GIF file from plots of all `iter` created with \link{plot.Vote}.
#'
#' @param vote An object of class `Vote`.
#' @param movie.name Name of the GIF file.
#' @param iter A vector of the iterations to plot, defaults to `seq(1, vote$call$iter)`.
#' @param ...
#' @export
animate.Vote <- function(vote, movie.name, iter = NULL, ...) {
  xrange <- get_range_x(vote)
  yrange <- get_range_y(vote)

  if (is.null(iter)) {
    iter <- seq(vote$call$iter)
  }

  saveGIF({
    for (i in iter) {
      print(plot(vote, i, xrange, yrange))
    }
  },
  movie.name = movie.name,
  ani.width = 800, ani.height = 600)
}
