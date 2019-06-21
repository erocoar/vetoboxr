#' Animate selected iterations of a \code{Vote} object.
#'
#' \code{animate.Vote()} plots selected iterations of a \code{Vote} object and combines them into one GIF using the \code{animation} package.
#'
#' @name animate
#'
#' @param vote An object of class \code{Vote}.
#' @param movie.name Name of the resulting GIF file.
#' @param iter A vector of the iterations to plot, defaults to \code{seq(1, vote$iter)}.
#' @param ... Additional keyword arguments.
NULL

#' @include voters-class.R
NULL

#' @rdname animate
#' @export
animate <- function (vote, movie.name, iter = NULL, ...) {
  UseMethod("animate", voters)
}

#' @rdname animate
#' @importFrom animation saveGIF
#' @export
animate.Vote <- function(vote, movie.name, iter = NULL, ...) {
  xrange <- get_range_x(vote)
  yrange <- get_range_y(vote)

  if (is.null(iter)) {
    iter <- seq(vote$iter)
  }

  saveGIF({
    for (i in iter) {
      print(plot.Vote(vote, i, xrange, yrange))
    }
  },
  movie.name = movie.name,
  ani.width = 800, ani.height = 600)
}
