animate <- function (voters, movie.name, iter = NULL, ...) {
  UseMethod("animate", voters)
}

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
  ani.width = 600, ani.height = 600)
}
