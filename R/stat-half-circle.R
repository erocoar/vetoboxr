#' @rdname geom_half_circle
#' @importFrom ggplot2 layer
#' @export
#' @rdname geom_half_circle
stat_half_circle <- function(mapping = NULL, data = NULL, geom = "half_circle",
                        position = "identity", n = 180, na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatHalfCircle,
    mapping = mapping,
    data = data,
    geom = geom,
    position = position,
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    params = list(n = n, na.rm = na.rm, ...)
  )
}

#' @rdname geom_half_circle
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @export
StatHalfCircle <- ggproto(
  "StatHalfCircle", Stat,
  required_aes = c("x", "y", "r"),
  compute_panel = function(data, scales, n = 180) {
    angle <- seq(0, pi, length.out = n)
    df <- do.call(rbind,
                  lapply(seq_v(data), function(i) {
                    data.frame(x = data$x[i] + data$r[i] * cos(angle),
                               y = data$y[i] + data$r[i] * sin(angle),
                               group = i)
                    })
                  )
    add_cols <- setdiff(colnames(data), c(colnames(df), "r"))
    for (col_ in add_cols) {
      df[, col_] <- rep(data[, col_], each = n)
      }
    df
    }
  )

