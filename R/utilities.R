create_index <- function(idx, dimension) {
  sort(c(idx, idx + seq(dimension - 1)))
}