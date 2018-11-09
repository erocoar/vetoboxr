#' Retrieve winset spatial polygons from `Vote` objects.
#'
#' @param vote An object of class `Vote`.
#' @param iter A vector of iterations for which to retrieve the winsets.
#' @param dimension Defaults to `2`.
#' @param quadsegs Number of linear segments used to approximate voter indifference circles.
#'
#' @importFrom sp SpatialPoints
#' @importFrom rgeos gBuffer gIntersection
#' @export
get_winset <- function(vote, iter = 1, dimension = 2, quadsegs = 50) {
  voter_roles <- vote$voter_roles[iter, ]
  # initialize sq and voter spatial points
  status_quo <- sp::SpatialPoints(vote$status_quo[iter, , drop = FALSE])

  voters <- sp::SpatialPoints(
    matrix(as.vector(t(vote$voter_position[iter, , drop = FALSE])), ncol = dimension, byrow = TRUE)
  )

  # create winset for every iter
  winsets <- vector("list", length(iter))
  voter_length <- length(voters) / length(iter)
  idx <- seq(1, length(iter) * voter_length, voter_length)

  lapply(seq_along(iter), function(i) {
    voter_list <- lapply(seq(idx[i], idx[i] + voter_length - 1), function(j) {
      rgeos::gBuffer(voters[j, ], width = norm(voters[j, ]@coords - status_quo[i, ]@coords, "2"),
                     quadsegs = quadsegs)
    })

    as_idx <- which(vote$voter_roles[i, ] == "AS")
    intersection <- voter_list[[as_idx]]

    coalition <- setdiff(vote$coalitions[[i]], as_idx)
    # for (v in seq(length(voter_list))[-as_idx]) {
    for (v in coalition) {
      intersection <- rgeos::gIntersection(intersection, voter_list[[v]])
    }

    intersection
  })
}
