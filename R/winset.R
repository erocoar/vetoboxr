#' Retrieve winset spatial polygons from `Vote` objects.
#'
#' \code{get_winset()} converts the positions of voters at the selected iterations into spatial polygons with the given voter radii, and calculates the intersection of all coalition members' indifference circles, which is called the winset.
#'
#' @param vote An object of class `Vote`.
#' @param iter A vector of iterations for which to retrieve the winsets.
#' @param dimension Defaults to `2`.
#' @param quadsegs Number of linear segments used to approximate voter indifference circles.
#'
#' @importFrom sp SpatialPoints
#' @importFrom rgeos gBuffer gIntersection
#' @export
get_winset <- function(vote, iter = 1, dimension = 2, quadsegs = 100) {
  # TODO consider only coalitions from the start? compare efficiency.

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
    if (is.null(vote$coalitions[[iter[i]]])) {
      NULL
    } else if isTRUE(all.equal(unname(vote$status_quo[iter[i], ]), unname(vote$outcome[iter[i], ]))) {
      NULL
    } else {
      voter_list <- lapply(seq(idx[i], idx[i] + voter_length - 1), function(j) {
        rgeos::gBuffer(voters[j, ], width = norm(voters[j, ]@coords - status_quo[i, ]@coords, "2"),
                       quadsegs = quadsegs)
      })

      coalition <- vote$coalitions[[iter[i]]]

      if (all(apply(combn(coalition, 2), 2, function(x) {
        rgeos::gIntersects(voter_list[[x[1]]], voter_list[[x[2]]])
      }))) {

        as_idx <- which(vote$voter_roles[iter[i], ] == "AS")
        intersection <- voter_list[[as_idx]]
        coalition <- setdiff(coalition, as_idx)

        for (v in coalition) {
          intersection <- rgeos::gIntersection(intersection, voter_list[[v]])
        }

        intersection
      } else {
        NULL
      }
    }
  })
}
