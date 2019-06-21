#' @include voters-class.R
NULL

#' Voters Array Generators
#'
#' Vote helpers are functions that take as input an object of class \code{Voters} and additional information regarding drift, vibration and the number of iterations to vote on to
#' - reshape the position matrix and role vector to the correct size, resembling the number of iterations
#' - add the specified drift to the voter positions and (calculate and) add the vibration
#' - for voters with random positions, determine the actual positions, per run
#'
#' @name vote_helpers
#'
#' @param voters A \code{Voters} object.
#' @param drift A vector or matrix of drift values. If vector, then the drift is constant per iteration and the vector must be of length \code{dimension * voter_count}. If matrix, the drift can vary per dimension and must be of shape \code{iterations} x \code{dimension * voter_count}.
#' @param vibration A function with first parameter \code{n} that generates random noise.
#' @param iter The number of iterations.
#' @param no_random_veto Indicator for whether voters with random position can take on the Veto role.
#' @param no_random_normal Indicator for whether random voters can take on the Normal role.
#' @param ... Additional keyword arguments.
NULL

#' @rdname vote_helpers
#' @export
setGeneric("create_voter_array", function(voters, drift, vibration, iter, ...) {
  standardGeneric("create_voter_array")
})

#' @rdname vote_helpers
#' @export
setMethod(
  "create_voter_array",
  signature = signature(
    voters = "Voters"
    ),
  function(voters, drift, vibration, iter, ...) {
    n <- voters@voter_count * voters@dimension
    if (!is.null(drift)) {
      if (is.vector(drift)) {
        stopifnot(length(drift) == n)
        drift <- matrix(rep(drift, iter),
                        ncol = n,
                        nrow = iter,
                        byrow = TRUE)
        drift <- apply(drift, 2, cumsum)
        } else if (is.matrix(drift)) {
          stopifnot(ncol(drift) == n)
          stopifnot(nrow(drift) == iter)
        }
      } else {
        drift <- matrix(0, ncol = n, nrow = iter)
      }
    if (!is.null(vibration)) {
      if (is.matrix(vibration)) {
        stopifnot(ncol(vibration) == n)
        stopifnot(nrow(vibration) == iter)
        } else if (is.function(vibration)) {
          vibration <- matrix(vibration(prod(n * iter), ...),
                              ncol = n, nrow = iter, byrow = TRUE)
        }
      } else {
        vibration <- matrix(0, ncol = n, nrow = iter)
      }

    position <- matrix(rep(voters@position, iter),
                       ncol = n, nrow = iter, byrow = TRUE)
    voter_names <- names(voters@position)
    voters@position <- position + drift + vibration
    colnames(voters@position) <- voter_names
    rownames(voters@position) <- seq(iter)
    voters
    })

#' @rdname vote_helpers
#' @export
setGeneric("create_role_array", function(voters, iter, ...) {
  standardGeneric("create_role_array")
})

#' @rdname vote_helpers
#' @export
setMethod(
  "create_role_array",
  signature = signature(voters = "Voters"),
  function(voters, iter, no_random_veto, no_random_normal, ...) {
    roles <- voters@role

    # if (nrow(roles) == iter && sum(roles == "Random") == 0) {
    #   return(roles)
    # }

    n <- voters@voter_count * iter

    # 1. if there are no random roles and all iters have roles, return
    if (!any(roles == "Random")) {
      if (nrow(roles) == iter) {
        return(roles)
      }
    }

    # 2. if not all iters have roles, reshape
    if (nrow(roles) != iter) {
      roles <- matrix(roles, nrow = iter, ncol = voters@voter_count, byrow = TRUE)
    }

    # 3. per row, check for random AS or other random roles.
    roles <- apply(roles, 1, function(x) {
      random_idx <- which(x == "Random")
      nonrandom_idx <- setdiff(seq_along(x), random_idx)

      if (!"AS" %in% x) {
        as_idx <- sample(random_idx, 1)
        x[as_idx] <- "AS"
        random_idx <- setdiff(random_idx, as_idx)
      }
      if (isTRUE(no_random_veto)) {
        x[random_idx] <- "Normal"
        } else if (isTRUE(no_random_normal)) {
          x[random_idx] <- "Veto"
          } else {
            x[random_idx] <- sample(c("Veto", "Normal"),
                                    length(random_idx), replace = TRUE, ...)
            }
      x
      })
    t(roles)
    }
  )

# create coal array after the fact
create_coalition_array <- function(vote) {
  coalitions <- matrix(FALSE, nrow = vote$iter, ncol = vote$voter_count)
  for (iter in seq(vote$iter)) {
    coalitions[iter, vote$coalitions[[iter]]] <- TRUE
  }
  coalitions
}
