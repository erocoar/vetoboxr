#' @include voters.R
NULL

#' @export
setGeneric("create_voter_array", function(voters, drift, vibration, iter, ...) {
  standardGeneric("create_voter_array")
})

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

#' @export
setGeneric("create_role_array", function(voters, iter, ...) {
  standardGeneric("create_role_array")
})

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

