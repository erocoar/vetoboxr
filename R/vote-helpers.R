create_index <- function(idx, dimension) {
  sort(c(idx, idx + seq(dimension - 1)))
}

setGeneric("create_voter_array", function(voters, drift, vibration, iter, ...) {
  standardGeneric("create_voter_array")
})

setMethod("create_voter_array",
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
                                nrow = iter, byrow = TRUE)
              } else if (is.matrix(drift)) {
                stopifnot(ncol(drift) == n)
                stopifnot(nrow(drift) == iter)
              }
            } else {
              drift <- matrix(0, ncol = n,
                              nrow = iter)
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
