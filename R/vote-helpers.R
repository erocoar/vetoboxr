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
            if (!is.null(drift)) {
              if (is.vector(drift)) {
                stopifnot(length(drift) == voters@voter_count * voters@dimension)
                drift <- matrix(rep(drift, iter),
                                ncol = voters@voter_count * voters@dimension,
                                nrow = iter, byrow = TRUE)
              } else if (is.matrix(drift)) {
                stopifnot(ncol(drift) == voters@voter_count * voters@dimension)
                stopifnot(nrow(drift) == iter)
              }
            } else {
              drift <- matrix(0, ncol = voters@voter_count * voters@dimension,
                              nrow = iter)
            }

            if (!is.null(vibration)) {
              if (is.matrix(vibration)) {
                stopifnot(ncol(vibration) == voters@voter_count * voters@dimension)
                stopifnot(nrow(vibration) == iter)
              } else if (is.function(vibration)) {
                vibration <- matrix(vibration(prod(voters@voter_count * voters@dimension * iter), ...),
                       ncol = voters@voter_count * voters@dimension, nrow = iter, byrow = TRUE)
              }
            } else {
              vibration <- matrix(0, ncol = voters@voter_count * voters@dimension,
                                  nrow = iter)
            }

            position <- matrix(rep(voters@position, iter),
                               ncol = voters@voter_count * voters@dimension,
                               nrow = iter, byrow = TRUE)
            voter_names <- names(voters@position)
            voters@position <- position + drift + vibration
            colnames(voters@position) <- voter_names
            rownames(voters@position) <- seq(iter)
            voters
          })

create_voter_array(voters, c(1,2,3,4,5,6), rnorm, 5)
