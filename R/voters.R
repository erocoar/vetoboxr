#' @include vote-classes.R
NULL

#' An S4 class representing a group of voters.
#'
#' @slot voter_count Number of voters.
#' @export
.Voters <- setClass("Voters",
                    contains = "Voter",
                    slots = c(
                      voter_count = "numeric"
                    ),
                    prototype = prototype(
                      voter_count = NA_integer_
                    )
)


setMethod("initialize", "Voters",
          function(.Object, position, dimension, role, voter_count) {
            .Object@position <- position
            .Object@dimension <- dimension
            .Object@role <- role
            .Object@voter_count <- voter_count

            names(.Object@position) <- paste0(
              rep(paste0("Voter ", seq(voter_count)), each = dimension),
              " Dim ", seq(dimension)
            )
            .Object
          }
)

setMethod("+", signature = signature(e1 = "Voter", e2 = "Voter"),
          function(e1, e2) {
            check_positions(e1, e2)
            check_roles(e1, e2)

            .Voters(
              position = c(e1@position, e2@position),
              dimension = e1@dimension,
              role = cbind(e1@role, e2@role),
              voter_count = length(e1@role) + length(e2@role)
            )
          })

setMethod("+", signature = signature(e1 = "Voters", e2 = "Voter"),
          function(e1, e2) {
            check_positions(e1, e2)
            check_roles(e1, e2)

            .Voters(
              position = c(e1@position, e2@position),
              dimension = e1@dimension,
              role = cbind(e1@role, e2@role),
              voter_count = e1@voter_count + length(e2@role)
            )
          })

setMethod("+", signature = signature(e1 = "Voter", e2 = "Voters"),
          function(e1, e2) {
            check_positions(e1, e2)
            check_roles(e1, e2)

            .Voters(
              position = c(e1@position, e2@position),
              dimension = e1@dimension,
              role = cbind(e1@role, e2@role),
              voter_count = length(e1@role) + e2@voter_count
            )
          })

setMethod("+", signature = signature(e1 = "Voters", e2 = "Voters"),
          function(e1, e2) {
            check_positions(e1, e2)
            check_roles(e1, e2)

            .Voters(
              position = c(e1@position, e2@position),
              dimension = e1@dimension,
              role = cbind(e1@role, e2@role),
              voter_count = sum(e1@voter_count, e2@voter_count)
            )
          }
)

#' @export
as.voters <- function (voter) {
  UseMethod("as.voters", voter)
}

#' @export
as.voters.Voter <- function(voter) {
  .Voters(voter@position, voter@dimension, voter@role, 1)
}
