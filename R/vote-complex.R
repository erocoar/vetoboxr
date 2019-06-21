#' Aggregate Voters.
#'
#' The \code{+} function is used to combine multiple voters into the \code{Voters} class.
#'
#' @name complex_voters
#'
#' @param e1,e2 Objects of class \code{Voter} or \code{Voters}.
NULL

#' @include voters-class.R
NULL

#' @rdname complex_voters
#' @aliases +
#' @export
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

#' @rdname complex_voters
#' @aliases +
#' @export
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

#' @rdname complex_voters
#' @aliases +
#' @export
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

#' @rdname complex_voters
#' @aliases +
#' @export
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


