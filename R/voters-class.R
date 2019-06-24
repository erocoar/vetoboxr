#' Voters Class.
#'
#' The Voters class is used to represent multiple voters at the same time.
#'
#' @name voters_class
#'
#' @param position A position vector or matrix, either indicating the voter's position once (notwithstanding drift and vibration) or for all voting iterations.
#' @param role (optional) The role of the voter, one of "Normal", "Veto", "AS" and "Random". Can only be used with the \code{Voter()} constructor function.
NULL

#' @include vote-classes.R
NULL

# TODO: Lazyeval to get voter names here for `voters`

#' @slot position A position matrix with \code{dimension * voter_count} columns. There is either one row for fixed positions (notwithstanding drift and vibration), or one row per vote iteration.
#' @slot dimension An integer specifying the dimension of each voter's position, defaults to \code{ncol(position) / voter_count}.
#' @slot role The role of the voter, one of "AS", "Veto", "Normal" or "Random".
#' @slot voter_count The number of voters.
#' @rdname voters_class
.Voters <- setClass(
  "Voters",
  contains = "Voter",
  slots = c(voter_count = "numeric"),
  prototype = prototype(voter_count = NA_integer_)
)

#' @param .Object The Voters object.
#' @param dimension Dimensionality of the voters' positions.
#' @param voter_count The number of voters.
#' @rdname voters_class
setMethod(
  "initialize", "Voters",
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

#' @rdname voters_class
#' @export
Voters <- function(position, dimension, role) {
  role <- rbind(role)
  if (length(position) %% dimension != 0 && ncol(position) %% dimension != 0) {#!is.vector(position)) {
    stop("Voter positions do not match dimension.")
  } else if (ncol(role) != length(position) / dimension) {
    stop("Voter roles do not match voter count.")
  } else if (!all(unlist(role) %in% c("AS", "Veto", "Normal"))) {
    stop("Voter roles must be one of AS, Veto and Normal.")
  }
  methods::new("Voters", position = position, dimension = dimension,
               role = role, voter_count = ncol(role))
}

#' @param voter Voter object.
#' @rdname voters_class
#' @export
as.voters <- function (voter) {
  UseMethod("as.voters", voter)
}

#' @rdname voters_class
#' @export
as.voters.Voter <- function(voter) {
  .Voters(voter@position, voter@dimension, voter@role, 1)
}

