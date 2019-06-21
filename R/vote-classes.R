#' Voter Classes
#'
#' Voter classes are used to represent voters having various roles (Normal, Voter, Agenda Setter).
#'
#' @name voter_class
#' @param position A position vector or matrix, either indicating the voter's position once (notwithstanding drift and vibration) or for all voting iterations.
#' @param role (optional) The role of the voter, one of "Normal", "Veto", "AS" and "Random". Can only be used with the \code{Voter()} constructor function.
NULL

#' @slot position A position vector.
#' @slot dimension An integer specifying the dimension, defaults to `length(position)`
#' @name Position
Position <- setClass("Position",
                     slots = c(
                       position = "vector",
                       dimension = "numeric"
                     )
)


#' Assigns the `position` vector and `dimension`, with default \code{dimension} of \code{length(position)}.
#' @param .Object the Position object.
#' @param position The position vector.
#' @rdname Position
setMethod("initialize", "Position", function(.Object, position) {
  dimensions <- dim(position)
  dimensions <- if (is.null(dimensions)) length(position) else dimensions[2]
  .Object@position <- position
  .Object@dimension <- dimensions
  .Object
}
)

# setMethod("show", "Position", function(object) {
#   cat(is(object)[[1]], "\n",
#       sapply(slotNames(object), function(x) {
#         paste("  ", x, slot(object, x), "\n")
#       }),
#       sep = ""
#   )
# })

#' Check whether an object is of class `Position`.
#' @param x An object.
#' @rdname Position
is.position <- function(x) {
  inherits(x, "Position")
}

setClassUnion("matrixORvector", c("matrix", "vector"))

#' @slot position A position vector or matrix.
#' @slot dimension An integer specifying the dimension, defaults to \code{length(position)} if \code{position} is of type \code{vector} or \code{ncol(position)} if \code{position} is of type \code{matrix}.
#' @slot role The role of the voter, one of "AS", "Veto", "Normal" and "Random".
#' @name voter_class
.Voter <- setClass("Voter",
                   slots = c(
                     role = "matrixORvector"
                   ),
                   prototype = prototype(
                     role = NA_character_
                   ),
                   contains = "Position"
)

#' Constructor for Voter class.
#'
#' @param .Object The Voter object.
#' @param position The position vector.
#' @param role Role character or vector.
#'
#' @importFrom methods callNextMethod
#' @rdname Voter
setMethod("initialize", "Voter", function(.Object, position, role) {
  .Object <- callNextMethod(.Object, position)
  .Object@role <- role
  .Object
})

#' @importFrom methods new
#' @rdname voter_class
#' @export
Voter <- function(position, role = "Normal") {
  methods::new("Voter", position = position, role = role)
}

#' @importFrom methods new
#' @rdname voter_class
#' @export
Veto <- function(position) {
  methods::new("Voter", position = position, role = "Veto")
}

#' @importFrom methods new
#' @rdname voter_class
#' @export
AS <- function(position) {
  methods::new("Voter", position = position, role = "AS")
}

#' @importFrom methods new
#' @rdname voter_class
#' @export
RandomVoter <- function(position) {
  methods::new("Voter", position = position, role = "Random")
}

#' S4 class representing the Status Quo.
#'
#' @slot position A position vector of length \code{dimension}.
#' @slot dimension The dimension of the status quo.
#' @slot drift A vector indicating the drift (constant change per iteration).
#'
#' @name SQ-class
#' @aliases SQ
#' @rdname SQ-class
.SQ <- setClass(
  "SQ",
  slots = c(
    # drift = "vector"
    drift = "function"
  ),
  prototype = prototype(
    # drift = NA_real_
    drift = function() 0
  ),
  contains = "Position"
)

#' Constructor for SQ class.
setMethod("initialize", "SQ", function(.Object, position, drift) {
  .Object <- callNextMethod(.Object, position)
  .Object@drift <- if (is.null(drift)) rep(0, .Object@dimension) else drift
  .Object
})

#' @param position A position vector.
#' @importFrom methods new
#'
#' @rdname SQ-class
#' @export
SQ <- function(position, drift = NULL) {
  stopifnot(is.null(dim(position)) && is.null(dim(drift)))
  # stopifnot(length(position) == length(drift) || is.null(drift))
  methods::new("SQ", position = position, drift = drift)
  # .SQ(position = position)
}
