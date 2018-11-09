#' An S4 class representing a position, used to build Voter and Status Quo classes.
#'
#' @slot position A position vector.
#' @slot dimension An integer specifying the dimension, defaults to `length(position)`
Position <- setClass("Position",
                     slots = c(
                       position = "vector",
                       dimension = "numeric"
                     )
)


#' Init function of the Position class.
#' Assigns the `position` vector and `dimension`, with default `dimension` being `length(position)`.
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

#' An S4 class representing a voter.
#'
#' @slot position A position vector.
#' @slot dimension An integer specifying the dimension, defaults to `length(position)`
#' @slot role The role of the voter, one of `"AS"`, `"Veto"`, `"Normal"` or `"Random"`.
#' @name Voter
.Voter <- setClass("Voter",
                   slots = c(
                     role = "matrixORvector"
                   ),
                   prototype = prototype(
                     role = NA_character_
                   ),
                   contains = "Position"
)

#' Constructor for \link{Voter-class}.
#'
#' @rdname Voter
setMethod("initialize", "Voter", function(.Object, position, role) {
  .Object <- callNextMethod(.Object, position)
  .Object@role <- role
  .Object
})

#' Constructor for Voter, by default with role `"Normal"`.
#'
#' @param position A position vector.
#' @param role Defaults to `"Normal"`.
#' @rdname Voter
#' @export
Voter <- function(position, role = "Normal") {
  methods::new("Voter", position = position, role = role)
}

#' Constructor for Voter with role `"Veto"`.
#'
#' @param position A position vector.
#' @rdname Voter
#' @export
Veto <- function(position) {
  methods::new("Voter", position = position, role = "Veto")
}

#' Constructor for Voter with role `"AS"`.
#'
#' @param position A position vector.
#' @rdname Voter
#' @export
AS <- function(position) {
  methods::new("Voter", position = position, role = "AS")
}

#' Constructor for Voter with role `"Random"`.
#'
#' @param position A position vector.
#' @rdname Voter
#' @export
RandomVoter <- function(position) {
  methods::new("Voter", position = position, role = "Random")
}

#' S4 class representing the Status Quo.
#'
#' @slot position A position vector.
#' @slot dimension
#'
#' @name SQ-class
#' @aliases SQ
#' @rdname SQ-class
.SQ <- setClass(
  "SQ",
  contains = "Position"
)


#' Constructor for the SQ class.
#'
#' @param position A position vector.
#' @rdname SQ-class
#' @export
SQ <- function(position) {
  methods::new("SQ", position = position)
  # .SQ(position = position)
}
