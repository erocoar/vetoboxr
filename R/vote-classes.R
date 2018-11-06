# Vote(SQ(c(2, 4)) ~ Voter(c(3, 4), "Veto") + Voter(c(1, 1), "Veto") + Voter(c(7, 3), "AS"), iter = 1)
# setOldClass("data.frame")
# setClassUnion("data.frameORvector", c("data.frame", "vector"))

Position <- setClass("Position",
  slots = c(
    position = "vector",
    dimension = "numeric"
  )
)

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

is.position <- function(x) {
  inherits(x, "Position")
}

.Voter <- setClass("Voter",
  slots = c(
    role = "character"
  ),
  prototype = prototype(
    role = NA_character_
  ),
  contains = "Position"
)

setMethod("initialize", "Voter", function(.Object, position, role) {
  .Object <- callNextMethod(.Object, position)
  .Object@role <- role
  .Object
})

#'
#' @param position
#' @param role
#' @export
Voter <- function(position, role = "Normal") {
  methods::new("Voter", position = position, role = role)
}


#'
#' The SQ class.
#'
#' This class
#'
#' @slot position
#' @slot dimension
#'
#' @name SQ-class
#' @aliases SQ
#' @rdname SQ-class
.SQ <- setClass("SQ",
  contains = "Position"
)


#'
#' @param position
#' @export
SQ <- function(position) {
  methods::new("SQ", position = position)
  # .SQ(position = position)
}



