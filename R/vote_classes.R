setOldClass("data.frame")
setClassUnion("data.frameORvector", c("data.frame", "vector"))

Position <- setClass("Position",
  slots = c(
    position = "data.frameORvector",
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

Voter <- function(position, role = "Normal") {
  v <- methods::new("Voter", position = position, role = role)
  v
}

.SQ <- setClass("SQ",
  contains = "Position"
)

SQ <- function(position) {
  methods::new("SQ", position = position)
  # .SQ(position = position)
}



