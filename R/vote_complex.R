# TODO: Lazyeval to get voter names here for `voters`
.Voters <- setClass("Voters",
  contains = "Position"
)

setMethod("initialize", "Voters",
  function(.Object, position, dimension) {
    .Object@position <- position
    .Object@dimension <- dimension
    .Object
  }
)

setMethod("+", signature = signature(e1 = "Voter", e2 = "Voter"),
  function(e1, e2) {
    check_positions(e1, e2)
    .Voters(
      position = cbind(e1@position, e2@position),
      dimension = e1@dimension
    )
  })

setMethod("+", signature = signature(e1 = "Voters", e2 = "Voter"),
  function(e1, e2) {
    check_positions(e1, e2)
    .Voters(
      position = cbind(e1@position, e2@position),
      dimension = e1@dimension
    )
  })

setMethod("+", signature = signature(e1 = "Voter", e2 = "Voters"),
  function(e1, e2) {
    check_positions(e1, e2)
    .Voters(
      position = cbind(e1@position, e2@position),
      dimension = e1@dimension
    )
  })

check_positions <- function(v1, v2) {
  if (!is.position(v1) || (nargs() == 2 && !is.position(v2))) {
    stop("Operator requires Position objects as input.")
  }
  if (v1@dimension != v2@dimension) {
    stop("Position objects must have equal dimensions.")
  }
}
