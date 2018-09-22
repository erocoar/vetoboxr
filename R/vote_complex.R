# TODO: Lazyeval to get voter names here for `voters`
.Voters <- setClass("Voters",
  contains = "Voter"
)

setMethod("initialize", "Voters",
  function(.Object, position, dimension, role) {
    .Object@position <- position
    .Object@dimension <- dimension
    .Object@role <- role
    .Object
  }
)

setMethod("+", signature = signature(e1 = "Voter", e2 = "Voter"),
  function(e1, e2) {
    check_positions(e1, e2)
    .Voters(
      position = cbind(e1@position, e2@position),
      dimension = e1@dimension,
      role = c(e1@role, e2@role)
    )
  })

setMethod("+", signature = signature(e1 = "Voters", e2 = "Voter"),
  function(e1, e2) {
    check_positions(e1, e2)
    .Voters(
      position = cbind(e1@position, e2@position),
      dimension = e1@dimension,
      role = c(e1@role, e2@role)
    )
  })

setMethod("+", signature = signature(e1 = "Voter", e2 = "Voters"),
  function(e1, e2) {
    check_positions(e1, e2)
    .Voters(
      position = cbind(e1@position, e2@position),
      dimension = e1@dimension,
      role = c(e1@role, e2@role)
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
