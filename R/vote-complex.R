# TODO: Lazyeval to get voter names here for `voters`
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
    .Voters(
      position = c(e1@position, e2@position),
      dimension = e1@dimension,
      role = c(e1@role, e2@role),
      voter_count = length(e1@role) + length(e2@role)
    )
  })

setMethod("+", signature = signature(e1 = "Voters", e2 = "Voter"),
  function(e1, e2) {
    check_positions(e1, e2)
    .Voters(
      position = c(e1@position, e2@position),
      dimension = e1@dimension,
      role = c(e1@role, e2@role),
      voter_count = e1@voter_count + length(e2@role)
    )
  })

setMethod("+", signature = signature(e1 = "Voter", e2 = "Voters"),
  function(e1, e2) {
    check_positions(e1, e2)
    .Voters(
      position = c(e1@position, e2@position),
      dimension = e1@dimension,
      role = c(e1@role, e2@role),
      voter_count = length(e1@role) + e2@voter_count
    )
  })

setMethod("+", signature = signature(e1 = "Voters", e2 = "Voters"),
  function(e1, e2) {
    check_positions(e1, e2)
    .Voters(
      position = c(e1@position, e2@position),
      dimension = e1@dimension,
      role = c(e1@role, e2@role),
      voter_count = sum(e1@voter_count, e2@voter_count)
    )
  }
)

check_positions <- function(v1, v2) {
  if (!is.position(v1) || (nargs() == 2 && !is.position(v2))) {
    stop("Operator requires Position objects as input.")
  }
  if (v1@dimension != v2@dimension) {
    stop("Position objects must have equal dimensions.")
  }
}

as.voters <- function (voter) {
  UseMethod("as.voters", voter)
}

as.voters.Voter <- function(voter) {
  .Voters(voter@position, voter@dimension, voter@role, 1)
}
