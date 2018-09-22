setClass("Voting", 
  slots = c(
    voters = "character",
    voter_positions = "Position",
    sq = "Position",
    dimension = "numeric"
  )
)

# TODO: Lazyeval to get voter names here for `voters`
setMethod("initialize", "Voting", 
  function(.Object, voters, voter_positions, sq, dimension) {
    .Object@voters <- voters
    .Object@voter_positions <- NA
  }
)

setMethod("+", signature(v1 = "Position", v2 = "Position"),
  function(v1, v2) {
    check_positions(v1, v2)
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

