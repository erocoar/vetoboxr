#' Determine possible coalitions based on agenda setter, veto players and normal voters.
#' The agenda setter and veto players must always be part of the coalition.
#' \code{determine_voters} determines how many voters, apart from agenda setter and veto players, are required to form a majority coalition.
#' \code{determine_coalitions} lists all possible combinations of normal voters to join such coalition.
#'
#' @name determine_coalitions
NULL


#' @param as Agenda Setter Index.
#' @param veto Veto Player Index.
#' @param normal Normal Voter Index.
#' @rdname determine_coalitions
#' @export
determine_voters <- function(as, veto, normal) {
  votercount <- length(c(veto, normal, as))
  required_voters <- c(as, veto)
  if (length(required_voters) <= 0.5 * votercount) {
    more_voters <- ceiling(0.5 * votercount + 1) - votercount %% 2 - length(required_voters)
  } else {
    more_voters <- 0
  }
  more_voters
}

#' @param more_voters Number of required normal voters to form majority, as determined by \code{determine_voters}.
#' @importFrom utils combn
#' @rdname determine_coalitions
#' @export
determine_coalitions <- function(normal, more_voters) {
  n <- length(normal)
  if (more_voters > n) {
    return(NULL)
  } else if (n == 1) {
    return(cbind(normal))
  }
  combn(normal, more_voters)
}



# determine_coalitions uses `combn()` to find all possible combinations of normal voters
# that can achieve a majority vote together with veto players and agenda setter
# naturally this becomes prohibitively expensive with increasing `voter_count`,
# so check before that whether a normal voter can even enter a coalition
# voters can enter a coalition if they can form a winset together with the agenda setter
# meaning the voter and as indifference circles touch not just at the sq (as they always do)
# but also at some other point

# so general structure is
# 1 - check whether AS + Veto has winset, if not, SQ will be outcome either way ...
# 2 - if winset, check if majority is needed
# 3 - if needed, check which normal voter has winset (>1 intersection with AS) and veto
# 4 - if enough voters are found, create combn coalitions
# 5 - foreach coalition check whether each voter has >1 intersection with each other voter

check_viability <- function(voter_positions, voter_radii) {
  # voter pos always length 2 * dim
  # voter radii always length 2
  # as_index always 1! so voter_index 2
  # and x indices c(1, 3)
  # y indices c(1, 3) + 1

  # first check if voter positions are equal i.e. infinite intersections
  as_position <- voter_positions[seq(1, 2)]
  voter_position <- voter_positions[seq(3, 4)]

  if (isTRUE(all.equal(unname(as_position), unname(voter_position)))) {
    return(TRUE)
  }
  d <- norm(as_position - voter_position, "2")
  !(isTRUE(all.equal(sum(voter_radii), d)) || isTRUE(all.equal(voter_radii[1] - voter_radii[2], d)))
}
#
# check_viability(c(6, 2, 6, 2), vote$voter_radii[1, c(3, 3)])


# for each coalition check whether winset could be non-empty
check_winset <- function(voter_position, voter_radii, dimension) {
  k <- length(voter_position)
  n <- k / dimension
  x_idx <- seq(1, k, 2)
  y_idx <- x_idx + 1
  seq_idx <- lapply(seq(n), function(x) voter_position[c(x_idx[x], y_idx[x])])
  viability <- matrix(FALSE, n, n)
  for (i in seq(n)[-n]) {
    for (j in seq(i + 1, n)) {
      viability[i, j] <-
        check_viability(voter_position[c(x_idx[i] + c(0, 1), x_idx[j] + c(0, 1))],
                      voter_radii[c(i, j)])
    }
  }
  all(viability[upper.tri(viability)])
}
