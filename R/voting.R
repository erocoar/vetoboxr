# library(CVXR)

#' @importFrom CVXR get_problem_data Problem Minimize
#' @importFrom ECOSolveR ECOS_csolve
solve_vote <- function(prob) {
  prob_data <- CVXR::get_problem_data(prob, solver = "ECOS")
  ECOSolveR::ECOS_csolve(c = prob_data[["c"]],
                         G = prob_data[["G"]],
                         h = prob_data[["h"]],
                         dims = prob_data[["dims"]],
                         A = prob_data[["A"]],
                         b = prob_data[["b"]])$x[seq(2)]
}

#' Vote.
#'
#' Executes the spatial model of voting, solving the `Formula`
#' constrained optimization problem.
#'
#' @param formula A formula with SQ on LHS and Voters on RHS.
#' @param iter Number of runs.
#'
#' @importFrom CVXR Variable Minimize p_norm Problem
#' @export
#'
Vote <- function(
  formula,
  drift = NULL,
  vibration = NULL,
  iter = 1,
  ...) {
  function_call <- match.call()
  sq <- eval(formula[[2]])
  voters <- eval(formula[[3]])

  # initialize attributes
  voter_names <- labels(terms(formula))
  voter_count <- voters@voter_count
  voter_roles <- voters@role
  voter_array <- create_voter_array(voters, drift, vibration, iter, ...)

  dimension <- sq@dimension

  # checks
  stopifnot(dimension == voters@dimension)
  stopifnot("AS" %in%  roles && (sum(roles == "AS") == 1))

  # initialize arrays
  status_quo <- matrix(0, ncol = dimension, nrow = iter + 1)
  status_quo[1, ] <- sq@position

  voter_roles <- matrix(voter_roles, ncol = length(voter_roles), nrow = iter, byrow = TRUE)
  voter_position <- voter_array@position
  voter_radii <- matrix(0, ncol = voter_count, nrow = iter)

  dimension_distance <- matrix(0, ncol = dimension, nrow = iter)
  total_distance <- matrix(0, ncol = iter, nrow = 1)

  for (i in seq(1, iter)) {

    # update role indices
    as_index <- which(voter_roles[i, ] == "AS")
    as_index_full <- create_index(as_index, dimension)
    veto_index <- which(voter_roles[i, ] == "Veto")
    veto_index_full <- create_index(veto_index, dimension)
    normal_index <- which(voter_roles[i, ] == "Normal")
    normal_index_full <- create_index(normal_index, dimension)

    # calculate sq radius by voter
    voter_radii[i, ] <- sapply(seq(1, voter_count * dimension, dimension), function(x) {
      norm(status_quo[i, ] - voter_position[i, c(x, x + seq(dimension - 1))], "2")
    })

    # check if as position == sq position in which case the outcome is predetermined
    # TODO

    # determine possible coalitions  given a selected majority rule.
    # coalitions necessarily include veto players and agenda setter.
    coalitions <- determine_coalitions(as_index, veto_index, normal_index)

    # initialize CVXR objects
    cvxr_sq <- CVXR::Variable(dimension, 1)
    cvxr_obj <- CVXR::Minimize(CVXR::p_norm(voter_position[i, as_index] - cvxr_sq))

    veto_constraints <- lapply(veto_index, function(x) {
      p_norm(cvxr_sq - voter_position[i, c(x, x + seq(dimension - 1))]) <=
        p_norm(status_quo[i, ] - voter_position[i, c(x, x + seq(dimension - 1))])
    })

    # if there are coalitions, check all and store intermediate sqs
    if (!is.null(coalitions)) {
      intermediate_sqs <- matrix(0, ncol = dimension, nrow = ncol(coalitions))
      for (j in seq(ncol(coalitions))) {
        constraints <- c(veto_constraints,
          lapply(coalitions[, j], function(x) {
            p_norm(cvxr_sq - voter_position[i, c(x, x + seq(dimension - 1))]) <=
              p_norm(status_quo[i, ] - voter_position[i, c(x, x + seq(dimension - 1))])
          }))
        intermediate_sqs[j, ] <- solve_vote(CVXR::Problem(cvxr_obj, constraints))
      }

      coalition_distances <- dist(rbind(voter_position[i, as_index_full], intermediate_sqs))
      coalition_distances <- dists[seq(1, nrow(intermediate_sqs))]
      status_quo[i + 1, ] <- intermediate_sqs[which.min(dists), ]
    } else {
      status_quo[i + 1, ] <- solve_vote(Problem(cvxr_obj, veto_constraints))
    }
  }

  out <- list(
    call = function_call,
    args = list(voters = voters,
                voter_array = voter_array,
                sq = sq,
                vibration = vibration,
                drift = drift,
                iter = iter),
    dimension = dimension,
    voter_names = voter_names,
    voter_count = voter_count,
    voter_roles = voter_roles,
    voter_position = voter_position,
    voter_radii = voter_radii,
    dimension_distance = dimension_distance,#TODO
    voter_payoff = voter_payoff, #TODO
    total_distance = total_distance, #TODO
    total_payoff = total_payoff, #TODO
    status_quo = status_quo,
    outcome = outcome #TODO
  )
}

