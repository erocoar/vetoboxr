#' Solve a voting problem using `ECOSolveR::ECOS_csolve()`.
#'
#' @param prob A problem of class `CVXR::Problem`
#' @param dimension The dimension of the problem.
#'
#' @importFrom CVXR get_problem_data Problem Minimize
#' @importFrom ECOSolveR ECOS_csolve
solve_vote <- function(prob, dimension) {
  prob_data <- CVXR::get_problem_data(prob, solver = "ECOS")
  ECOSolveR::ECOS_csolve(c = prob_data[["c"]],
                         G = prob_data[["G"]],
                         h = prob_data[["h"]],
                         dims = prob_data[["dims"]],
                         A = prob_data[["A"]],
                         b = prob_data[["b"]])$x[seq(dimension)]
}

#' Vote.
#'
#' Executes the spatial model of voting, solving, for each iteration, the constrained optimization problem of minimizing the distance of the Status Quo to the Agenda Setter by the means of a majority vote.
#'
#' @param formula A formula with SQ on LHS and Voters on RHS.
#' @param drift A vector detailing the drift of each voter in each dimension.
#' @param vibration A function generating random noise, must take `n` as first argument.
#' @param iter The number of votes to cast.
#' @param keep_winset_objects Boolean indicator for whether to keep winset spatial polygons.
#' @param no_random_veto If there are `"Random"` voters, never assign them to the `"Veto"` role.
#' @param no_random_normal If there are `"Random"` voters, never assign them to the `"Normal"` role.
#' @param ...
#'
#' @importFrom CVXR Variable Minimize p_norm Problem
#' @export
Vote <- function(
  formula,
  drift = NULL,
  vibration = NULL,
  iter = 1,
  keep_winset_objects = TRUE,
  no_random_veto = FALSE,
  no_random_normal = FALSE,
  ...) {
  function_call <- match.call()
  sq <- eval(formula[[2]])
  voters <- eval(formula[[3]])

  # initialize attributes
  voter_names <- labels(terms(formula))
  voter_count <- voters@voter_count
  voter_roles <- create_role_array(voters, iter, no_random_veto, no_random_normal, ...)
  voter_array <- create_voter_array(voters, drift, vibration, iter, ...)

  voter_coalitions <- vector("list", iter)

  dimension <- sq@dimension

  # checks
  stopifnot(dimension == voters@dimension)
  stopifnot(rowSums(voter_roles == "AS") == rep(1, iter))

  # initialize arrays
  status_quo <- matrix(0, ncol = dimension, nrow = iter + 1)
  status_quo[1, ] <- sq@position

  voter_position <- voter_array@position
  voter_radii <- matrix(0, ncol = voter_count, nrow = iter)

  # general role idx to create role indices
  role_idx <-  seq(1, voter_count * dimension, dimension)

  for (i in seq(1, iter)) {

    # update role indices
    as_index_orig <- which(voter_roles[i, ] == "AS")
    as_index_role <- role_idx[as_index_orig]
    as_index_full <- create_index(as_index_role, dimension)
    veto_index_orig <- which(voter_roles[i, ] == "Veto")
    veto_index_role <- role_idx[veto_index_orig]
    veto_index_full <- create_index(veto_index_role, dimension)
    normal_index_orig <- which(voter_roles[i, ] == "Normal")
    normal_index_role <- role_idx[veto_index_orig]
    normal_index_full <- create_index(normal_index_role, dimension)

    voter_coalitions[[i]] <- which(voter_roles[i, ] %in% c("Veto", "AS"))

    # calculate sq radius by voter
    voter_radii[i, ] <- sapply(seq(1, voter_count * dimension, dimension), function(x) {
      x_idx <- if (dimension > 1) c(x, x + seq(dimension - 1)) else x
      norm(status_quo[i, ] - voter_position[i, x_idx], "2")
    })

    # check if as position == sq position in which case the outcome is predetermined
    # TODO

    # determine possible coalitions  given a selected majority rule.
    # coalitions necessarily include veto players and agenda setter.
    coalitions <- determine_coalitions(as_index_orig, veto_index_orig, normal_index_orig)

    # initialize CVXR objects
    cvxr_sq <- CVXR::Variable(dimension, 1)
    cvxr_obj <- CVXR::Minimize(CVXR::p_norm(voter_position[i, as_index_full] - cvxr_sq))

    veto_constraints <- lapply(veto_index_role, function(x) {
      x_idx <- if (dimension > 1) c(x, x + seq(dimension - 1)) else x
      p_norm(cvxr_sq - voter_position[i, x_idx]) <=
        p_norm(status_quo[i, ] - voter_position[i, x_idx])
    })

    # if there are coalitions, check all and store intermediate sqs
    if (!is.null(coalitions)) {
      intermediate_sqs <- matrix(0, ncol = dimension, nrow = ncol(coalitions))
      for (j in seq(ncol(coalitions))) {
        constraints <- c(
          veto_constraints,
          lapply(coalitions[, j], function(x) {
            x <- role_idx[x]
            x_idx <- if (dimension > 1) c(x, x + seq(dimension - 1)) else x
            p_norm(cvxr_sq - voter_position[i, x_idx]) <=
              p_norm(status_quo[i, ] - voter_position[i, x_idx])
            }))
        intermediate_sqs[j, ] <- solve_vote(CVXR::Problem(cvxr_obj, constraints), dimension)
      }

      coalition_distances <- dist(rbind(voter_position[i, as_index_full], intermediate_sqs))
      coalition_distances <- coalition_distances[seq(1, nrow(intermediate_sqs))]

      win_coalition <- which.min(coalition_distances)
      voter_coalitions[[i]] <- c(voter_coalitions[[i]], coalitions[, win_coalition])

      status_quo[i + 1, ] <- intermediate_sqs[win_coalition, ]
    } else {
      status_quo[i + 1, ] <- solve_vote(Problem(cvxr_obj, veto_constraints), dimension)
    }
  }

  outcome <- status_quo[-1, , drop = FALSE]
  status_quo <- status_quo[-nrow(status_quo), , drop = FALSE]

  total_distance <- cbind("Total Distance" = apply(outcome - status_quo, 1, norm, "2"))
  dimension_distance <- sqrt((outcome - status_quo)^2)
  colnames(dimension_distance) = paste0("Distance Dimension ", seq(dimension))

  # payoff <- voter_radii[-1, ] - voter_radii[-nrow(voter_radii), ] # TODO fix this: needs to be like sq -> outcome. so iter + 1.
  # total_payoff <- cbind("Aggregate Payoff" = rowSums(payoff))

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
    coalitions = voter_coalitions,
    dimension_distance = dimension_distance,
    # voter_payoff = payoff,
    total_distance = total_distance,
    # total_payoff = total_payoff,
    status_quo = status_quo,
    outcome = outcome
  )

  if (dimension == 2) {
    winsets <- get_winset(out, seq(1, iter))
    if (isTRUE(keep_winset_objects)) {
      out$winsets <- winsets
    }
    out$winset_area <- sapply(winsets, function(x) x@polygons[[1]]@area)
  }

  class(out) <- "Vote"
  out
}
