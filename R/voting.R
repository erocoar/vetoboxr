#' Solve a voting problem using \code{ECOSolver}.
#'
#' @param prob A problem of class `CVXR::Problem`
#' @param dimension The dimension of the problem.
#'
#' @importFrom CVXR get_problem_data Problem Minimize
#' @importFrom ECOSolveR ECOS_csolve
solve_vote <- function(prob, dimension) {
  prob_data <- CVXR::get_problem_data(prob, solver = "ECOS")
  ECOSolveR::ECOS_csolve(
    c = prob_data[["c"]],
    G = prob_data[["G"]],
    h = prob_data[["h"]],
    dims = prob_data[["dims"]],
    A = prob_data[["A"]],
    b = prob_data[["b"]],
    control = ECOSolveR::ecos.control(feastol = .Machine$double.eps))$x[seq(dimension)]
}

#' Vote.
#'
#' Executes the spatial model of voting, solving, for each iteration, the constrained optimization problem of minimizing the distance of the Status Quo to the Agenda Setter by the means of a majority vote.
#'
#' @param formula A formula with SQ on LHS and Voters on RHS.
#' @param voters (required if no formula principal argument is given.) An object of class `"Voters"`.
#' @param sq (required if no formula principal argument is given.) An object of class `"SQ"`.
#' @param x ()
#' @param drift A vector detailing the drift of each voter in each dimension.
#' @param vibration A function generating random noise, must take `n` as first argument.
#' @param iter The number of votes to cast.
#' @param keep_winset_objects Boolean indicator for whether to keep winset spatial polygons.
#' @param random_veto_count If there are `"Random"` voters, how many are assigned `"Veto"` role per round.
#' @param winset_quadsegs Number of linear segments used to approximate voter indifference circles.
#' @param ... Additional keyword arguments.
#'
#' @importFrom CVXR Variable Minimize p_norm Problem
#' @importFrom stats dist terms
#' @export


#' @rdname Vote
#' @export
Vote <- function (x, ...) {
  UseMethod("Vote")
}


#' @rdname Vote
#' @export
Vote.formula <- function(
  formula,
  drift = NULL,
  vibration = NULL,
  iter = 1,
  keep_winset_objects = TRUE,
  random_veto_count = NULL,
  winset_quadsegs = 100,
  ...
) {
  # function_call <- match.call()
  sq <- eval(formula[[2]])
  voters <- eval(formula[[3]])

  # voter_names <- labels(terms(formula))
  Vote(voters, sq, drift, vibration, iter, keep_winset_objects,
       random_veto_count, winset_quadsegs, ...)
}

#' @rdname Vote
#' @export
Vote.Voters <- function(
  voters,
  sq,
  drift = NULL,
  vibration = NULL,
  iter = 1,
  keep_winset_objects = TRUE,
  random_veto_count = FALSE,
  winset_quadsegs = 100,
  ...) {

  function_call <- match.call()
  # sq <- eval(formula[[2]])
  # voters <- eval(formula[[3]])

  # initialize attributes
  # voter_names <- labels(terms(formula))
  voter_count <- voters@voter_count
  voter_roles <- create_role_array(voters, iter, random_veto_count, ...)
  voter_array <- create_voter_array(voters, drift, vibration, iter, ...)

  voter_coalitions <- vector("list", iter)

  dimension <- sq@dimension

  status_quo_drift <- sq@drift

  # check that dimension is equal among sq and voters
  # check that all iterations have exactly 1 AS
  stopifnot(dimension == voters@dimension && all(rowSums(voter_roles == "AS") == 1))

  # initialize arrays
  status_quo <- matrix(Inf, ncol = dimension, nrow = iter + 1)
  status_quo[1, ] <- sq@position

  voter_position <- voter_array@position
  voter_radii <- matrix(Inf, ncol = voter_count, nrow = iter)

  # general index for x value of each voter
  x_idx <-  seq(1, voter_count * dimension, dimension)

  for (i in seq(1, iter)) {
    cat(i, "\n")
    # 0. update sq-drift
    # TODO implement good SQ class checks
    # TODO sq_drift function argument by default n / dim?
    status_quo[i, ] <- status_quo[i, ] + status_quo_drift()#status_quo_drift

    # 1. create indices for each role
    as_index_orig     <- which(voter_roles[i, ] == "AS")
    as_index_x        <- x_idx[as_index_orig]
    as_index_full     <- create_index(as_index_x, dimension)
    veto_index_orig   <- which(voter_roles[i, ] == "Veto")
    veto_index_x      <- x_idx[veto_index_orig]
    veto_index_full   <- create_index(veto_index_x, dimension)
    normal_index_orig <- which(voter_roles[i, ] == "Normal")
    normal_index_x    <- x_idx[normal_index_orig]
    normal_index_full <- create_index(normal_index_x, dimension)

    # 2. calculate voter-sq distance
    voter_radii[i, ] <- sapply(x_idx, function(x) {
      full_idx <- create_index(x, dimension, sort = FALSE)
      norm(status_quo[i, ] - voter_position[i, full_idx], "2")
    })

    # 3. check if AS == SQ in which case the SQ will not change
    if (isTRUE(all.equal(unname(voter_position[i, as_index_full]), status_quo[i, ]))) {
      status_quo[i + 1, ] <- status_quo[i, ]
      next()
    }

    # 4. check if AS and Veto Players have mutual winset, if not, SQ will not change
    if (!check_winset(
      voter_position[i, c(as_index_full, veto_index_full)],
      voter_radii[i, c(as_index_orig, veto_index_orig)],
      dimension
    )) {
      status_quo[i + 1, ] <- status_quo[i, ]
      next()
    }

    # 5. initialize CVXR SQ, vote objective and veto constraints
    cvxr_sq <- CVXR::Variable(dimension, 1)
    cvxr_obj <- CVXR::Minimize(CVXR::p_norm(voter_position[i, as_index_full] - cvxr_sq))

    veto_constraints <- lapply(veto_index_x, function(x) {
      full_idx <- create_index(x, dimension, sort = FALSE)
      p_norm(cvxr_sq - voter_position[i, full_idx]) <=
        p_norm(status_quo[i, ] - voter_position[i, full_idx])
    })

    # 6. check how many normal voters are needed for majority
    more_voters <- determine_voters(as_index_orig, veto_index_orig, normal_index_orig)

    if (more_voters > 0) {
      # 7. if coalition required, check which Normal have winset with AS
      viable_normal <- sapply(seq_along(normal_index_orig), function(x) {
        check_viability(voter_position[i, c(as_index_full, normal_index_x[x] + c(0, 1))],
                        voter_radii[i, c(as_index_orig, normal_index_orig[x])])
      })

      # 8. determine all combns of eligible normal voters
      coalitions <- determine_coalitions(normal_index_orig[viable_normal], more_voters)

      if (is.null(coalitions)) {
        status_quo[i + 1, ] <- status_quo[i, ]
        next()
      }

      # 9. determine which coalitions have mutual winsets
      viable_coals <- sapply(seq(ncol(coalitions)), function(j) {
        check_winset(voter_position[i, create_index(x_idx[coalitions[, j]], dimension)],
                     voter_radii[i, coalitions[, j]], dimension)
      })

      if (!any(viable_coals)) {
        status_quo[i + 1, ] <- status_quo[i, ]
        next()
      }

      coalitions <- coalitions[, viable_coals, drop = FALSE]

      # 10. determine vote outcome for each feasible coalition
      intermediate_sqs <- matrix(0, ncol = dimension, nrow = ncol(coalitions))
      for (j in seq(ncol(coalitions))) {
        constraints <- c(
          veto_constraints,
          lapply(coalitions[, j], function(x) {
            x <- x_idx[x]
            full_idx <- create_index(x, dimension, sort = FALSE)
            p_norm(cvxr_sq - voter_position[i, full_idx]) <=
              p_norm(status_quo[i, ] - voter_position[i, full_idx])
          }))
        intermediate_sqs[j, ] <- solve_vote(CVXR::Problem(cvxr_obj, constraints), dimension)
      }

      # 11. for each outcome, determine distance to AS. Min. distance is eventual coalition
      coalition_distances <- dist(rbind(voter_position[i, as_index_full], intermediate_sqs))
      coalition_distances <- coalition_distances[seq(1, nrow(intermediate_sqs))]

      win_coalition <- which.min(coalition_distances)
      status_quo[i + 1, ] <- intermediate_sqs[win_coalition, ]

      # 12. if SQ did in fact move, update the coalition. otherwise, there shouldn't have been one
      if (!isTRUE(all.equal(status_quo[i, ], status_quo[i + 1, ]))) {
        voter_coalitions[[i]] <- c(as_index_orig, veto_index_orig, coalitions[, win_coalition])
      }
    } else {
      status_quo[i + 1, ] <- solve_vote(Problem(cvxr_obj, veto_constraints), dimension)
      voter_coalitions[[i]] <- c(as_index_orig, veto_index_orig)
    }
  }

  # 13. prepare result data
  outcome <- status_quo[-1, , drop = FALSE]
  colnames(outcome) <- paste0("Outcome ", seq(1, dimension))
  status_quo <- status_quo[-nrow(status_quo), , drop = FALSE]
  colnames(status_quo) <- paste0("Status Quo ", seq(1, dimension))
  total_distance <- cbind("Total Distance" = apply(outcome - status_quo, 1, norm, "2"))
  dimension_distance <- sqrt((outcome - status_quo)^2)
  colnames(dimension_distance) = paste0("Distance Dimension ", seq(dimension))

  # to get payoff, voter radii must be calculated for the outcome of last iter
  voter_rad_n <- sapply(x_idx, function(x) {
    full_idx <- create_index(x, dimension, sort = FALSE)
    norm(outcome[iter, ] - voter_position[iter, full_idx], "2")
  })

  payoff <- voter_radii - rbind(voter_radii[-1, ], voter_rad_n)
  aggregate_payoff <- rowSums(payoff)

  out <- list(
    call = function_call,
    args = list(voters = voters,
                voter_array = voter_array,
                sq = sq,
                vibration = vibration,
                drift = drift,
                iter = iter),
    dimension = dimension,
    # voter_names = voter_names,
    iter = iter,
    voter_count = voter_count,
    voter_roles = voter_roles,
    voter_position = voter_position,
    voter_radii = voter_radii,
    coalitions = voter_coalitions,
    dimension_distance = dimension_distance,
    voter_payoff = payoff,
    total_distance = total_distance,
    total_payoff = data.frame("Total Payoff" = aggregate_payoff, check.names = FALSE),
    status_quo = status_quo,
    outcome = outcome
  )

  if (dimension == 2) {
    winsets <- get_winset(out, seq(1, iter), quadsegs = winset_quadsegs)
    if (isTRUE(keep_winset_objects)) {
      out$winsets <- winsets
    }

    out$winset_area <- sapply(winsets, function(x) {
      if (!is.null(x)) x@polygons[[1]]@area else x
    })
  }

  class(out) <- "Vote"
  out
}

