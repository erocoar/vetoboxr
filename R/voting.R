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
Vote <- function(formula, iter) {
  sq <- eval(formula[[2]])
  voters <- eval(formula[[3]])
  voter_names <- labels(terms(formula))
  dimension = sq@dimension
  sq <- sq@position
  roles <- voters@role

  if (dimension != voters@dimension) {
    stop("SQ and Voter objects must have equal dimensions.")
  }

  if (!"AS" %in% roles || sum(roles == "AS") > 1) {
    stop("There must be exactly one Agenda Setter.")
  }

  as_idx <- which(roles == "AS")
  veto_idx <- which(roles == "Veto")
  normal_idx <- which(roles == "Normal")

  v_sq <- matrix(sq, nrow = iter + 1, ncol = dimension, byrow = TRUE)
  v_pos <- voters@position

  for (i in seq(1, iter)) {
   new_sq <- CVXR::Variable(dimension, 1)
   asd <<- list(v_pos[, as_idx], new_sq)
   obj <- CVXR::Minimize(CVXR::p_norm(v_pos[, as_idx] - new_sq))

   coalitions <- determine_coalitions(as_idx, veto_idx, normal_idx)

   veto_constraints <- lapply(veto_idx, function(x) {
     p_norm(new_sq - v_pos[, x]) <= p_norm(v_sq[i, ] - v_pos[, x])
   })

   if (!is.null(coalitions)) {
     intermediate_sqs <- matrix(0, nrow = ncol(coalitions), ncol = 2)
     for (j in seq(1, ncol(coalitions))) {
       constraints <- c(veto_constraints,
        lapply(coalitions[, j], function(x) {
          p_norm(new_sq - v_pos[, x]) <= p_norm(v_sq[i, ] - v_pos[, x])
        }))

       intermediate_sqs[j, ] <- solve_vote(CVXR::Problem(obj, constraints))
     }

     dists <- dist(rbind(v_pos[, as_idx], intermediate_sqs))
     dists <- dists[seq(1, nrow(intermediate_sqs))]
     v_sq[i + 1, ] <- intermediate_sqs[which.min(dists), ]
   } else {
     v_sq[i + 1, ] <- solve_vote(Problem(obj, veto_constraints))
   }
  }
  v_sq
}




