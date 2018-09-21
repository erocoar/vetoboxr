library(CVXR)

solve_vote <- function(prob) {
  prob_data <- get_problem_data(prob, solver = "ECOS")
  ECOSolveR::ECOS_csolve(c = prob_data[["c"]],
                         G = prob_data[["G"]],
                         h = prob_data[["h"]],
                         dims = prob_data[["dims"]],
                         A = prob_data[["A"]],
                         b = prob_data[["b"]])$x[seq(2)]
}

Vote <- function(sq, voterlist, iter) {
  
  v_roles <- sapply(voterlist, function(object) object@role)
  v_pos   <- sapply(voterlist, function(object) object@position)
  
  as_idx <- which(v_roles == "AS")
  veto_idx <- which(v_roles == "Veto")
  normal_idx <- setdiff(seq_along(v_roles), c(veto_idx, as_idx))
  
  sq <- sq@position
  
  v_sq <- matrix(sq, nrow = iter + 1, ncol = length(sq), byrow = TRUE)
  
  for (i in seq(1, iter)) {
   new_sq <- Variable(2, 1)
   obj <- Minimize(p_norm(v_pos[, as_idx] - new_sq))
   
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
       
       intermediate_sqs[j, ] <- solve_vote(Problem(obj, constraints))
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


Vote(SQ(c(1,2)), voterlist = list(Voter(c(7,5), "AS"), Voter(c(3,4), "Veto"), Voter(c(1,1), ""), 
                                  Voter(c(5, 9), "")), iter = 3)
