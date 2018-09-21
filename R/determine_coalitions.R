determine_coalitions <- function(as, veto, normal) {
  votercount <- length(c(veto, normal, as))
  required_voters <- c(as, veto)
  if (length(required_voters) <= 0.5 * votercount) {
    more_voters <- ceiling(0.5 * votercount + 1) - votercount %% 2 - length(required_voters)
    possible_coalitions <- combn(normal, more_voters)
    possible_coalitions
  } 
}