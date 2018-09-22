#' Determine possible coalitions based on agenda setter, veto players and normal voters.
#'
#' The agenda setter and veto player must always be part of the coalition.
#' This function returns possible combinations of AS+Veto+Normal that form a majority.
#'
#' @param as Agenda Setter Index.
#' @param veto Veto Player Index.
#' @param normal Normal Voter Index.
#'
#' @export
determine_coalitions <- function(as, veto, normal) {
  votercount <- length(c(veto, normal, as))
  required_voters <- c(as, veto)
  if (length(required_voters) <= 0.5 * votercount) {
    more_voters <- ceiling(0.5 * votercount + 1) - votercount %% 2 - length(required_voters)
    possible_coalitions <- combn(normal, more_voters)
    possible_coalitions
  }
}


