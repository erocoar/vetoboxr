#' Convert a \code{Vote} object into a tidy dataframe.
#' @param vote A \code{Vote} object.
#' @export
vote_df <- function(vote) {
  voter_pos_idx <- seq(1, ncol(vote$voter_position), vote$dimension)
  coalition_array <- create_coalition_array(vote)
  voter_data <- lapply(seq(vote$voter_count), function(x) {
    data <- cbind.data.frame(
      vote$voter_position[, create_index(voter_pos_idx[x], vote$dimension), drop = FALSE],
      vote$voter_roles[, x],
      vote$voter_radii[, x],
      vote$voter_payoff[, x],
      coalition_array[, x]
      )
    colnames(data)[-c(1, 2)] <- paste0("Voter ", x, c(" Role", " Radius", " Payoff", " Coalition"))
    data
  })
  voter_data <- do.call(cbind.data.frame, voter_data)
  voter_data <- cbind.data.frame(voter_data, vote$dimension_distance,
                                 vote$total_distance, vote$total_payoff,
                                 vote$status_quo, vote$outcome)
  voter_data$`Dimension` <- vote$dimension
  if ("winset_area" %in% names(vote)) {
    voter_data$winset_area <- vote$winset_area
  }
  voter_data
}
