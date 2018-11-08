check_positions <- function(v1, v2) {
  if (!is.position(v1) || (nargs() == 2 && !is.position(v2))) {
    stop("Operator requires Position objects as input.")
  }
  if (v1@dimension != v2@dimension) {
    stop("Position objects must have equal dimensions.")
  }
}

check_roles <- function(v1, v2) {
  if (is.matrix(v1@role) && is.matrix(v2@role)) {
    if (nrow(v1@role) != nrow(v2@role)) {
      stop("Roles must be of equal length.")
    }
  } else if (is.vector(v1@role) & is.vector(v2@role)) {
    if (length(v1@role) != length(v2@role)) {
      stop("Roles must be of equal length.")
    }
  } else if (is.vector(v1@role) & is.matrix(v2@role)) {
    if ((length(v1@role) != nrow(v2@role)) & length(v1@role) != 1) {
      stop("Roles must be of equal length.")
    }
  } else if (is.vector(v2@role) & is.matrix(v1@role)) {
    if (length(v2@role) != nrow(v1@role) & length(v2@role) != 1) {
      stop("Roles must be of equal length.")
    }
  }
}