.Voter <- setClass("Voter", 
  slots = c(
    position = "numeric",
    role = "character"
  )
)

Voter <- function(position, role) {
  v <- methods::new("Voter", position = position, role = role)
  v
}

.SQ <- setClass("SQ",
  slots = c(
    position = "numeric"
  )
)

SQ <- function(position) {
  .SQ(position = position)
}

