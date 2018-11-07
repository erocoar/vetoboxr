sq <- sp::SpatialPoints(data.frame(x=2, y=4))
voters <- sp::SpatialPoints(data.frame(x=c(3,1,7), y=c(4,1,3)))

voter_list <- list()

for (v in seq(length(voters))) {
  voter_list <- c(voter_list,
                  rgeos::gBuffer(voters[v], width = norm(voters[v]@coords - c(2, 4), "2")))
}

intersection = voter_list[[3]]

for (v in seq(length(voter_list) - 1)) {
  intersection = rgeos::gIntersection(intersection, voter_list[[v]])
}