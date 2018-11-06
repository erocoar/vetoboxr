#'
#' @importFrom sp SpatialPoints
#' @importFrom rgeos gBuffer gIntersection
get_winset <- function(vote, iter = NA) {

}


voters <- Voter(c(3, 4), "Veto") + Voter(c(1, 1), "Veto") + Voter(c(7, 3), "AS")



form <- SQ(c(2, 4)) ~ Voter(c(3, 4), "Veto") + Voter(c(1, 1), "Veto") + Voter(c(7, 3), "AS")
Vote(SQ(c(2, 4)) ~ Voter(c(3, 4), "Veto") + Voter(c(1, 1), "Veto") + Voter(c(7, 3), "AS"), iter = 1)

library(rgeos)

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

library(ggplot2)
ggplot() + geom_polygon(aes(x = intersection@polygons[[1]]@Polygons[[1]]@coords[,1],
                            y = intersection@polygons[[1]]@Polygons[[1]]@coords[,2]), alpha = 0.4) +
  geom_point(aes(x=2,y=4), color="darkgreen", size=2) +
  geom_point(aes(x=c(3, 1), y = c(4, 1),), color="red", size=2) +
  geom_point(aes(x=7,y=3), color="blue", size=2) +
  geom_circle(aes(x = c(3, 1), y = c(4, 1), r =
                    c(norm(c(3,4) - c(2, 4), "2"), norm(c(1,1) - c(2,4), "2")))) +
  geom_circle(aes(x=7,y=3,r=norm(c(2,4) - c(7, 3), "2"))) +
  coord_fixed()
