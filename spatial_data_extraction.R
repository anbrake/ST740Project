
library(sf) # to read in shapefile
library(geosphere) # to calculate centroids

# Can you get the points of the perimeter of each community area?

com_boundary_init <- st_read("comarea/ComArea_ACS14_f.shp")

# sort com_boundary by ComAreaID

com_boundary_order <- com_boundary_init[order(com_boundary_init$ComAreaID),]

# compare with comarea. Looks like these arbitrarily selected columns match up.

# all.equal(comarea$shape_area.N.33.31, com_boundary_order$shape_area)
# all.equal(comarea$Over21P.N.18.4, com_boundary_order$Over21P)
# all.equal(comarea$Blk14.N.18.0, com_boundary_order$Blk14)
# all.equal(comarea$Hlitave.N.18.4, com_boundary_order$Hlitave)
# all.equal(comarea$Tuberculosis, com_boundary_order$Tuberc)

# grab the column names of com_boundary_order, to use for comarea
clean_names <- names(com_boundary_order)[-87]

# save(clean_names, file = "clean_names.RData")



# now, delete the columns already in comarea--don't need those (except for ComAreaID and community name).

com_boundary_omit <- com_boundary_order[, -c(6:86)]

com_boundary <- com_boundary_omit

num_com <- nrow(com_boundary)



# now, to find adjacencies.

# adjacency algorithm: 

# for a community, go through all the communities with a higher ComAreaID
# if they share at least one coordinate, call them adjacent.

com_coords <- vector("list", num_com)

# list of coordinates for each community area 

cb_geom <- com_boundary$geometry

for (i in 1:num_com) {
    
    com_coords[[i]] <- cb_geom[[i]][[1]][[1]]
    
    # there's one community area, Norwood Park, that has two inner lists
    # cb_geom[[10]][[1]][[1]] and cb_geom[[10]][[1]][[2]]. I ignore 
    # the second one, because that's the hole inside the outer perimeter.
    
    # As it turns out, O'Hare has two tiny regions in addition to one big region,
    # cb_geom[[10]][[2]][[1]] and cb_geom[[10]][[3]][[1]]. They're negligible, so I ignore those.
    
}



# approach: paste the long lat coordinates together.

com_coords_paste <- vector("list", num_com)

for (i in 1:num_com) {
  
  com_coords_paste[[i]] <- apply(com_coords[[i]], 1, paste, collapse=" ")
  
}



# returns 0 if two communities aren't adjacent, 1 if they are
adjacent_com <- function(com1, com2, coords_paste) {
  
  shared_coords <- intersect(com_coords_paste[[com1]], com_coords_paste[[com2]])
  
  if (length(shared_coords) == 0) {
    return(0)
  } else if (length(shared_coords) > 0) {
    return(1)
  } else {
    return(NA)
  }
  
}



com_adj_mat <- matrix(0, nrow = num_com, ncol = num_com)

for (i in 1:(num_com - 1)) {
  
  for (j in (i + 1):num_com) {
    
    com_adj_mat[i, j] <- adjacent_com(i, j, com_coords_paste)
    
  }
  
}

com_adj_mat_half <- com_adj_mat[upper.tri(com_adj_mat)]
com_adj_mat <- t(com_adj_mat)
com_adj_mat[upper.tri(com_adj_mat)] <- com_adj_mat_half

save(com_adj_mat, file = "com_adj_mat.RData")



# checking the adjacency matrix

comarea_names <- com_boundary$community

# adjacency is good for Roger's Park
comarea_names[which(com_adj_mat[1,] == 1)]

# adjacency is good for O'Hare
comarea_names[which(com_adj_mat[76,] == 1)]

# adjacency is good for East Garfield Park
comarea_names[which(com_adj_mat[27,] == 1)]

# adjacency is good for Uptown
comarea_names[which(com_adj_mat[3,] == 1)]






# Can you get the centroids of the community areas?

centroid(com_coords[[72]])



CentroidLat <- rep(NA, num_com)

CentroidLon <- rep(NA, num_com)



for (i in 1:num_com) {
  
  cent <- centroid(com_coords[[i]])
  
  CentroidLat[i] <- cent[2]
  
  CentroidLon[i] <- cent[1]
  
}



# Find the distance from the Loop (Chicago's central business district) to each centroid

Dist2Loop <- rep(NA, num_com)

loop_cent <- c(CentroidLat[32], CentroidLon[32])

for (i in 1:num_com) {
  
  Dist2Loop[i] <- sqrt(sum((c(CentroidLat[i], CentroidLon[i]) - loop_cent)^2))
  
}



# 3 new variables to merge in:
#' CentroidLat
#' CentroidLon
#' Dist2Loop




