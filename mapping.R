library(sf) # to read in shapefile
library(ggplot2)

com_boundary_init <- st_read("comarea/ComArea_ACS14_f.shp")

com_boundary_order <- com_boundary_init[order(com_boundary_init$ComAreaID),]

cb_geom <- com_boundary_order$geometry

# just plots the boundaries
plot(cb_geom)


plot(st_geometry(cb_geom), col = sf.colors(12, categorical = TRUE), border = 'dimgray', 
     axes = TRUE)

plot(st_geometry(st_centroid(cb_geom)), pch = 3, col = 'black', add = TRUE)





library(rgdal)
library(ggplot2)
library(dplyr)



shp <- readOGR(dsn = "comarea/ComArea_ACS14_f.shp")  



# map <- ggplot() + geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
# 
# map + theme_void()



# order the shp@data

shp@data$ComAreaID <- as.numeric(as.character(shp@data$ComAreaID))

shp@data <- shp@data[order(shp@data$ComAreaID),]

shp@data$ComAreaID <- as.character(shp@data$ComAreaID)



shp@data <- shp@data %>% mutate(probs = prob_post_means)

shp_df <- broom::tidy(shp, region = "ComAreaID")
shp_df <- shp_df %>% left_join(shp@data, by = c("id" = "ComAreaID"))

map <- ggplot() + geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, fill = probs), colour = "black") + theme_void()
map + scale_fill_continuous(name="Prior Probabilities\nof Inclusion")





shp@data <- shp@data %>% mutate(probs = prob_post_means, mips_prenatal = mips_mat[, 4])

shp_df <- broom::tidy(shp, region = "ComAreaID")
shp_df <- shp_df %>% left_join(shp@data, by = c("id" = "ComAreaID"))

map <- ggplot() + geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, fill = mips_prenatal), colour = "black") + theme_void()
map + scale_fill_continuous(name="MIP for\n1st Trimester \n Prenatal Care")



shp@data <- shp@data %>% mutate(probs = prob_post_means, mips_poverty = mips_mat[, 38])

shp_df <- broom::tidy(shp, region = "ComAreaID")
shp_df <- shp_df %>% left_join(shp@data, by = c("id" = "ComAreaID"))

map <- ggplot() + geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, fill = mips_poverty), colour = "black") + theme_void()
map + scale_fill_continuous(name="MIP for\nBelow 200% \n Poverty Line")


