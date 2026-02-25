library(visor)
library(sf)

occluders <- st_read("data/buildings_visor.shp")

#occluders <- st_simplify(occluders, dTolerance = 0.1)


# print(class(occluders))
# print(st_geometry_type(occluders))
# print(st_is_valid(occluders))
# print(st_crs(occluders))
# dput(head(occluders))

line <- st_read("data/viso_line_EW.gpkg")

vpoints <- get_viewpoints(line, density = 1)

# point <- st_read("data/central_point_citytheater.gpkg")



# vpoints_matrix <- st_coordinates(vpoints)

print(class(vpoints_matrix))
print(head(vpoints_matrix))
print(st_crs(vpoints))

isovist <- get_isovist(vpoints, occluders, ray_num = 40, ray_length = 180,
                       remove_holes = FALSE)

plot(isovist, col = "blue")
plot(occluders, col = "grey", add = TRUE)
plot(line, col = "lightblue", add = TRUE)
plot(vpoints, col = "red", add = TRUE)
