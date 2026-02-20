library(visor)
library(sf)

occluders <- st_read("data/buildings_100m_visibility.gpkg")

line <- st_read("data/viso_line_NS.gpkg")

vpoints <- get_viewpoints(line, density = 0.5)

isovist <- get_isovist(vpoints, occluders, ray_num = 200, ray_length = 5,
                       remove_holes = FALSE)

plot(isovist, col = "blue")
plot(occluders, col = "grey", add = TRUE)
plot(point, col = "lightblue", add = TRUE)
plot(viewpoints, col = "red", add = TRUE)