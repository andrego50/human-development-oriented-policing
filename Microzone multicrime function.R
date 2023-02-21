## Microzone multicrime

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Write the deparment name
departament <- 'TOLIMA'
# Write the municipalities names
municipalities <- c('IBAGUÉ', 'CAJAMARCA', 'PIEDRAS', 'ALVARADO')
# Import criminal data
data <- ''

# Run function
multicrime <- function(data, departament, municipalities) {
  
# Required Packages
my_packages <- c('ks', 'sf', 'mapview', 'dplyr', 'readr')
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]
if (length(not_installed)) install.packages(not_installed, dependencies = TRUE)
for (q in 1:length(my_packages)) {
  library(my_packages[q], character.only = TRUE)
}

# Load of the municipalities polygons
municipios <- st_read("mun_13_02_2018/Municipios.shp")
municipios <- st_as_sf(municipios, crs = 4326)
unidad <- municipios[municipios$NOM_DEPART == departament,]
unidad <- unidad[unidad$NOM_MUNICI %in% municipalities,]
unidad <- st_transform(unidad, 4326) 

grid <- st_make_grid(unidad, cellsize = 0.01/5)
grid_kde <- do.call(rbind, st_centroid(grid)) %>% 
  as_tibble()

# 

}
