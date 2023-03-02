## Microzone_multicrime function

multicrime <- function(data, departament, municipalities, grouped_crime, probs) {
  
  # Loading polygons of the municipalities
  municipios <- st_read("data/mun_13_02_2018/Municipios.shp")
  municipios <- st_as_sf(municipios, crs = 4326)
  unidad <- municipios[municipios$NOM_DEPART == departament,]
  unidad <- unidad[unidad$NOM_MUNICI %in% municipalities,]
  unidad <- st_transform(unidad, 4326) 
  grid <- st_make_grid(unidad, cellsize = 0.01/5)
  grid_kde <- do.call(rbind, st_centroid(grid)) %>% 
    as_tibble()
  
  # Insect point within polygons
  data_intersect <- data %>% 
    st_as_sf(coords = c('LONGITUD', 'LATITUD'), crs = 4326) %>%
    st_join(unidad, join = st_intersects) %>%
    as_tibble() %>%
    na.omit()
  
  # Kernel density estimation funtion
  microterritorio <- list()
  microterritorio_rural <- list()
  microterritorio_urbano <- list()
  
  for (i in 1:length(grouped_crime)) {
    groups <- grouped_crime[[i]]
    multicrimen_ubano <- subset(data_intersect, 
                                data_intersect$DESCRIPCION_CONDUCTA %in% groups &
                                  data_intersect$ZONA == 'URBANA')
    multicrimen_rural <- subset(data_intersect, 
                                data_intersect$DESCRIPCION_CONDUCTA %in% groups &
                                  data_intersect$ZONA == 'RURAL')
    
    kd_urbana <- kde(st_coordinates(multicrimen_ubano$geometry), 
                     Hpi(st_coordinates(multicrimen_ubano$geometry), 
                         pilot="dscalar"),
                     eval.points = grid_kde)
    kd_rural <- kde(st_coordinates(multicrimen_rural$geometry), 
                    Hpi(st_coordinates(multicrimen_rural$geometry), 
                        pilot="dscalar"),
                    eval.points = grid_kde)
    
    kde_urbana <- data.frame(x = kd_urbana[["eval.points"]][[1]], 
                             y = kd_urbana[["eval.points"]][[2]],
                             densidad = kd_urbana[["estimate"]])
    kde_rural <- data.frame(x = kd_rural[["eval.points"]][[1]], 
                            y = kd_rural[["eval.points"]][[2]],
                            densidad = kd_rural[["estimate"]])
    
    percentil_urbana <- quantile(kde_urbana$densidad, probs = probs)[[1]]
    percentil_rural <- quantile(kde_rural$densidad, probs = probs)[[1]]
    
    microterritorio_urbano[[i]] <- kde_urbana[kde_urbana$densidad >
                                                percentil_urbana,]
    microterritorio_rural[[i]] <- kde_rural[kde_rural$densidad > 
                                              percentil_rural,]
  }

write.csv(as.data.frame(do.call(cbind, microterritorio_urbano)), 
          'results/urban_microzone.csv')
write.csv(as.data.frame(do.call(cbind, microterritorio_rural)), 
          'results/rural_microzone.csv')
}
