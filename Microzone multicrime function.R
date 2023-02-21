## Microzone multicrime

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Write the deparment name
departament <- 'TOLIMA'

# Write the municipalities names
municipalities <- c('IBAGUÉ', 'CAJAMARCA', 'PIEDRAS', 'ALVARADO')

# Import criminal data
data <- read_csv("delitos.csv", locale = locale(decimal_mark = ","))

# Grouped crimes
homicidio <- c("ARTÍCULO 103. HOMICIDIO", "ARTÍCULO 104A. FEMINICIDIO")

lesiones <- c("ARTÍCULO 111. LESIONES PERSONALES", "ARTICULO 120 LESIONES CULPOSAS", "ARTÍCULO 119. LESIONES PERSONALES ( CIRCUNSTANCIAS DE AGRAVACIÓN)",
              "ARTÍCULO 113. DEFORMIDAD", "ARTÍCULO 116. PÉRDIDA ANATÓMICA O FUNCIONAL DE UN ÓRGANO O MIEMBRO", "ARTÍCULO 229. VIOLENCIA INTRAFAMILIAR")

sexual <- c("ARTÍCULO 206. ACTO SEXUAL VIOLENTO", "ARTÍCULO 205. ACCESO CARNAL VIOLENTO", "ARTÍCULO 210. ACCESO CARNAL O ACTO SEXUAL ABUSIVO CON INCAPAZ DE RESISTIR",
            "ARTÍCULO 210 A. ACOSO SEXUAL", "ARTÍCULO 207. ACCESO CARNAL O ACTO SEXUAL EN PERSONA PUESTA EN INCAPACIDAD DE RESISTIR", "ARTÍCULO 213. INDUCCIÓN A LA PROSTITUCIÓN",
            "ARTÍCULO 214. CONSTREÑIMIENTO A LA PROSTITUCIÓN")

sexual_nna <- c("ARTÍCULO 208. ACCESO CARNAL ABUSIVO CON MENOR DE 14 AÑOS", "ARTÍCULO 209. ACTOS SEXUALES CON MENOR DE 14 AÑOS",
                "ARTÍCULO 218. PORNOGRAFÍA CON MENORES", "ARTÍCULO 219 A. UTILIZACIÓN O FACILITACIÓN DE MEDIOS DE COMUNICACIÓN PARA OFRECER SERVICIOS SEXUALES DE MENORES",
                "ARTÍCULO 217 A. DEMANDA DE EXPLOTACION SEXUAL COMERCIAL DE PERSONA MENOR DE 18 AÑOS DE EDAD", "ARTÍCULO 213 A. PROXENETISMO CON MENOR DE EDAD",
                "ARTÍCULO 126. LESIONE CULPOSAS AL FETO", "ARTÍCULO 217. ESTÍMULO A LA PROSTITUCIÓN DE MENORES")

extorsion <- c("ARTÍCULO 244. EXTORSIÓN")

hurto <- c("ARTÍCULO 239. HURTO PERSONAS", "ARTÍCULO 239. HURTO ENTIDADES COMERCIALES", "ARTÍCULO 239. HURTO RESIDENCIAS",
           "ARTÍCULO 239. HURTO AUTOMOTORES", "ARTÍCULO 239. HURTO PIRATERÍA TERRESTRE", "ARTÍCULO 239. HURTO ENTIDADES FINANCIERAS", 
           "ARTÍCULO 243. ABIGEATO")

# Make object of grouped crime
grouped_crime <- list(homicidio, lesiones, sexual, sexual_nna, extorsion, hurto)

# Run function
multicrime <- function(data, departament, municipalities, grouped_crime) {
  
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

# Insect point within polygons
data_intersect <- data %>% 
  st_as_sf(coords = c('LONGITUD', 'LATITUD'), crs = 4326) %>%
  st_join(unidad, join = st_intersects) %>%
  as_tibble() %>%
  na.omit()

# Kernel density estimation
microterritorio <- NULL

for (i in length(grouped_crime)) {
  grupos <- grouped_crime[[i]]
  multicrimen_ubano <- subset(data_intersect, 
                              data_intersect$DESCRIPCION_CONDUCTA %in% grupos &
                                data_intersect$ZONA == 'URBANA')
  multicrimen_rural <- subset(data_intersect, 
                              data_intersect$DESCRIPCION_CONDUCTA %in% grupos &
                                data_intersect$ZONA == 'RURAL')
  
  coord_urbana <- multicrimen_ubano[,c('LONGITUD','LATITUD')]
  coord_rural  <- multicrimen_rural[,c('LONGITUD','LATITUD')]
  
  kd_urbana <- kde(coord_urbana, Hpi(coord_urbana, pilot="dscalar"),
                   eval.points = grid_kde)
  kd_rural <- kde(coord_rural, Hpi(coord_rural, pilot="dscalar"),
                  eval.points = grid_kde)
  
  kde_urbana <- data.frame(x = kd_urbana[["eval.points"]][[1]], 
                           y = kd_urbana[["eval.points"]][[2]],
                           densidad = kd_urbana[["estimate"]])
  kde_rural <- data.frame(x = kd_rural[["eval.points"]][[1]], 
                          y = kd_rural[["eval.points"]][[2]],
                          densidad = kd_rural[["estimate"]])
  
  percentil_urbana <- quantile(kde_urbana$densidad, probs = probs)[[1]]
  percentil_rural <- quantile(kde_rural$densidad, probs = probs)[[1]]
  
  microterritorio_urbano <- kde_urbana[kde_urbana$densidad >
                                         percentil_urbana,]
  microterritorio_rural <- kde_rural[kde_rural$densidad > 
                                       percentil_rural,]
  
  microterritorio_urbano <- st_as_sf(microterritorio_urbano,
                                     coords = c('x','y'), 
                                     crs = 4326)
  microterritorio_rural <- st_as_sf(microterritorio_rural,
                                    coords = c('x','y'), 
                                    crs = 4326)
  microterritorio[[i]] <- list(rural = microterritorio_rural, 
                               urbano = microterritorio_urbano)
}

}

multicrime()
probs = 0.999