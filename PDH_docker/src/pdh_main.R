# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Required Packages
my_packages <- c('ks', 'sf', 'dplyr', 'readr')
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]
if (length(not_installed)) install.packages(not_installed, dependencies = TRUE)
for (q in 1:length(my_packages)) {
  library(my_packages[q], character.only = TRUE)
}

# Microzone multicrime
source('microzone_multicrime_function.R')

# Import criminal data
data <- read_csv("data/delitos.csv", locale = locale(decimal_mark = ","))

# Write the deparment name
departament <- 'TOLIMA'

# Write the municipalities names
municipalities <- c('IBAGUÉ', 'CAJAMARCA', 'PIEDRAS', 'ALVARADO')

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

multicrime(data = data, 
           departament = departament, 
           municipalities = municipalities, 
           grouped_crime = grouped_crime, 
           probs = 0.999)

