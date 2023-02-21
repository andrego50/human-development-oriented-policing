## Microzone multicrime

departamento <- ''


# Required Packages
my_packages <- c('ks', 'sf', 'mapview', 'dplyr', 'readr')
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]
if (length(not_installed)) install.packages(not_installed, dependencies = TRUE)
for (q in 1:length(my_packages)) {
  library(my_packages[q], character.only = TRUE)
}

# Load of the municipalities polygons

