FROM rocker/rstudio

RUN R -e "install.packages('ks', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sf', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('readr', dependencies=TRUE, repos='http://cran.rstudio.com/')"

COPY . .
