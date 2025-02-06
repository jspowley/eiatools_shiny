FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    git

RUN R -e "install.packages(c('devtools', 'shiny', 'bslib', 'tidyverse', 'eiatools', 'DT', 'rlang', 'rhandsontable'), dependencies = TRUE, repos = 'https://packagemanager.rstudio.com/cran/latest')"
RUN R -e "devtools::install_github("https://github.com/jspowley/eiatools")"

RUN git clone https://github.com/jspowley/eiatools_shiny.git /srv/shiny-server/eiatools_shiny
RUN chown -R shiny:shiny /srv/shiny-server/eiatools_shiny

EXPOSE 3838

#CMD ["/init"]
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/eiatools_shiny/', host = '0.0.0.0', port = 3838)"]
