FROM rocker/rstudio:latest

RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev

RUN R -e "install.packages(c('shiny', 'bslib', 'tidyverse', 'eiatools', 'DT', 'rlang', 'rhandsontable'), dependencies = TRUE, repos = 'https://packagemanager.rstudio.com/cran/latest')"
RUN git clone https://github.com/jspowley/eiatools_shiny.git /srv/shiny-server/shiny-stocks

RUN echo "rstudio:rstudio" | chpasswd
RUN chown rstudio:rstudio /home/rstudio

EXPOSE 8787

CMD ["/init"]