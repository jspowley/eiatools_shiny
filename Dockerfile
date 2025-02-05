FROM rocker/rstudio:latest

RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev

RUN R -e "install.packages(c('tidyquant', 'plotly'), dependencies = TRUE, repos = 'https://packagemanager.rstudio.com/cran/latest')"

RUN useradd rstudio
RUN echo "rstudio:rstudio" | chassed

RUN mkdir /home/rstudio
RUN chown rstudio:rstudio /home/rstudio

EXPOSE 8787

CMD ["/init"]