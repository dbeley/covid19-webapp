# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 
  

# install R packages required 
# (change it dependeing on the packages you need)
RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lubridate', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinythemes', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RCurl', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', repos='http://cran.rstudio.com/')"

# copy the app to the image
COPY covid19-webapp.Rproj /srv/shiny-server/
COPY app.R /srv/shiny-server/
COPY data.R /srv/shiny-server/
COPY dependencies.R /srv/shiny-server/
COPY reactive_components.R /srv/shiny-server/

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
CMD ["/usr/bin/shiny-server.sh"]
