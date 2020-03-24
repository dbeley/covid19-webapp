usePackage <- function(p) {
  if (!is.element(p, installed.packages()[, 1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackageGithub <- function(p, auteur) {
  if (!is.element(p, installed.packages()[, 1]))
    devtools::install_github(paste(auteur, p, sep = "/"))
  require(p, character.only = TRUE)
}

# Chargement des librairies ----
usePackage("devtools")
usePackage("tidyverse")
devtools::install_version("ggplot2", version = "3.2.1", repos = "http://cran.us.r-project.org")
usePackage("lubridate")
usePackage("dplyr")
usePackage("viridis")
# usePackage("scales")
usePackage("RCurl")
usePackage("plotly")
# usePackage("rworldmap")
usePackage("leaflet")
usePackage("htmltools")
# usePackage("shinydashboard")

# Options globales ----
theme_set(theme_minimal() + theme(text = element_text(size = 16)))
#theme_set(theme_void())