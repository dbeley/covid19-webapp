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

set_wd <- function() {
  usePackage("rstudioapi") # make sure you have it installed
  current_path <- getActiveDocumentContext()$path
  setwd(dirname(current_path))
  print(getwd())
}

if (.Platform$OS.type == "unix")
  set_wd()
if (.Platform$OS.type == "windows") {
  set_wd()
  usePackage("installr")
  updater()
  Sys.getenv("R_ZIPCMD", "zip")
}

# Chargement des librairies ----
usePackage("readxl")
usePackage("tidyverse")
usePackage("devtools")
usePackage("lubridate")
usePackage("dplyr")
# dépendances supplémentaires : udunits + gdal
usePackage("gganimate")
# usePackageGithub("gganimate", "thomasp85")
usePackageGithub("patchwork", "thomasp85")
usePackage("viridis")
usePackage("scales")
usePackage("RCurl")
usePackage("plotly")

usePackage("rworldmap")
# usePackage("ggmap")
usePackage("leaflet")
# usePackage("rnaturalearth")
# usePackage("rnaturalearthdata")
# usePackage("sf")
usePackage("htmltools")

# Options globales ----
theme_set(theme_minimal() + theme(text = element_text(size = 20)))
#theme_set(theme_void())