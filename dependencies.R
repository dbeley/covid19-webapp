dir.create(
  path = Sys.getenv("R_LIBS_USER"),
  showWarnings = FALSE,
  recursive = TRUE
)

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[, 1]))
    install.packages(p, lib = Sys.getenv("R_LIBS_USER"), repos = "https://cran.rstudio.com/")
  require(p, character.only = TRUE)
}

# Chargement des librairies ----
usePackage("devtools")
usePackage("tidyverse")
# devtools::install_version("ggplot2", version = "3.2.1", repos = "http://cran.us.r-project.org")
usePackage("lubridate")
usePackage("RCurl")
usePackage("leaflet")
# usePackage("plotly")
# usePackage("dplyr")

# Options globales ----
theme_set(theme_minimal() + theme(text = element_text(size = 16)))
#theme_set(theme_void())