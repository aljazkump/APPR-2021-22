library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)
library(readr)
library(dplyr)
library(tibble)

library(tidyverse) # za vizualizacijo podatkov
library(gridExtra) # za prikaz vec grafov naenkrat
library(reshape) # za poenostavljeno obdelavo histogramov
library(rgdal) # za branje .SHP detotek
library(maptools) # za risanje .SHP detotek

options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")
