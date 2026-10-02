## Globala inställningar för Shinyappen: digidel

# Ladda nödvändiga paket
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(ggiraph)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(rdshinyappar)

telemetry <- skapa_telemetry("digidel")

# Allmänna options - TRUE = visa inte R-felmeddelanden i appen,
# FALSE = visa felmeddelanden från R på webben
options(shiny.sanitize.errors = FALSE)
