## Globala inställningar för Shinyappen: digidel

# Ladda nödvändiga paket
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(ggiraph)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rdshinyappar)
library(leaflet)
library(sf)
library(writexl)

# hjälpfunktioner i R/ (Shiny laddar dem automatiskt, men vi gör det explicit så att ordningen blir tydlig)
for (fil in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(fil, encoding = "utf-8")

telemetry <- skapa_telemetry("digidel")

# Allmänna options - TRUE = visa inte R-felmeddelanden i appen,
# FALSE = visa felmeddelanden från R på webben
options(shiny.sanitize.errors = FALSE)
options(dplyr.summarise.inform = FALSE)

APP_TITEL <- "Digital delaktighet"

# Källrad längst ner i diagrammen
KALLA_DIGIDEL <- "Källa: Digidel-enkäten, bearbetning av Samhällsanalys, Region Dalarna"

# Kartans geometri förenklas med så här många meter när den läses in, så att kartan går snabbare.
# Sätt till NULL för att använda geometrin som den är.
digidel_forenkling_meter <- 200

# ---- 1. Läs in karta och statistik ----

lan_sf <- hamta_lan_karta(digidel_forenkling_meter)

digidel_data <- hamta_digidel_data(digidel_tabell)
digidel_exempeldata <- isTRUE(attr(digidel_data, "exempeldata"))

# En rad per besök (id), för kartans besöksantal och genomsnittlig tidsåtgång
digidel_besok <- digidel_data |>
  dplyr::distinct(id, lanskod, lansnamn, tid_min)

# Frågorna i den ordning de förekommer oftast, så de vanligaste hamnar överst i listrutan
digidel_fragor <- digidel_data |>
  dplyr::count(fraga, sort = TRUE) |>
  dplyr::pull(fraga)

# färgskala för kartan
digidel_kartpalett <- "Blues"
farg_vald_lan <- "#db3747"
