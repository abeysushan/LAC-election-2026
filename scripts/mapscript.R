# Kerala LAC election map

# Load libraries

library(tidyverse)
library(googlesheets4)
library(janitor)
library(sf)
library(leaflet)
library(htmltools)
library(htmlwidgets)

# Load data

sheets_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQU23ns7jeJxqs93ivRVlTTSOss6sKFdclWC0Wajc7mrbJIhnITnLgrUpTzXVFuSq4ikfzlwzJUL2I0/pub?gid=513083881&single=true&output=csv"

results <- read_csv(sheets_url)

# Load map shapefile

map <- st_read("data/KeralaLAC.geojson")

# Join map and results

map_results <- map |>
  left_join(results, by = "Asmbly_Con")

# Generate map

ggplot(map_results) +
  geom_sf(aes(fill = Win), color = "black", size = 0.1) +
  scale_fill_manual(values = c(
    "LDF" = "red",
    "UDF" = "lightblue",
    "NDA" = "#F1C338",
    "OTH" = "grey",
    "NA" = "white"
  )) +
  theme_void()

# Generate html map

m <- leaflet(map_results, height = "90vh") |>
  addTiles() |>
  addPolygons(
    fillColor = ~ case_when(
      Win == "LDF" ~ "red",
      Win == "UDF" ~ "lightblue",
      Win == "NDA" ~ "#F1C338",
      Win == "OTH" ~ "grey",
      TRUE ~ "white"
    ),
    color = "black",
    weight = 0.8,
    fillOpacity = 0.7,
    popup = ~ paste0(
      "<strong>", Asmbly_Con, "</strong><br>",
      "Winner: ", Win
    )
  )

# Add title and footer

m <- m |>
  addControl(
    "<h4>Kerala LAC Election Results 2026</h4>",
    position = "topright"
  ) |>
  addControl(
    "<em>Data source: @abeysushan</em>",
    position = "bottomright"
  ) |>
  addControl(
    paste("Last updated:", Sys.time()),
    position = "bottomleft"
  )

# Style page

# Add title + footer
page <- tagList(
  tags$h2("Kerala Assembly Election Results 2026",
    style = "text-align:left; font-family:sans-serif;"
  ),
  m
)

# Save
htmltools::save_html(page, "index.html")
