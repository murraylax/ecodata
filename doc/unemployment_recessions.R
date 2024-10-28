## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  collapse = FALSE,
  comment = "#>",
  ft.align = "left",
  fig.width = 8,  # Width of the plots (in inches)
  fig.height = 4,  # Width of the plots (in inches)
  fig.retina = NULL  # Disable retina scaling
)
# library(gt)
library(flextable)
library(htmltools)

cat('
<style>
/* Force fixed layout on flextable */
table {
    table-layout: fixed !important;
    width: 100% !important;
}
td {
    overflow: hidden;  /* Prevent content from overflowing */
    text-overflow: ellipsis;  /* Optional: Add ellipsis for overflowed text */
}
</style>
')

## -----------------------------------------------------------------------------
library(tidyverse)
library(ecodata)

# Identify the data
data_sources <- c(
  "https://fred.stlouisfed.org/series/UNRATE",
  "https://fred.stlouisfed.org/series/WIUR",
  "https://fred.stlouisfed.org/series/CAUR"
)
variable_names <- c("U.S. Unemployment Rate", "Wisconsin Unemployment Rate", "California Unemployment Rate")

# Download the data
udata <- get_ecodata(data_sources, variable_names)

# Filter for only 2006-2013
udata <- udata |>
  filter(Date >= "2006-01-01", Date <= "2018-12-31")

# View the data
glimpse(udata)
ecodata_description_table(udata)

## -----------------------------------------------------------------------------
ggplot_ecodata_ts(udata, variables = "U.S. Unemployment Rate", plot.recessions = TRUE)

## ----fig.height=5-------------------------------------------------------------
ggplot_ecodata_ts(udata, plot.recessions = TRUE, title = "Unemployment Rate Comparisons")

## -----------------------------------------------------------------------------
all_states <- get_ecodata_allstates_fred("https://fred.stlouisfed.org/series/WIUR") 
all_states <- all_states |>
  filter(Date >= "2006-01-01", Date <= "2018-12-31")

## ----fig.height=6, fig.width=9------------------------------------------------
ggplot_ecodata_bar(all_states, 
                   title = "States with Highest Peak Unemployment Rates", 
                   plot_at = "largest", highest = 10)

## ----fig.height=6, fig.width=9------------------------------------------------
ggplot_ecodata_bar(all_states, 
                   title = "States with Lowest Peak Unemployment Rates", 
                   plot_at = "largest", lowest = 10)

