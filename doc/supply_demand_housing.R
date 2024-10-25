## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  collapse = FALSE,
  comment = "#>",
  ft.align = "left",
  fig.width = 10,  # Width of the plots (in inches)
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
  "https://fred.stlouisfed.org/series/MSPNHSUS",
  "https://fred.stlouisfed.org/series/NHSUSSPT",
  "https://fred.stlouisfed.org/series/WPUIP2311001"
)
variable_names <- c("Median Sales Price", "Number of Sales", "Construction Cost Index")

# Download the data
housingdata <- get_ecodata(data_sources, variable_names)

# Filter for only 2021-2022
housingdata <- housingdata |>
  filter(Date >= "2021-01-01", Date <= "2022-12-31")

# View the data
glimpse(housingdata)
ecodata_description_table(housingdata)

## -----------------------------------------------------------------------------
# Housing Prices
ggplot_ecodata_ts(housingdata,
                  variables = "Median Sales Price", 
                  title = "Median Sales Price of New Houses in the U.S.", 
                  ylab = "") 

## -----------------------------------------------------------------------------
# Quantity of Sales
ggplot_ecodata_ts(housingdata, 
                  variables = "Number of Sales", 
                  title = "Number of New Houses Sold in the U.S.")

## -----------------------------------------------------------------------------
# Construction Costs
ggplot_ecodata_ts(housingdata, 
                  variables = "Construction Cost Index", 
                  title = "Construction Costs for New Houses in the U.S.")

