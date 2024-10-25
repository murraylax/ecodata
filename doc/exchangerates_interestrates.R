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
  "https://fred.stlouisfed.org/series/IR3TIB01USM156N",
  "https://fred.stlouisfed.org/series/IR3TIB01GBM156N",
  "https://fred.stlouisfed.org/series/DEXUSUK"
)
variable_names <- c("Interest Rate United States", "Interest Rate United Kingdom", "USD / GBP Exchange Rate")

# Download the data
mydata <- get_ecodata(data_sources, variable_names)

# Filter only for 2022
mydata <- mydata |>
  filter(Date >= "2022-01-01", Date <= "2022-12-31")

# View the data
glimpse(mydata)
ecodata_description_table(mydata)

## -----------------------------------------------------------------------------
# Plot the interest rates
ggplot_ecodata_ts(mydata, 
                  variables = c("Interest Rate United States", "Interest Rate United Kingdom"),
                  title = "Interest Rates in the U.S. and U.K.")

## -----------------------------------------------------------------------------
# Plot the interest rates
ggplot_ecodata_ts(mydata, 
                  variables = "USD / GBP Exchange Rate",
                  title = "USD / GDP Exchange Rate")

