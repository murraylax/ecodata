## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  collapse = FALSE,
  comment = "#>",
  ft.align = "left",
  fig.width = 7,  # Width of the plots (in inches)
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

## ----eval = FALSE-------------------------------------------------------------
#  # install the devtools package, if necessary
#  install.packages("devtools")
#  
#  # Install the ecodata package
#  devtools::install_github("murraylax/ecodata")`

## ----eval = FALSE-------------------------------------------------------------
#  library(ecodata)

## ----eval = FALSE-------------------------------------------------------------
#  ecodata_set_fredkey("abcd1234efgh5678ijkl9012mnop3456")

## ----eval = FALSE-------------------------------------------------------------
#  fredr::fredr_set_key("abcd1234efgh5678ijkl9012mnop3456")

## -----------------------------------------------------------------------------
library(tidyverse)
library(ecodata)

## -----------------------------------------------------------------------------
gdp <- get_ecodata("https://fred.stlouisfed.org/series/GDPC1")

## ----echo = FALSE-------------------------------------------------------------
flextable(slice(gdp, 1:10)) |>
  flextable::width(width = 2)

## -----------------------------------------------------------------------------
ggplot_ecodata_ts(gdp)

## -----------------------------------------------------------------------------
ggplot_ecodata_ts(gdp, title = "Real GDP for the United States")

## -----------------------------------------------------------------------------
ggplot_ecodata_ts(gdp) + labs(title = "Real GDP for the United States")

## -----------------------------------------------------------------------------
# Include recession bars with a parameter
ggplot_ecodata_ts(gdp, title = "Real GDP for the United States", plot.recessions = TRUE)

## -----------------------------------------------------------------------------
# Include recession bars by adding a geom  
ggplot_ecodata_ts(gdp) + 
  labs(title = "Real GDP for the United States") +
  geom_recession()

## -----------------------------------------------------------------------------
gdp |>
  filter(Date >= "2000-01-01", Date <= "2023-12-31") |>
  ggplot_ecodata_ts(title = "Real GDP for the United States", plot.recessions = TRUE)

## -----------------------------------------------------------------------------
my_variables <- c(
  "https://data.worldbank.org/indicator/SI.DST.FRST.20?locations=US",
  "https://data.worldbank.org/indicator/SI.POV.DDAY?locations=US",
  "https://fred.stlouisfed.org/series/UNRATE"
)

## -----------------------------------------------------------------------------
mydata <- get_ecodata(my_variables)

## ----echo = FALSE-------------------------------------------------------------
flextable(head(mydata, 12)) |>
  flextable::width(width = 2)

## ----echo = FALSE-------------------------------------------------------------
flextable(slice(mydata, (nrow(mydata)-40):(nrow(mydata)-29) )) |>
  flextable::width(width = 2)

## -----------------------------------------------------------------------------
my_variables <- c(
  "https://data.worldbank.org/indicator/SI.DST.FRST.20?locations=US",
  "https://data.worldbank.org/indicator/SI.POV.DDAY?locations=US",
  "https://fred.stlouisfed.org/series/UNRATE"
)

variable_names <- c(
  "Income Share of Bottom 20%",
  "Poverty Rate",
  "Unemployent Rate"
)

mydata <- get_ecodata(my_variables, variable_names)

## ----echo = FALSE-------------------------------------------------------------
flextable(slice(mydata, (nrow(mydata)-40):(nrow(mydata)-29) )) |>
  flextable::width(width = 2)

## ----fig.height = 6, warning = FALSE------------------------------------------
ggplot_ecodata_ts(mydata, title = "Unemployment and Poverty in U.S.", plot.recessions = TRUE)

## ----fig.height = 6, warning = FALSE------------------------------------------
ggplot_ecodata_facet(mydata, 
                     title = "Unemployment and Poverty in U.S.", 
                     ncol = 1, 
                     plot.recessions = TRUE)

## -----------------------------------------------------------------------------
ggplot_ecodata_facet(mydata, 
                     variables = c("Income Share of Bottom 20%", "Poverty Rate"),
                     title = "Income and Poverty in U.S.", 
                     ncol = 1, 
                     plot.recessions = TRUE)

## -----------------------------------------------------------------------------
mydata |>
  select(Date, `Income Share of Bottom 20%`, `Poverty Rate`) |>
  ggplot_ecodata_facet(title = "Income and Poverty in the U.S.", ncol = 1, plot.recessions = TRUE)

## -----------------------------------------------------------------------------
allstates <- get_ecodata_allstates_fred("https://fred.stlouisfed.org/series/CAUR")

## -----------------------------------------------------------------------------
glimpse(allstates)

## -----------------------------------------------------------------------------
allcountries <- get_ecodata_allcountries_wb("https://data.worldbank.org/indicator/SI.POV.DDAY?locations=US")

## -----------------------------------------------------------------------------
glimpse(allcountries)

## ----fig.height=5, fig.width=8------------------------------------------------
ggplot_ecodata_bar(allcountries, 
                   plot_at = "last", 
                   highest = 10, 
                   title = "Countries with Highest Poverty Rates")

## ----fig.height=5, fig.width=8------------------------------------------------
ggplot_ecodata_bar(allcountries, 
                   plot_at = "last", 
                   highest = 10, 
                   title = "Countries with Highest Poverty Rates",
                   highlight = "Zambia")

## -----------------------------------------------------------------------------
variables <- c("https://fred.stlouisfed.org/series/GDPC1",
               "https://fred.stlouisfed.org/series/GDPPOT",
               "https://fred.stlouisfed.org/series/CPIAUCSL")

varnames <- c("Real GDP", "Potential GDP", "CPI")

mydata <- get_ecodata(variables, varnames = varnames)

## -----------------------------------------------------------------------------
mydata <- ecodata_compute_pctchange(mydata, variable = "CPI", new_variable = "Inflation")

## -----------------------------------------------------------------------------
ggplot_ecodata_ts(mydata, variables = "Inflation", title = "United States Inflation Rate", plot.recessions = TRUE)

## -----------------------------------------------------------------------------
mydata <- mydata |> 
  mutate(`Output Gap` = (`Real GDP` - `Potential GDP`) / `Potential GDP` * 100, units = "Percent")

## -----------------------------------------------------------------------------
ggplot_ecodata_ts(mydata, variables = "Output Gap", title = "Output Gap", plot.recessions = TRUE)

## -----------------------------------------------------------------------------
ecodata_cite_table(mydata)

## ----eval = FALSE-------------------------------------------------------------
#  ecodata_description_table(mydata)

## ----echo = FALSE-------------------------------------------------------------
ft <- ecodata_description_table(mydata) 
htmltools::tags$div(
  style = "overflow-x: auto; width: 100%; max-width: 100%;",
  htmltools_value(ft)
)

## ----warning = FALSE, message = FALSE-----------------------------------------
# Line 1: Download data
intrates <- get_ecodata(c("https://fred.stlouisfed.org/series/DGS1", 
                          "https://fred.stlouisfed.org/series/DGS10"), frequency = "m")

## ----warning = FALSE, message = FALSE, fig.width=9----------------------------
# Line 2: Make a time series plot
ggplot_ecodata_ts(intrates, title = "Interest Rates on U.S. Treasuries", plot.recessions = TRUE)

## ----warning = FALSE, message = FALSE-----------------------------------------
# Line 3: Cite my sources
ecodata_cite_table(intrates)

