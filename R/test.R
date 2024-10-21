library(ecodata)
variables <- c(
  "https://fred.stlouisfed.org/series/GDPC1",
  "https://data.worldbank.org/indicator/GC.DOD.TOTL.GD.ZS?locations=US"
)

mydata <- get_ecodata(variables)
ggplot_ecodata_ts(mydata, "Real Gross Domestic Product") +
  geom_recession()

# Fix this, because it doesn't show a percent. Get rid of 'None'
ggplot_ecodata_ts(mydata, "United States Central government debt, total") +
  geom_recession()

wi <- get_ecodata("https://fred.stlouisfed.org/series/WIUR")
ggplot_ecodata_ts(wi) + geom_recession()

all <- get_ecodata_allstates_fred("https://fred.stlouisfed.org/series/WIUR")

# Find out what's going on with 'label' and 'variable' with this...
ecodata_description_table(mydata)

ecodata_cite_table(mydata)
