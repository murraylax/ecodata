% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ecodata.R
\name{get_ecodata_allstates_fred}
\alias{get_ecodata_allstates_fred}
\title{Get state-level FRED data for all U.S. states}
\usage{
get_ecodata_allstates_fred(
  varcode,
  frequency = NULL,
  units = NULL,
  recessions = FALSE
)
}
\arguments{
\item{varcode}{String for the variable code or URL to the data, for any one U.S. state}

\item{frequency}{Optional, string for what frequency to aggregate to. Valid only for FRED data. This parameter is passed to `fredr::fredr()`. The default is no aggregation.
- "d" - Daily
- "w" - Weekly
- "bw" - Biweekly
- "m" - Monthly
 - "q" - Quarterly
 - "sa" - Semiannual
 - "a" - Annual
 - "wem" - Weekly, ending Monday
 - "wetu" - Weekly, ending Tuesday
 - "wew" - Weekly, ending Wednesday
 - "weth" - Weekly, ending Thursday
 - "wef" - Weekly, ending Friday
 - "wesa" - Weekly, ending Saturday
 - "wesu" - Weekly, ending Sunday
 - "bwew" - Biweekly, ending Wednesday
 - "bwem" - Biweekly, ending Monday}

\item{units}{Optional, string indicating the data transformation to make when retrieving the data. This parameter is passed to `fredr::fredr()`. The default is no transformation.
- "lin" - Levels (No transformation)
- "chg" - Change
- "ch1" - Change from 1 year ago
- "pch" - Percent change
- "pc1" - Percent change from 1 year ago
- "pca" - Compounded annual rate of change
- "cch" - Continuously compounded rate of change
- "cca" - Continuously compounded annual rate of change
- "log" - Natural log}

\item{recessions}{Optional, logical for whether to include an NBER recession dummy variable. Default = FALSE.}
}
\value{
Data frame the variable requested for all U.S. states. The data frame will include a date variable and a column for every U.S. state. The data frame will also include all relevant meta data describing the data and citing its source.
}
\description{
Downloads data from FRED for all U.S. states, for a given variable code or URL.
The variable code or URL needs to be a state-specific variable, and be for just one of any of the U.S. States.
The function will retrieve the data for all U.S. states.
}
