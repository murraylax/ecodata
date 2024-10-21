# ecodata
R package for downloading and visualizing data from FRED and World Bank

Copyright (C) 2004 James M. Murray

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

## Installation

The package is available from https://github.com/murraylax/ecodata. Use the `devtools` package to install directly in R:

`devtools::install_github("murraylax/ecodata")

## Set up

Load the library:

`library(ecodata)`

The package uses the FRED (Federal Reserve Economic Data) API, which requires an API key. If you have not already done so, you must create an account at https://fred.stlouisfed.org/, login, then go to https://fredaccount.stlouisfed.org/apikeys to create an API key. Click the button, `+ Request API Key` and follow instructions. When complete, you will have an API key that will be a 32 character string with letters and numbers, similar to 'abcd1234efgh5678ijkl9012mnop3456' (this is a fictional key, it will not work to use exactly this string).

Set up the API key to use in the `ecodata` package (and/or the `fredr` package) by entering the following in the R console (replace the string with your own key):

`ecodata_set_fredkey("abcd1234efgh5678ijkl9012mnop3456")`

## Get started

You can find real GDP data for the United States on FRED here: https://fred.stlouisfed.org/series/GDPC1

You can download it and load into R with the following call:

`mydata <- get_ecodata("https://fred.stlouisfed.org/series/GDPC1")`

You can plot the data with the following call:

`ggplot_ecodata_ts(mydata)`
