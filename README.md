# ECODATA
R package for downloading and visualizing data from FRED and World Bank

Copyright (C) 2024 James M. Murray

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

The package is available from <a href="https://github.com/murraylax/ecodata" target="_blank">https://github.com/murraylax/ecodata</a>. Use the `devtools` package to install directly in R:

`devtools::install_github("murraylax/ecodata")`

## Set up

Load the library:

`library(ecodata)`

The package uses the FRED (Federal Reserve Economic Data) API, which requires an API key. If you have not already done so, you must create an account at https://fred.stlouisfed.org/, login, then go to https://fredaccount.stlouisfed.org/apikeys to create an API key. Click the button, `+ Request API Key` and follow instructions. When complete, you will have an API key that will be a 32 character string with letters and numbers, similar to 'abcd1234efgh5678ijkl9012mnop3456' (this is a fictional key, it will not work to use exactly this string).

Set up the API key to use in the `ecodata` package (and/or the `fredr` package) by entering the following in the R console (replace the string with your own key):

`ecodata_set_fredkey("abcd1234efgh5678ijkl9012mnop3456")`

You should only need to do the above once per machine, then the FRED API should be available every time you load R.

## Get started

  - Vignette: <a href="https://murraylax.org/ecodata/getting-started.html" target="_blank">Getting started with the ECODATA package</a>
  
  - Browse vingettes: <a href="https://murraylax.org/ecodata/table_of_contents.html" target="_blank">Table of contents</a>
  
  - <a href="https://murraylax.org/ecodata/manual/" target="_blank">Reference Manual</a>
