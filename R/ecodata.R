# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Get a glimpse of ecodata
#'
#' A override of dplyr's glimpse() that also prints the label for the dataframe
#' Just prints information about the dataframe
#'
#' @param x An ecodata data frame, the return value from `get_ecodata()`
#' @seealso \code{\link[dplyr]{glimpse}}
#' @export
glimpse <- function(x) {
  cat(attr(x, "label"))
  cat("\n")
  dplyr::glimpse(x)
}

#' `filter()`
#'
#' A wrapper for `dplyr::filter()`.
#' @seealso [dplyr::filter()]
filter <- function(...) {
  dplyr::filter(...)
}

#' `str_detect()`
#' Wrapper for `stringr::str_detect()`
#' @seealso [stringr::str_detect()]
str_detect <- function(...) {
  stringr::str_detect(...)
}

#' `rename()`
#' Wrapper for `dplyr::rename()`
#' @seealso [dplyr::rename()]
rename <- function(...) {
  dplyr::rename(...)
}

#' `str_replace()`
#' Wrapper for `stringr::str_replace()`
#' @seealso [stringr::str_replace()]
str_replace <- function(...) {
  stringr::str_replace(...)
}

#' `str_to_lower()`
#' Wrapper for `stringr::str_to_lower()`
#' @seealso [str_to_lower()]
str_to_lower <- function(...) {
  stringr::str_to_lower(...)
}

#' `ecodata_theme()`
#' A ggplot theme that is simple and clean and has a large text size
#' @return Returns a ggplot theme that can be added to a `ggplot()` call
#' @export
ecodata_theme <- function() {
  rettheme <- ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = 22)) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"))

  return(rettheme)
}

#' `ecodata_linear_interpolate()`
#' Perform a linear interpolation for any missing values for an ecodata data frame. This is useful for plotting time series with mixed frequencies.
#' @param df An ecodata data frame
#' @param variable_names Optional, vector of strings for the names of the variables in `df` to perform the linear interpolation. Default is to perform the operation on all the variables.
#' @return An ecodata data frame with the same structure as `df`, with NA values replaced with linear interpolations.
ecodata_linear_interpolate <- function(df, variable_names = NULL) {
  if(is.null(variable_names)) {
    variable_names <- get_ecodata_varnames(df)
  }

  df <- dplyr::arrange(df, Date)

  for(varname in variable_names) {
    # Save original attributes
    original_attributes <- attributes(df[[varname]])

    # Interpolate
    df[[varname]] <- zoo::na.approx(df[[varname]], x = df$Date, na.rm = FALSE)

    # Re-apply attributes
    attributes(df[[varname]]) <- original_attributes
  }

  return(df)
}

#' `string_compare()`
#' Compare two strings, ignoring case and whitespace
#' @param str1 String to compare
#' @param str2 Another string to compare
#' @return Boolen on whether the strings match
string_compare <- function(str1, str2) {
  return(
    stringr::str_squish(stringr::str_to_lower(str1)) == stringr::str_squish(stringr::str_to_lower(str2))
  )
}

#' `string_detect()`
#' Detect a pattern in a string, ignoring case
#' @param str String to search
#' @param pattern Pattern to look for
#' @return Logical indicating whether or not the pattern was found
string_detect <- function(str, pattern) {
  return(
    stringr::str_detect(
      stringr::str_to_lower(str), stringr::str_to_lower(pattern)
    )
  )
}

#' `str_extract()`
#' Wrapper for `stringr::str_extract()`
#' @seealso [stringr::str_extract()]
str_extract <- function(...) {
  stringr::str_extract(...)
}

#' `string_detect_any()`
#' Discover whether any of the strings in `str` contain `pattern`
#' @param str Vector of strings to search through
#' @param pattern Pattern to search for
#' @return Logical on whether the pattern was found in any of the strings
string_detect_any <- function(str, pattern) {
  return(any(string_detect(str, pattern)))
}

#' `string_which()`
#' Wrapper for `str_which()` while ignoring case, returns what index into the string the pattern is found
#' @param str String or vector of strings to search through
#' @param pattern Pattern to look for
#' @return Integer index into the string that the pattern was found
string_which <- function(str, pattern) {
  return(
    stringr::str_which(
      stringr::str_to_lower(str), stringr::str_to_lower(pattern)
    )
  )
}

#' `is_valid_url()`
#' Test whether a string is a valid URL to an actual web address
#' @param url String, that is maybe a URL
#' @return Logical, whether or not the string was actually a valid URL
is_valid_url <- function(url) {
  response <- tryCatch({
    httr::HEAD(url)
  }, error = function(e) {
    return(NULL)
  })
  retval <- FALSE
  if(!is.null(response)) {
    retval <- httr::status_code(response) < 400
  }
  return(retval)
}

#' `get_varcode_url_fred()`
#' Extract a variable code from a FRED URL
#' @param url String that is a URL to a FRED data series
#' @return String that is the variable code
#' @export
get_varcode_url_fred <- function(url) {
  url <- stringr::str_to_lower(url)
  if(str_detect(url, "fred.stlouisfed.org")) {
    varcode <- str_extract(url, "(?<=series/).*")
  } else {
    error_code <- sprintf("Failed to find series: %s", url)
    stop(error_code)
  }
  return(varcode)
}

#' `get_varcode_url_worldbank()`
#' Extract a variable code from a Workbank URL
#' @param url String that is a URL to a World Bank data series
#' @return String that is the variable code
#' @export
get_varcode_url_worldbank <- function(url) {
  url <- stringr::str_to_lower(url)
  if(stringr::str_detect(url, "data.worldbank.org")) {
    varcode <- str_extract(url, "(?<=indicator/).*")
  } else {
    error_code <- sprintf("Failed to find series: %s", url)
    stop(error_code)
  }
  return(varcode)
}

#' `get_varcode_url()`
#' Extract a variable code from a FRED or Workbank URL
#' @param url String that is a URL to a FRED or World Bank data series
#' @return String that is the variable code
#' @export
get_varcode_url <- function(url) {
  url <- stringr::str_to_lower(url)
  if(stringr::str_detect(url, "data.worldbank.org")) {
    varcode <- get_varcode_url_worldbank(url)
  } else if(stringr::str_detect(url, "fred.stlouisfed.org")) {
    varcode <- get_varcode_url_fred(url)
  } else {
    error_code <- sprintf("Failed to find series: %s", url)
    stop(error_code)
  }
  return(varcode)
}

#' `get_state_fips_all()`
#' Get a data frame with all the U.S. states and their FIPS codes. Data comes from `usmaps::fips_info()`
#' @return A data frame with all the U.S. states and their FIPS codes
#' @seealso [usmaps::fips_info()]
#' @export
get_state_fips_all <- function() {
  # fips_codes <- usmap::fips_info()
  # fips_codes <- dplyr::rename(fips_codes, State = full, FIPS = fips, Abbr = abbr)
  # return(fips_codes)

  # Comment this out
  # load("./data/fips_codes.RData")
  data(fips_codes)
  return(fips_codes.df)
}

#' `get_state_fips(state)`
#' Get the FIPS code for a given state
#' @param state String that is either the full name of the state or the two-letter abbreviation
#' @return String that is the FIPS code for the state
#' @export
get_state_fips <- function(state) {
  fips.df <- get_state_fips_all()
  state_fips.df <- dplyr::filter(fips.df, string_compare(Abbr, state))
  if(nrow(state_fips.df) == 0) {
    state_fips.df <- dplyr::filter(fips.df, string_compare(State, state))
  }
  if(nrow(state_fips.df) == 0) {
    warning_message <- sprintf("No state found matching %s", state)
    warning(warning_message)
  }
  return(state_fips.df$FIPS[1])
}

#' `get_state_name(state)`
#' Get the state name for the given abbreviation or FIPS code
#' @param state String that is a FIPS code or two-letter abbreviation
#' @return String that is the name of the state
#' @export
get_state_name <- function(state) {
  fips.df <- get_state_fips_all()
  state_fips.df <- filter(fips.df, string_compare(Abbr, state))
  if(nrow(state_fips.df) == 0) {
    state_fips.df <- filter(fips.df, string_compare(FIPS, state))
  }
  if(nrow(state_fips.df) == 0) {
    warning_message <- sprintf("No state found matching %s", state)
    warning(warning_message)
  }
  return(state_fips.df$State[1])
}

#' `get_state_abbr(state)`
#' Get the two-letter abbreviation for a state, given the name of the state or the FIPS code
#' @param state String that is a name of a state or the FIPS code
#' @return String that is the two-letter abbreviation for the state
#' @export
get_state_abbr <- function(state) {
  fips.df <- get_state_fips_all()
  state_fips.df <- dplyr::filter(fips.df, string_compare(State, state))
  if(nrow(state_fips.df) == 0) {
    state_fips.df <- dplyr::filter(fips.df, string_compare(FIPS, state))
  }
  if(nrow(state_fips.df) == 0) {
    warning_message <- sprintf("No state found matching %s", state)
    warning(warning_message)
  }
  return(state_fips.df$Abbr[1])
}

#' `geom_recession()`
#' Return a geom_rect() that identifies recessions in a time series plot
#' @param data Data frame
#' @param alpha Numeric transparency parameter. Default = 0.2.
#' @param fill String identifying the fill color, default is "dodgerblue3"
#' @return A geom_rect() that can be added to a ggplot time series plot
#' @export
geom_recession <- function(data = NULL, stat = "identity",
                           position = "identity", ...,
                           alpha = 0.2, fill = "dodgerblue3",
                           show.legend = NA,
                           inherit.aes = TRUE) {
  layer <- ggplot2::layer(
    geom = GeomRecession, mapping = ggplot2::aes(), data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(alpha = alpha, fill = fill, ...)
  )

  class(layer) <- c("GeomRecessionLayer", class(layer))
  return(layer)
}

#' Definition of the GeomRecession
GeomRecession <- ggplot2::ggproto("GeomRecession", ggplot2::Geom,

  draw_panel = function(self, data, panel_params, coord, alpha, fill) {
    mindate <- min(data$x)
    maxdate <- max(data$x)
    linewidth = 0.0

    # Comment this out later
    # load("./data/recessions.RData")

    data(recessions)
    recession_data <- recession_data |>
      dplyr::filter(Trough >= mindate & Peak <= maxdate)

    # If a recession started before the start of the dataset, set the beginning to the start of the dataset
    if(min(recession_data$Peak) <= mindate) {
      recession_data$Peak[recession_data$Peak <= mindate] <- mindate
    }
    # If a recession ended before the end of the dataset, set the end to the end of the dataset
    if(max(recession_data$Trough) >= maxdate) {
      recession_data$Trough[recession_data$Trough >= maxdate] <- maxdate
    }

    recession_data <- recession_data |>
      dplyr::rename(xmin = Peak, xmax = Trough) |>
      dplyr::mutate(ymin = -Inf, ymax = Inf,
        alpha = alpha, fill = fill, colour = fill, linewidth = linewidth, linetype = 1)

    ggplot2::GeomRect$draw_panel(recession_data, panel_params, coord)  # Use geom_rect to draw the rectangles
  },

  # Override setup_layer to ensure the layer is moved to the back
  setup_layer = function(self, data, plot) {
    plot <- ggplot2::ggplot_build(plot)
    recession_layer <- which(sapply(plot$plot$layers, function(x) inherits(x$geom, "GeomRecession")))

    if (length(recession_layer) > 0) {
      # Reorder layers: move recession layer to the beginning (back)
      plot$plot$layers <- append(plot$plot$layers[-recession_layer], plot$plot$layers[recession_layer], after = 0)
    }

    return(plot)
  },

  draw_key = ggplot2::draw_key_rect  # Use the default rectangle key in legends
)

#' 'ggplot_add.GeomRecessionLayer()'
#' Define a ggplot_add method to automatically move the recession layer to the back
ggplot_add.GeomRecessionLayer <- function(object, plot, object_name) {
  # Add the layer to the plot
  plot <- NextMethod()

  # Find the recession layers
  recession_layers <- which(sapply(plot$layers, function(layer) inherits(layer$geom, "GeomRecession")))

  if (length(recession_layers) > 0) {
    # Move recession layers to the back
    plot$layers <- c(plot$layers[recession_layers], plot$layers[-recession_layers])
  }

  return(plot)
}

#' `get_recessions(data)`
#' Get NBER recession dates that line up with data in the data frame
#' @param data Data frame with a time series
#' @return A copy of the data frame, `data`, with a new variable `Recession` equal to TRUE or FALSE if there was a recession in the time period
#' @export
get_recessions <- function(data) {
  first_date <- min(data$Date)
  last_date <- max(data$Date)
  nber_recessions_code <- "USRECDM"
  raw.recessions.df <- fredr::fredr(nber_recessions_code)
  recessions.df <- tidyr::tibble(Date = raw.recessions.df$date, Recession = raw.recessions.df$value)
  recessions.df <- dplyr::filter(recessions.df, Date >= first_date, Date <= last_date)
  if(string_detect_any(names(data), "Recession")) {
    # Get rid of the current recession variable if it's there
    idx <- string_which(names(data), "Recession")
    data <- data[,-idx]
  }
  data <- dplyr::left_join(data, recessions.df, by = "Date")
}

#' `get_wb_variable_code()`
#' For a given string from a World Bank Data URL or partial URL, extract the code for the variable, but not the location
#' @param text String that is the URL or partial URL from World Bank Data for a given variable
#' @export
get_wb_variable_code <- function(text) {
  # Define the correct regex pattern
  variable_pattern <- ".*/([^/?]*)(?:\\?.*)?$"

  # Extract the desired part from each example string
  extract_variable <- function(text, pattern) {
    # If no slash is found, just return up to the first '?', or the whole string
    result <- stringr::str_match(text, pattern)[, 2]
    if (is.na(result)) {
      stringr::str_split(text, "\\?", n = 2)[[1]][1]
    } else {
      result
    }
  }
  variable_code <- extract_variable(text, variable_pattern)
}


#' `get_ecodata_variable_wb()`
#' Retrieve data for a single variable from World Bank Data, given the URL for the data location, variable code, or the partial URL including the variable code and possibly the location
#' @param varcode String that is the URL for the data, variable code, or the partial URL including the variable code and possibly the location.
#'                If the location is part of the text, it will download the variable for just that country/location. If location is not given, it will download the variable for all countries/locations.
#' @param varname Optional, string to give the variable name. By default, the variable will be named the variable code given in World Bank Data.
#' @return Data frame for the single variable from World Bank Data. If country is specified in the varcode, the data frame will include the date and a single column for a single country. If no country is specified, the data frame will include a column for every country. The data frame will also include all relevant meta data describing the data and citing its source.
#' @export
get_ecodata_variable_wb <- function(varcode, varname = NULL) {
  locations_pattern <- "(?<=\\?locations=)[^&]*"
  country_code <- str_extract(varcode, locations_pattern)
  if(is.na(country_code)) country_code <- ""

  variable_code <- get_wb_variable_code(varcode)

  if(variable_code == "") {
    warnmsg <- sprintf("No data found in World Bank for variable code, \'%s\'.", varcode)
    warning(warnmsg)
    return(NA)
  }

  if(country_code != "") {
    all_country_codes <- stringr::str_split(country_code, pattern = "-", simplify = TRUE)[1,]
    df <- get_ecodata_variable_country_wb(variable_code, all_country_codes[1], varname = varname)
    if(length(all_country_codes) > 1) {
      for(ci in 2:length(all_country_codes)) {
        dfc <- get_ecodata_variable_country_wb(variable_code, all_country_codes[ci], varname = varname)
        df <- dplyr::full_join(df, dfc, by = "Date")
      }
    }
  } else {
    df <- get_ecodata_variable_allcountries_wb(variable_code, varname = varname)
  }
  return(df)
}

#' `get_ecodata_variable_allcountries_wb()`
#' Retrieve data for a single variable from World Bank Data for all available countries, given the URL for the data location, variable code, or the partial URL including the variable code.
#' @param varcode String that is the URL for the data, variable code, or the partial URL including the variable code. If a location is included in the `varcode`, it will be ignored.
#' @param varname Optional, string to give the variable name. By default, the variable will be named the variable code given in World Bank Data.
#' @return Data frame for the single variable from World Bank Data. The data frame will include a column for every country. The data frame will also include all relevant meta data describing the data and citing its source.
#' @export
get_ecodata_variable_allcountries_wb <- function(varcode, varname = NULL) {
  variable_code <- get_wb_variable_code(varcode)

  if(variable_code == "") {
    warnmsg <- sprintf("No data found in World Bank for variable code, \'%s\'.", varcode)
    warning(warnmsg)
    return(NA)
  }

  raw.df <- tryCatch({
      wbstats::wb_data(indicator = variable_code, date_as_class_date = TRUE)
    },
    error = function(e) {
      NA
    })
  if(all(is.na(raw.df))) {
    warnmsg <- sprintf("No data found in World Bank for variable code, \'%s\'.", varcode)
    warning(warnmsg)
    return(NA)
  }

  if(is.null(varname)) {
    varname <- variable_code
  }

  info <- wbstats::wb_search(variable_code)

  df <- dplyr::tibble(
    Date = raw.df$date,
    Country = raw.df$country,
    Country_ISO2 = raw.df$iso2c,
    !!dplyr::sym(varname) := raw.df[[variable_code]]
  )
  countries.df <- df |>
    dplyr::select(Country, Country_ISO2) |>
    dplyr::distinct()

  df <- df |>
    dplyr::mutate(`Variable Name` = sprintf("%s %s", varname, Country)) |>
    dplyr::select(Date, `Variable Name`, dplyr::all_of(varname)) |>
    tidyr::pivot_wider(names_from = `Variable Name`, values_from = dplyr::all_of(varname)) |>
    dplyr::arrange(Date)

  all_variable_names <- get_ecodata_varnames(df)
  for(variable_name in all_variable_names) {
    attr(df[[variable_name]], "Variable") <- varname
    country <- stringr::str_squish(stringr::str_remove_all(variable_name, varname))
    country_code <- countries.df$Country_ISO2[countries.df$Country == country]
    attr(df[[variable_name]], "Code") <- sprintf("%s?locations=%s", varcode, country_code)
    attr(df[[variable_name]], "Description") <- info$indicator_desc[1]
    attr(df[[variable_name]], "Frequency") <- "Annual"
    attr(df[[variable_name]], "Seasonal Adjustment") <- "Not Seasonally Adjusted"
    source_str <- sprintf("World Bank Data")
    attr(df[[variable_name]], "Source") <- source_str
    url_str <- sprintf("https://data.worldbank.org/indicator/%s?locations=%s", varcode, country_code)
    attr(df[[variable_name]], "URL") <- url_str
    access_date <- format.Date(Sys.Date(), "%B %d, %Y")
    attr(df[[variable_name]], "Access Date") <- access_date
    cite_str <- sprintf("SOURCE: %s; %s (Accessed on %s)", source_str, url_str, access_date)
    attr(df[[variable_name]], "Cite") <- cite_str
  }

  return(df)
}


#' `get_ecodata_variable_wb()`
#' Downloads data from World Bank for a given varcode and countrycode, sets the variable equal to varname
#' @param varcode String that is the variable code
#' @param countrycode String that is the country code
#' @param varname Optional, string for the variable name
#' @return Data frame time series with two variables, Date and the variable requested. The data frame will also include all relevant meta data describing the data and citing its source.
#' @export
get_ecodata_variable_country_wb <- function(varcode, countrycode, varname = NULL) {
  raw.df <- tryCatch({
        wbstats::wb_data(country = countrycode, indicator = varcode, date_as_class_date = TRUE)
      },
      error = function(e) {
        NA
      })
  if(all(is.na(raw.df))) {
    warnmsg <- sprintf("No data found in World Bank for variable code, \'%s\', and country code, \'%s\'. Returning NA.\n", varcode, countrycode)
    warning(warnmsg)
    return(NA)
  }
  info <- wbstats::wb_search(varcode)

  # Infer units from info
  units <- "None"
  wb_description <- info$indicator_desc[1]
  if (string_detect(wb_description, "growth") & (string_detect(wb_description, "percent") | string_detect(wb_description, "%")) ) {
    units <- "Growth rate (percentage)"
  } else if(string_detect(wb_description, "index")) {
    units <- "Index"
  } else if(string_detect(wb_description, "percent") | string_detect(wb_description, "proportion")) {
    units <- "Percentage"
  } else if((string_detect(wb_description, "dollar") | string_detect(wb_description, "\\$")) & string_detect(wb_description, "PPP")  ) {
    units <- "International dollars (PPP)"
  } else if((string_detect(wb_description, "dollar") | string_detect(wb_description, "\\$")) & string_detect(wb_description, "constant")  ) {
    units <- "Real U.S. dollars"
  } else if((string_detect(wb_description, "dollar") | string_detect(wb_description, "\\$")) & string_detect(wb_description, "current")  ) {
    units <- "Nominal U.S. dollars"
  } else if(string_detect(wb_description, "LCU") & string_detect(wb_description, "constant")  ) {
    units <- "Real local currency"
  } else if(string_detect(wb_description, "LCU") & string_detect(wb_description, "current")  ) {
    units <- "Nominal local currency"
  }

  indicator <- sprintf("%s %s", raw.df$country[1], info$indicator[1])
  if(is.null(varname)) {
    varname <- stringr::str_squish(stringr::str_remove(indicator, "\\([^()]*\\)"))
  }

  df <- dplyr::tibble(Date = raw.df$date, !!dplyr::sym(varname) := raw.df[[varcode]])
  df <- dplyr::filter(df, !is.na( !!dplyr::sym(varname) ) )

  attr(df[[varname]], "Variable") <- varname
  attr(df[[varname]], "Code") <- sprintf("%s?locations=%s", varcode, countrycode)
  attr(df[[varname]], "Description") <- info$indicator_desc[1]
  attr(df[[varname]], "Frequency") <- "Annual"
  attr(df[[varname]], "Units") <- units
  attr(df[[varname]], "Seasonal Adjustment") <- "Not Seasonally Adjusted"
  source_str <- sprintf("World Bank Data")
  attr(df[[varname]], "Source") <- source_str
  url_str <- sprintf("https://data.worldbank.org/indicator/%s?locations=%s", varcode, countrycode)
  attr(df[[varname]], "URL") <- url_str
  access_date <- format.Date(Sys.Date(), "%B %d, %Y")
  attr(df[[varname]], "Access Date") <- access_date
  cite_str <- sprintf("SOURCE: %s; %s (Accessed on %s)", source_str, url_str, access_date)
  attr(df[[varname]], "Cite") <- cite_str

  return(df)
}

#' `get_ecodata_variable_fred()`
#' Downloads data from FRED for a given variable code or URL
#' @param varcode String that contains the variable code or URL to the data
#' @param varname Optional, string for the variable name
#' @param recessions Optional, logical for whether to include an NBER recession dummy variable. Default = FALSE.
#' @return Data frame time series with two variables, Date and the variable requested. The data frame will also include all relevant meta data describing the data and citing its source.
#' @export
get_ecodata_variable_fred <- function(varcode, varname = NULL, recessions = FALSE) {
  ecodata_fred_openapi()
  orig_varcode <- varcode
  if(is_valid_url(varcode)) {
    varcode <- tryCatch({
      get_varcode_url(varcode)
    },
    error = function(e) {
      NA
    })
  }
  if(is.na(varcode)) {
    warnmsg <- sprintf("No data found in FRED for \'%s\'. Returning NA.\n", orig_varcode)
    warning(warnmsg)
    return(NA)
  }

  varcode <- str_to_upper(varcode)

  raw.df <- tryCatch({
      fredr::fredr(varcode)
    },
    error = function(e) {
      NA
    })
  if(all(is.na(raw.df))) {
    warnmsg <- sprintf("No data found in FRED for \'%s\'. Returning NA.\n", orig_varcode)
    warning(warnmsg)
    return(NA)
  }

  info <- fredr::fredr_series(varcode)
  if(is.null(varname)) {
    varname <- info$title[1]
  }
  new.df <- tidyr::tibble(Date := raw.df$date, !!varname := raw.df$value)

  new.df <- dplyr::filter(new.df, !is.na(!!dplyr::sym(varname)))

  if(recessions) {
    nber_recessions_code <- "USRECDM"
    raw.recessions.df <- fredr::fredr(nber_recessions_code)
    recessions.df <- tidyr::tibble(Date = raw.recessions.df$date, Recession = raw.recessions.df$value)
    new.df <- dplyr::right_join(recessions.df, new.df, by = "Date")
  }

  # Attributes
  attr(new.df[[varname]], "Variable") <- varname
  attr(new.df[[varname]], "Code") <- varcode
  attr(new.df[[varname]], "Description") <- info$title[1]
  attr(new.df[[varname]], "Frequency") <- info$frequency[1]
  attr(new.df[[varname]], "Units") <- info$units_short[1]
  attr(new.df[[varname]], "Seasonal Adjustment") <- info$seasonal_adjustment[1]
  source_str <- sprintf("FRED Ⓡ, Federal Reserve Bank of St. Louis")
  attr(new.df[[varname]], "Source") <- source_str
  url_str <- sprintf("https://fred.stlouisfed.org/series/%s", varcode)
  attr(new.df[[varname]], "URL") <- url_str
  access_date <- format.Date(Sys.Date(), "%B %d, %Y")
  attr(new.df[[varname]], "Access Date") <- access_date
  cite_str <- sprintf("SOURCE: %s; %s (Accessed on %s)", source_str, url_str, access_date)
  attr(new.df[[varname]], "Cite") <- cite_str

  return(new.df)
}

#' Get state-level FRED data for all U.S. states
#'
#' Downloads data from FRED for all U.S. states, for a given variable code or URL.
#' The variable code or URL needs to be a state-specific variable, and be for just one of any of the U.S. States.
#' The function will retrieve the data for all U.S. states.
#' @param varcode String for the variable code or URL to the data, for any one U.S. state
#' @param recessions Optional, logical for whether to include an NBER recession dummy variable. Default = FALSE.
#' @return Data frame the variable requested for all U.S. states. The data frame will include a date variable and a column for every U.S. state. The data frame will also include all relevant meta data describing the data and citing its source.
#' @export
get_ecodata_allstates_fred <- function(varcode, recessions = FALSE) {
  ecodata_fred_openapi()
  if(is_valid_url(varcode)) {
    varcode <- get_varcode_url(varcode)
  }
  varcode <- str_to_upper(varcode)

  raw.df <- fredr::fredr(varcode)
  info <- fredr::fredr_series(varcode)
  varname <- info$title[1]

  fips.df <- get_state_fips_all()

  success <- FALSE
  for(state in fips.df$State) {
    if(str_detect(varname, state)) {
      found_state <- state
      success <- TRUE
      break
    }
  }
  if(success) {
    state_fips <- get_state_fips(state)
    state_abbr <- get_state_abbr(state)
    success <- FALSE
    if(str_detect(varcode, state_fips)) {
      success <- TRUE
      all_var_codes <- str_replace(varcode, state_fips, fips.df$FIPS)
    } else if(str_detect(varcode, state_abbr)) {
      success <- TRUE
      all_var_codes <- str_replace(varcode, state_abbr, fips.df$Abbr)
    }
  }
  df <- NA
  if(success) {
    df <- get_ecodata(all_var_codes, recessions = recessions)
  } else {
    error_msg <- sprintf("Could not find data on other states based on variable code, %s", varcode)
    stop(error_msg)
  }

  # Make the variable description for each equal to the (long) existing name
  for(var in names(df)) {
    orig_class <- class(df[[var]])
    Hmisc::label(df[[var]]) <- var
    attr(df[[var]], "label") <- var
    class(df[[var]]) <- orig_class
  }

  description <- stringr::str_replace(varname, found_state, "Each U.S. State")
  attr(df, "Description") <- description
  Hmisc::label(df) <- description

  description_remove <- stringr::str_remove(varname, found_state)
  names(df) <- stringr::str_remove(names(df), description_remove)
  names(df) <- stringr::str_to_title(names(df))
  return(df)
}

#' `get_ecodata_varnames()`
#' Fetch all the variables in a given data frame, except the date and recession
#' @param data Data frame with fetched with a get_ecodata() function
#' @return Returns a vector of variable names
#' @export
get_ecodata_varnames <- function(data) {
  ecovars_idx <- !string_compare(names(data), "date") & !string_detect(names(data), "recession")
  ecovars <- names(data)[ecovars_idx]
  return(ecovars)
}

#' `get_ecodata_variable()`
#' Get data for a single variable from either FRED or World Bank Data, for a given URL or variable code. The function will figure out whether the data is available from FRED or World Bank Data.
#' @param varcode String that identifies the variable code or URL for the data
#' @param varname Optional, string for the variable name. Default is the code given by the source.
#' @param recessions Logical for whether or not to include a dummy variable identifying a NBER U.S. Recessions. Default = FALSE.
#' @return Data frame time series that includes the date and the variable requested. The data frame will also include all relevant meta data describing the data and citing its source.
#' @export
get_ecodata_variable <- function(varcode, varname = NULL, recessions = FALSE) {
  if(string_detect(varcode, "stlouisfed")) {
    df <- get_ecodata_variable_fred(varcode, varname, recessions)
  } else if(string_detect(varcode, "worldbank")) {
    df <- get_ecodata_variable_wb(varcode, varname)
  } else {
    df <- tryCatch({
        get_ecodata_variable_fred(varcode, varname, recessions)
      },
      warning = function(w) {NA}
    )
    if(all(is.na(df))) {
      df <- tryCatch({
          get_ecodata_variable_wb(varcode, varname)
        },
        warning = function(W) {NA}
      )
    }

    if(all(is.na(df))) {
      warnmsg <- sprintf("No data found in FRED or World Bank for URL or variable code, \'%s\'. Returning NA.\n", varcode)
      warning(warnmsg)
    }
  }
  return(df)
}

#' `get_ecodata()`
#' Get data for one or more variables from FRED and/or World Bank Data, for given URLs or variable codes. The function will figure out whether the data is available from FRED or World Bank Data.
#' @param varcodes String for vector of strings that identifies the variable codes or URLs for the data.
#' @param varnames Optional, string or vector of strings for the variable names. Default is the code given by the source.
#' @param recessions Logical for whether or not to include a dummy variable identifying a NBER U.S. Recessions. Default = FALSE.
#' @return Data frame time series that includes the date and the variable requested. The data frame will also include all relevant meta data describing the data and citing its source.
#' @export
get_ecodata <- function(varcodes, varnames = NULL, recessions = FALSE) {
  if(!is.null(varnames) & (length(varcodes) != length(varnames)) ) {
    stop("Length of `varcodes` is not equal to the length of `varnames`. There needs to be the name number of variables as variable names.")
  }

  varname <- NULL
  if(!is.null(varnames)) {
    varname <- varnames[1]
  }
  df <- get_ecodata_variable(varcodes[1], varname, recessions = FALSE)

  if(length(varcodes) > 1) {
    for(v in 2:length(varcodes)) {
      varcode <- varcodes[v]
      varname <- NULL
      if(!is.null(varnames)) {
        varname <- varnames[v]
      }
      newvar <- get_ecodata_variable(varcode, varname, recessions = FALSE)
      df <- dplyr::full_join(df, newvar, by = "Date")
    }
  }
  if(recessions) {
    df <- get_recessions(df)
  }

  df <- dplyr::arrange(df, Date)

  return(df)
}

#' `add_ecodata()`
#' Add data for one or more variables from FRED and/or World Bank Data, for given URLs or variable codes. The function will figure out whether the data is available from FRED or World Bank Data.
#' @param data Data frame with existing ecodata, data from FRED and/or World Bank Data
#' @param varcodes String for vector of strings that identifies the variable codes or URLs for the data.
#' @param varnames Optional, string or vector of strings for the variable names. Default is the code given by the source.
#' @param recessions Logical for whether or not to include a dummy variable identifying a NBER U.S. Recessions. Default = FALSE.
#' @return Data frame merged with original data frame that includes the new variables requested. The data frame will also include all relevant meta data describing the data and citing its source.
#' @export
add_ecodata <- function(data, varcodes, varnames = NULL, recessions = FALSE) {
  df <- get_ecodata(varcodes, recessions = FALSE)
  df <- dplyr::full_join(data, df, by = "Date")

  if(recessions | string_detect_any(names(df), "Recession")) {
    df <- get_recessions(df)
  }
  df <- dplyr::arrange(df, Date)
  return(df)
}

#' `ecodata_get_units(df)`
#' Get unique units for the variables in the given data frame
#' @param df Data frame from `get_ecodata()` that includes units information in the meta data
#' @return A vector of unique unit descriptions from the data frame
#' @export
ecodata_get_units <- function(df) {
  all_units <- ""
  vars <- get_ecodata_varnames(df)
  for(v in 1:length(vars)) {
    varname <- vars[v]
    if(!is.null(attr(df[[varname]], "Units"))) {
      all_units[v] <- attr(df[[varname]], "Units")
    }
  }
  unique_units <- unique(all_units)
  unique_units <- unique_units[unique_units != ""]
  return(unique_units)
}

#' `ecodata_get_sources(df)`
#' Get unique sources for the variables in the data frame
#' @param df Data frame from `get_ecodata()` that includes units information in the meta data
#' @return A vector of unique source descriptions from the data frame
#' @export
ecodata_get_sources <- function(df) {
  all_sources <- ""
  vars <- get_ecodata_varnames(df)
  for(v in 1:length(vars)) {
    if(!is.null(attr(df[[vars[v]]], "Source"))) {
      all_sources[v] <- attr(df[[vars[v]]], "Source")
    }
  }
  unique_sources <- unique(all_sources)
  unique_sources <- unique_sources[unique_sources != ""]

  return(unique_sources)
}

#' `ecodata_description(data)`
#' Get a description of every variable in the given ecodata data frame
#' @param data Data frame from `get_ecodata()` that includes units information in the meta data
#' @return Returns a data frame with a row for each variable in the given data frame, and several columns describing attributes of each variable
#' @seealso [ecodata_description_table()]
#' @export
ecodata_description <- function(data) {
  desc.df <- dplyr::tibble()
  ecovars <- get_ecodata_varnames(data)
  for(v in ecovars) {
    row.df <- dplyr::as_tibble(attributes(data[[v]]))
    desc.df <- dplyr::bind_rows(desc.df, row.df)
  }
  desc.df <- dplyr::select(desc.df, -Cite)
  return(desc.df)
}

#' `ecodata_cite(data)`
#' Get citation information for every variable in the given ecodata data frame
#' @param data Data frame from `get_ecodata()` that includes units information in the meta data
#' @return Returns a data frame with a row for each variable in the given data frame, with a column for how to cite each variable
#' @seealso [ecodata_cite_table()]
#' @export
ecodata_cite <- function(data) {
  cite.df <- dplyr::tibble()
  ecovars <- get_ecodata_varnames(data)
  for(v in ecovars) {
    row.df <- dplyr::as_tibble(attributes(data[[v]])) |>
      dplyr::select(Variable, Cite)
    cite.df <- dplyr::bind_rows(cite.df, row.df)
  }
  return(cite.df)
}

#' `ecodata_description(data)`
#' Get a table that describes every variable in the given ecodata data frame
#' @param data Data frame from `get_ecodata()` that includes units information in the meta data
#' @return Returns a flextable with a row for each variable in the given data frame, and several columns describing attributes of each variable
#' @seealso [ecodata_description()]
#' @export
ecodata_description_table <- function(data) {
  desc.df <- ecodata_description(data)
  desc.df <- desc.df |>
    dplyr::mutate(Variable = Description) |>
    dplyr::select(-Description)

  tb <- flextable::flextable(desc.df) |>
    flextable::compose(j = "URL",
                       value = flextable::as_paragraph(
                         flextable::hyperlink_text(
                           x = desc.df$URL, url = desc.df$URL, props = officer::fp_text(color = "blue", font.size = 11)))) |>
    flextable::autofit()

  return(tb)
}

#' `ecodata_cite_table(data)`
#' Get a table of citation information for every variable in the given ecodata data frame
#' @param data Data frame from `get_ecodata()` that includes units information in the meta data
#' @return Returns a flextable with a row for each variable in the given data frame, with a column for how to cite each variable
#' @seealso [ecodata_cite()]
#' @export
ecodata_cite_table <- function(data) {
  desc.df <- ecodata_description(data)

  var.df <- desc.df |>
    dplyr::select(Variable) |>
    dplyr::mutate(Cite = "")

  tb <- flextable::flextable(var.df) |>
    flextable::compose(j = "Cite",
                       value = flextable::as_paragraph(
                          "Source: ",
                          desc.df$Source,
                          "; ",
                          flextable::hyperlink_text(
                            x = desc.df$URL, url = desc.df$URL, props = officer::fp_text(color = "blue", font.size = 11)),
                          "; Accessed on ",
                          desc.df$`Access Date`,
                          "."
                       )
                      ) |>
    flextable::autofit()

  return(tb)
}

#' ecodata_colorscale
#'
#' Return up to 6 colors to use for a categorical color scale in a plot
#' @param n Number of colors to return, n must be between 1 and 7
#' @return Returns a vector of color values
#' @export
ecodata_colorscale <- function(n) {
  n <- as.integer(n)
  if(is.na(n)) {
    stop("Number of colors, n, must be an integer.")
  }
  if(n>7) {
    stop("The maximum number of colors for discrete ecodata is 7.")
  } else if(n<1) {
    stop("The minimum number of colors for discrete ecodata is 1.")
  }
  rem <- floor(n/4)*2
  if(rem < 2) rem <- 2

  len <- n + rem
  if(n %% 2 == 1) len <- len+1
  b1 <- len/2 - rem/2
  b2 <- len - b1 + rem/2
  if(n %% 2 == 1) b2 <- b2+1

  mycols <- RColorBrewer::brewer.pal(n=len, name = "RdBu")[c(1:b1,b2:len)]
  return(mycols)
}

#' abbreviated_units
#'
#' Convert a number to an abbreviated string, using "th", "ml", or "bn" for "thousand", "million", or "billion", as appropriate
#' @param x Numeric, number to abbreviate
#' @return String describing the number, with the appropriate abbreviation
#' @export
abbreviated_units <- function(x) {
  labs <- dplyr::case_when(
    abs(x) >= 1e9 ~ paste0(scales::comma(x / 1e9), " bn"),  # Billions
    abs(x) >= 1e6 ~ paste0(scales::comma(x / 1e6), " ml"),  # Millions
    abs(x) >= 1e3 ~ paste0(scales::comma(x / 1e3), " th"),  # Thousands
    TRUE ~ as.character(x)  # Leave smaller numbers as-is
  )
  return(labs)
}

#' abbreviated_units_dollar
#'
#' Convert a dollar amount to an abbreviated string, using "th", "ml", or "bn" for "thousand", "million", or "billion", as appropriate
#' @param x Numeric, number to abbreviate
#' @return String describing the number as a dollar amount, with a dollar sign, commas, and with the appropriate abbreviation
#' @export
abbreviated_units_dollar <- function(x) {
  labs <- dplyr::case_when(
    abs(x) >= 1e9 ~ paste0("$", scales::comma(x / 1e9), " bn"),  # Billions
    abs(x) >= 1e6 ~ paste0("$", scales::comma(x / 1e6), " ml"),  # Millions
    abs(x) >= 1e3 ~ paste0("$", scales::comma(x / 1e3), " th"),  # Thousands
    TRUE ~ scales::dollar(x)  # Smaller numbers with dollar formatting
  )
  return(labs)
}

#' `ggplot_ecodata_ts(data)`
#' Make a time series line plot of the variables in the given data frame, in a single plot area.
#' The number of variables in the data frame should be between 1 and 7
#' @param data Data frame from `get_ecodata()` that includes a variable called `Date` and the other variables to plot
#' @param title Optional, string for the title of the plot. Default is ""
#' @param ylab Optional, string for the y-axis label. Default is the units of the meta data for the variables to be plotted
#' @param variables Optional, vector of strings that includes the economic variables to plot. If not specified, the function will plot all the variables given in `data`, if possible.
#' @param title_strlen Optional, word-wrap the length of the title by this many characters. Default = 60.
#' @param variable_strlen Optional, word-wrap the length of the variable names by this many characters. Default = 85.
#' @param plot.recessions Optional, logical for whether or not show show NBER recession bars in the plot
#' @export
ggplot_ecodata_ts <- function(data, variables = NULL, title="", ylab = NULL, title_strlen = 60, variable_strlen = 85, plot.recessions = FALSE) {
  linewidth <- 1.5
  linecolor <- "dodgerblue4"

  # Wrap title if necessary
  if(stringr::str_length(title) > title_strlen) {
    # Put a new line first after a colon
    title <- stringr::str_replace(title, ":", ":\n")
    alllines <- stringr::str_split(title, "\n")[[1]]
    alllines <- stringr::str_wrap(alllines, title_strlen)
    title <- paste(alllines, collapse = "\n")
  }

  # Identify the variables to plot
  includevars <- get_ecodata_varnames(data)
  if(!is.null(variables)) {
    morevars_idx <- names(data) %in% variables
    includevars <- names(data)[morevars_idx]
  }

  # filter out just the variables to include
  data <- data |>
    filter(!dplyr::if_all(dplyr::all_of(includevars), is.na))
  # Linear interpolation for missing values
  data <- ecodata_linear_interpolate(data, includevars)

  nvar <- length(includevars)
  if(nvar > 7) {
    stop("Number of variables to plot in a time series plot must be 7 or less.")
  }
  if(nvar < 1) {
    stop("There must be at least one variable to plot.")
  }

  plotvars <- includevars

  # Make sure all plot variables have the same units
  all_units <- vector(length = length(plotvars))
  for(v in 1:length(plotvars)) {
    all_units[v] <- attr(data[[plotvars[v]]], "Units")
  }
  unique_units <- unique(all_units)
  if(length(unique_units) > 1) {
    msg <- sprintf("Not all variables are in the same units. Units include, %s.", paste(unique_units, sep = ", ", collapse = ", "))
    warning(msg)
  }
  useunits <- unique_units[1]
  if(is.null(ylab)) {
    ylab <- useunits
  }

  # Setup a units function for the vertical axis scale labels
  units_function <- function(x) abbreviated_units(x)
  if(string_detect(useunits, "\\$") | str_detect(str_to_lower(useunits), "dollar")) {
    units_function <- function(x) return(abbreviated_units_dollar(x))
  } else if(string_detect(useunits, "%") | string_detect(useunits, "percent") | string_detect(useunits, "proportion")) {
    units_function <- function(...) return(scales::percent(scale=1, ...))
  }

  data_sources <- ecodata_get_sources( dplyr::select(data, dplyr::all_of(plotvars)) )
  if(length(data_sources) == 0) {
    sources_str <- ""
  } else {
    sources_str <- paste(data_sources, collapse = "\n", sep = "\n")
    if(length(data_sources)==1) sources_str <- sprintf("Source: %s", sources_str)
    if(length(data_sources)>1) sources_str <- sprintf("Sources:\n %s", sources_str)
  }

  mycols <- rev(ecodata_colorscale(nvar))

  plot.df <- data |>
    tidyr::pivot_longer(cols = dplyr::all_of(plotvars), names_to = "Variable", values_to = "Value")

  # Wrap strings for variable names
  plotvars <- stringr::str_wrap(plotvars, variable_strlen)
  plot.df <- dplyr::mutate(plot.df, Variable = stringr::str_wrap(Variable, variable_strlen))

  # Preserve the order of the variables
  plot.df <- dplyr::mutate(plot.df, Variable = factor(Variable, levels = plotvars, ordered = TRUE))

  plt <- ggplot2::ggplot(plot.df, ggplot2::aes(x = Date, y = Value, color = Variable)) +
    ggplot2::geom_line(linewidth = linewidth) +
    ggplot2::scale_x_date(breaks = scales::pretty_breaks(n=8)) +
    ggplot2::scale_y_continuous(labels = units_function) +
    ggplot2::scale_color_manual(values = mycols) +
    ggplot2::labs(y = ylab, x = "", color = "", title = title, caption = sources_str) +
    ecodata_theme() +
    ggplot2::theme(legend.position = "bottom", legend.justification = "left") +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = length(plotvars)))  # Each item in a separate row

  if(plot.recessions) {
    plt <- plot + geom_recession(fill="dodgerblue3")
  }

  return(plt)
}

#' ggplot_ecodata_facet
#'
#' Make a time series line plot of the variables in the given data frame, with each variable in its own facet.
#' @param data Data frame from `get_ecodata()` that includes a variable called `Date` and the other variables to plot
#' @param variables Optional, vector of strings that includes the economic variables to plot. If not specified, the function will plot all the variables given in `data`, if possible.
#' @param title Optional, string for the title of the plot. Default is ""
#' @param ylab Optional, string for the y-axis label. Default is the units of the meta data for the variables to be plotted
#' @param ncol Optional, number of columns for the facet plot. Default is 4.
#' @param scales Optional, string passed to `facet_wrap()` on whether to fix or free the scales on the x and y axes
#' @param color Optional, color of the lines. Default is "dodgerblue4"
#' @param title_strlen Optional, word-wrap the length of the title by this many characters. Default = 60.
#' @param strip_width Optional, word-wrap variable in the title of the individual facet. Default = 40.
#' @param plot.recessions Optional, logical for whether or not show show NBER recession bars in the plot
#' @return Returns a ggplot, faceted by each economic variable
#' @export
ggplot_ecodata_facet <- function(data, variables = NULL, title="", ylab = NULL, ncol = 4, scales = "free", color = "dodgerblue4", title_strlen = 60, strip_width = 40, plot.recessions = FALSE) {
  linewidth <- 1.5
  linecolor <- "dodgerblue4"
  if(stringr::str_length(title) > title_strlen) {
    # Put a new line first after a colon
    title <- stringr::str_replace(title, ":", ":\n")
    alllines <- stringr::str_split(title, "\n")[[1]]
    alllines <- stringr::str_wrap(alllines, title_strlen)
    title <- paste(alllines, collapse = "\n")
  }

  # Identify the variables to plot
  includevars <- get_ecodata_varnames(data)
  if(!is.null(variables)) {
    morevars_idx <- names(data) %in% variables
    includevars <- names(data)[morevars_idx]
  }

  # filter out just the variables to include
  data <- data |>
    filter(!dplyr::if_all(dplyr::all_of(includevars), is.na))
  # Linear interpolation for missing values
  data <- ecodata_linear_interpolate(data, includevars)

  nvar <- length(includevars)
  if(nvar < 1) {
    stop("There must be at least one variable to plot.")
  }

  plotvars <- includevars

  # Make sure all plot variables have the same units
  all_units <- vector()
  for(v in 1:length(plotvars)) {
    all_units[v] <- attr(data[[plotvars[v]]], "Units")
  }
  unique_units <- unique(all_units)
  if(length(unique_units) > 1) {
    msg <- sprintf("Not all variables are in the same units. Units include, %s.", paste(unique_units, sep = ", ", collapse = ", "))
    warning(msg)
  }
  useunits <- unique_units[1]
  if(is.null(ylab)) {
    ylab <- useunits
  }

  # Setup a units function for the vertical axis scale labels
  units_function <- function(x) abbreviated_units(x)
  if(string_detect(useunits, "\\$") | str_detect(str_to_lower(useunits), "dollar")) {
    units_function <- function(x) return(abbreviated_units_dollar(x))
  } else if(string_detect(useunits, "%") | string_detect(useunits, "percent") | string_detect(useunits, "proportion")) {
    units_function <- function(...) return(scales::percent(scale=1, ...))
  }

  plot.df <- data |>
    tidyr::pivot_longer(cols = dplyr::all_of(plotvars), names_to = "Variable", values_to = "Value")

  # Length of strip text
  plot.df <- dplyr::mutate(plot.df, Variable = stringr::str_wrap(Variable, width = strip_width))
  plotvars <- stringr::str_wrap(plotvars, width = strip_width)

  # Preserve the order of the variables
  plot.df <- dplyr::mutate(plot.df, Variable = factor(Variable, levels = plotvars, ordered = TRUE))

  plt <- ggplot2::ggplot(plot.df, ggplot2::aes(x = Date, y = Value)) +
    ggplot2::geom_line(linewidth = linewidth, color = color) +
    ggplot2::scale_x_date(breaks = scales::pretty_breaks(n=8)) +
    ggplot2::scale_y_continuous(labels = units_function) +
    ggplot2::facet_wrap(ggplot2::vars(Variable), ncol = ncol, scales = scales) +
    ggplot2::labs(y = "", x = "", color = "", title = title) +
    ecodata_theme() +
    ggplot2::theme(legend.position = "bottom", legend) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = length(plotvars)))  # Each item in a separate row

  if(plot.recessions) {
    plt <- plot + geom_recession(fill="dodgerblue3")
  }

  return(plt)
}

#' `ecodata_set_fredkey(key)`
#' Set the FRED key as a permanent environmental variable, and make it available immediately
#' This will append the ~/.Renviron file
#' @param API_Key String that is the API key for FRED. Go to https://fredaccount.stlouisfed.org/apikeys to request a key.
#' @export
ecodata_set_fredkey <- function(API_Key) {
  newentry <- sprintf("FRED_API_KEY=%s", API_Key)
  renviron_path <- path.expand("~/.Renviron")
  write(newentry, file = renviron_path, append = TRUE)
  Sys.setenv(FRED_API_KEY = API_Key)
  fredr::fredr_set_key(API_Key)
  # Test FRED
  gdp <- tryCatch(fredr::fredr("GDP"),
                  error = function(e) {
                    stop("Failed to connect to FRED. Check for internet connection or for valid FRED API key. Obtain a key at \"https://fredaccount.stlouisfed.org/apikeys\" and set the key with the function, `ecodata_set_fredkey()`")
                  })
}

#' `ecodata_get_fredkey()`
#' Get the FRED API key if it exists. Returns empty string if it doesn't exist
#' @return String that is the FRED API key. Empty string if it doesn't exist
#' @export
ecodata_get_fredkey <- function() {
  return(Sys.getenv("FRED_API_KEY"))
}

#' `string_not_empty(text)`
#' Just return TRUE or FALSE if string given is not empty (i.e. not NULL, not NA, not whitespace, not "")
#' @param text Text to test if it is not empty
#' @return Logical, whether or not the string exists and has characters in it
string_not_empty <- function(text) {
  if(is.null(text)) return(FALSE)
  if(is.na(text)) return(FALSE)
  if(stringr::str_squish(text)=="") return(FALSE)
  return(TRUE);
}

#' `string_empty(text)`
#' Just return TRUE or FALSE if string given is empty ("empty" can include NULL, NA, whitespace only, "")
#' @param text Text to test if it is empty
#' @return Logical, whether or not the string is empty
string_empty <- function(text) {
  return(!string_not_empty(text))
}

#' ecodata_fred_openapi()
#'
#' Check to see if the FRED API key already exists and is loaded. If it exists as an environmental variable, but is not loaded, it will attempt to load it. Halts execution if the API key is not found, or if downloading from FRED is not successful
#' @export
ecodata_fred_openapi <- function() {
  key <- fredr::fredr_get_key()
  if(string_empty(key)) {
    key <- ecodata_get_fredkey() # A key exists, but it is not set
    if(string_not_empty(key)) {
      fredr::fredr_set_key(API_Key)
    } else {
      stop("Must set FRED API key to proceed. Obtain a key at \"https://fredaccount.stlouisfed.org/apikeys\" and set the key with the function, `ecodata_set_fredkey()`")
    }
  }
  # Test FRED
  gdp <- tryCatch(fredr::fredr("GDP"),
           error = function(e) {
             stop("Failed to connect to FRED. Check for internet connection or for valid FRED API key. Obtain a key at \"https://fredaccount.stlouisfed.org/apikeys\" and set the key with the function, `ecodata_set_fredkey()`")
           })
}

# if(FALSE) {
#
# varcodes <- c(
#   "https://fred.stlouisfed.org/series/DTB3",
#   "https://fred.stlouisfed.org/series/DGS10",
#   "https://fred.stlouisfed.org/series/IR3TIB01DEM156N",
#   "https://fred.stlouisfed.org/series/IRLTLT01DEM156N",
#   "https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD?locations=US",
#   "https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD?locations=DE"
# )
#
# mydata <- get_ecodata(varcodes)
#
# # Plot a time series of the Real GDP data
# ggplot_ecodata_ts(mydata, title = "Real GDP", variables = c("United States GDP, PPP", "Germany GDP, PPP")) +
#   geom_recession()
#
# # Plot a time series of the U.S. interest rate data
# ggplot_ecodata_ts(mydata, title = "Interest Rates",
#                   variables = c("3-Month Treasury Bill Secondary Market Rate, Discount Basis", "Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity, Quoted on an Investment Basis")) +
#   geom_recession()
#
# # Plot a faceted time series of the U.S. and German interest rate data
# ggplot_ecodata_facet(mydata, title = "Interest Rates", ncol = 2,
#                   variables = c("3-Month Treasury Bill Secondary Market Rate, Discount Basis",
#                                 "Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity, Quoted on an Investment Basis",
#                                 "Interest Rates: 3-Month or 90-Day Rates and Yields: Interbank Rates: Total for Germany",
#                                 "Interest Rates: Long-Term Government Bond Yields: 10-Year: Main (Including Benchmark) for Germany")) +
#   geom_recession()
#
# # I forgot. Can I add on this variable too? Sure!
# mydata <- add_ecodata(mydata, "GDPC1")
# mydata <- add_ecodata(mydata, "MEPAINUSA646N")
#
# # Get information about the data
# mydata_description <- ecodata_description(mydata)
#
# # Get a pretty table of information about the data
# ecodata_description_table(mydata)
#
# # I need to cite my sources
# ecodata_cite_table(mydata)
# }
