#' glimpse
#'
#' A override of dplyr's glimpse() that also prints the label for the dataframe
#' Just prints information about the dataframe
#'
#' @param x An ecodata dataframe, the return value from `get_ecodata()`
#' @seealso [dplyr::glimpse()]
#' @export
glimpse <- function(x) {
  cat(attr(x, "label"))
  cat("\n")
  dplyr::glimpse(x)
}

#' filter
#'
#' A wrapper for `dplyr::filter()`.
#' @seealso [dplyr::filter()]
filter <- function(...) {
  dplyr::filter(...)
}

#' str_detect
#' Wrapper for `stringr::str_detect()`
#' @seealso [stringr::str_detect()]
str_detect <- function(...) {
  stringr::str_detect(...)
}

#' rename
#' Wrapper for `dplyr::rename()`
#' @seealso [dplyr::rename()]
rename <- function(...) {
  dplyr::rename(...)
}

#' str_replace
#' Wrapper for `stringr::str_replace()`
#' @seealso [stringr::str_replace()]
str_replace <- function(...) {
  stringr::str_replace(...)
}

#' str_to_lower
#' Wrapper for `stringr::str_to_lower()`
#' @seealso [str_to_lower()]
str_to_lower <- function(...) {
  stringr::str_to_lower(...)
}

#' ecodata_theme()
#' A ggplot theme that is simple and clean and has a large text size
#' @return Returns a ggplot theme that can be added to a `ggplot()` call
#' @export
ecodata_theme <- function() {
  rettheme <- ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = 22)) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"))

  return(rettheme)
}

#' string_compare
#' Compare two strings, ignoring case and whitespace
#' @param str1 String to compare
#' @param str2 Another string to compare
#' @return Boolen on whether the strings match
string_compare <- function(str1, str2) {
  return(
    stringr::str_squish(stringr::str_to_lower(str1)) == stringr::str_squish(stringr::str_to_lower(str2))
  )
}

#' string_detect
#'
string_detect <- function(str, pattern) {
  return(
    stringr::str_detect(
      stringr::str_to_lower(str), stringr::str_to_lower(pattern)
    )
  )
}

str_extract <- function(...) {
  stringr::str_extract(...)
}

string_detect_any <- function(str, pattern) {
  return(any(string_detect(str, pattern)))
}

string_which <- function(str, pattern) {
  return(
    stringr::str_which(
      stringr::str_to_lower(str), stringr::str_to_lower(pattern)
    )
  )
}

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

get_varcode_url <- function(url) {
  if(str_detect(url, "fred.stlouisfed.org")) {
    varcode <- str_extract(url, "(?<=series/).*")
  } else {
    error_code <- sprintf("Failed to find series: %s", url)
    stop(error_code)
  }
  return(varcode)
}

get_state_fips_all <- function() {
  fips_codes <- usmap::fips_info()
  fips_codes <- rename(fips_codes, State = full, FIPS = fips, Abbr = abbr)
  return(fips_codes)
}

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

GeomRecession <- ggplot2::ggproto("GeomRecession", ggplot2::Geom,

  draw_panel = function(self, data, panel_params, coord, alpha, fill) {
    dd <<- data
    mindate <- min(data$x)
    maxdate <- max(data$x)
    linewidth = 0.0

    load("recessions.RData")

    recession_data <- recession_data |>
      dplyr::filter(Trough >= mindate & Peak <= maxdate)


    # If a recession started before the start of the dataset, set the beginning to the start of the dataset
    if(min(recession_data$Peak) <= mindate) {
      recession_data[recession_data$Peak <= mindate] <- mindate
    }
    # If a recession ended before the end of the dataset, set the end to the end of the dataset
    if(max(recession_data$Trough) >= maxdate) {
      recession_data[recession_data$Trough >= maxdate] <- maxdate
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

# Define a ggplot_add method to automatically move the recession layer to the back
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

get_ecodata_variable_wb <- function(varcode, countrycode, varname = NULL) {
  raw.df <- wbstats::wb_data(country = countrycode, indicator = varcode, date_as_class_date = TRUE)
  info <- wbstats::wb_search(varcode)

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

get_ecodata_variable <- function(varcode, varname = NULL, recessions = TRUE) {
  if(is_valid_url(varcode)) {
    varcode <- get_varcode_url(varcode)
  }

  raw.df <- fredr::fredr(varcode)
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

get_ecodata_allstates <- function(varcode, recessions = TRUE) {
  if(is_valid_url(varcode)) {
    varcode <- get_varcode_url(varcode)
  }

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

# Just fetch all the variables except the date and recession
get_ecodata_varnames <- function(data) {
  ecovars_idx <- !string_compare(names(data), "date") & !string_detect(names(data), "recession")
  ecovars <- names(data)[ecovars_idx]
  return(ecovars)
}

get_ecodata <- function(varcodes, recessions = TRUE) {
  df <- get_ecodata_variable(varcodes[1], recessions = FALSE)

  if(length(varcodes) > 1) {
    for(v in 2:length(varcodes)) {
      varcode <- varcodes[v]
      newvar <- get_ecodata_variable(varcodes[v], recessions = FALSE)
      df <- dplyr::full_join(df, newvar, by = "Date")
    }
  }
  if(recessions) {
    df <- get_recessions(df)
  }

  df <- dplyr::arrange(df, Date)

  return(df)
}

add_ecodata <- function(data, varcodes, recessions = FALSE) {
  df <- get_ecodata(varcodes, recessions = FALSE)
  df <- dplyr::full_join(data, df, by = "Date")

  if(recessions | string_detect_any(names(df), "Recession")) {
    df <- get_recessions(df)
  }
  df <- dplyr::arrange(df, Date)
  return(df)
}

# Get unique units from a data frame
ecodata_get_units <- function(df) {
  all_units <- ""
  vars <- names(df)
  for(v in 1:length(vars)) {
    if(!is.null(attr(data[[vars[v]]], "Units"))) {
      all_units[v] <- attr(data[[vars[v]]], "Units")
    }
  }
  unique_units <- unique(all_units)
  unique_units <- unique_units[unique_units != ""]
  return(unique_units)
}

ecodata_get_sources <- function(df) {
  all_sources <- ""
  vars <- names(df)
  for(v in 1:length(vars)) {
    if(!is.null(attr(data[[vars[v]]], "Source"))) {
      all_sources[v] <- attr(data[[vars[v]]], "Source")
    }
  }
  unique_sources <- unique(all_sources)
  unique_sources <- unique_sources[unique_sources != ""]

  return(unique_sources)
}

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

ggplot_ecodata_ts <- function(data, variables = NULL, title="", show.legend = TRUE, plot.recessions = FALSE) {
  linewidth <- 1.5
  linecolor <- "dodgerblue4"
  title_strlen <- 60

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
    includevars <- c(includevars, names(data)[morevars_idx])
  }
  nvar <- length(includevars)

  if(nvar > 7) {
    stop("Number of variables to plot in a time series plot must be 7 or less.")
  }
  if(nvar < 1) {
    stop("There must be at least one variable to plot.")
  }

  plotvars <- includevars

  # Make sure all plot variables have the same units
  for(v in 1:length(plotvars)) {
    all_units[v] <- attr(data[[plotvars[v]]], "Units")
  }
  unique_units <- unique(all_units)
  if(length(unique_units) > 1) {
    msg <- sprintf("Not all variables are in the same units. Units inlude, %s", paste(unique_units, sep = ", ", collapse = ", "))
    stop(msg)
  }

  # Setup a units function for the vertical axis scale labels
  units_function <- function(...) return("")
  if(str_detect(unique_units, "$") | str_detect(str_to_lower(unique_units), "dollar")) {
    units_function <- function(...) return(scales::dollar(...))
  } else if(str_detect(unique_units, "%") | str_detect(str_to_lower(unique_units), "dollar")) {
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
    tidyr::pivot_longer(cols = all_of(plotvars), names_to = "Variable", values_to = "Value")

  # Preserve the order of the variables
  plot.df <- dplyr::mutate(plot.df, Variable = factor(Variable, levels = plotvars, ordered = TRUE))

  plt <- ggplot2::ggplot(plot.df, ggplot2::aes(x = Date, y = Value, color = Variable)) +
    ggplot2::geom_line(linewidth = linewidth) +
    ggplot2::scale_x_date(breaks = scales::pretty_breaks(n=8)) +
    ggplot2::scale_y_continuous(labels = units_function) +
    ggplot2::scale_color_manual(values = mycols) +
    ggplot2::labs(y = unique_units, x = "", color = "", title = title, caption = sources_str) +
    ecodata_theme() +
    ggplot2::theme(legend.position = "bottom", legend.justification = "left") +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = length(plotvars)))  # Each item in a separate row

  if(plot.recessions) {
    plt <- plot + geom_recession(fill="dodgerblue3")
  }

  return(plt)
}

ggplot_ecodata_facet <- function(data, variables = NULL, title="", ncol = 4, scales = "free", color = "dodgerblue4", strip_width = 40, show.legend = TRUE, plot.recessions = FALSE) {
  linewidth <- 1.5
  linecolor <- "dodgerblue4"
  title_strlen <- 60
  if(stringr::str_length(title) > title_strlen) {
    # Put a new line first after a colon
    title <- stringr::str_replace(title, ":", ":\n")
    alllines <- stringr::str_split(title, "\n")[[1]]
    alllines <- stringr::str_wrap(alllines, title_strlen)
    title <- paste(alllines, collapse = "\n")
  }

  includevars <- get_ecodata_varnames(data)
  if(!is.null(variables)) {
    morevars_idx <- names(data) %in% variables
    includevars <- c(includevars, names(data)[morevars_idx])
  }
  nvar <- length(includevars)
  if(nvar < 1) {
    stop("There must be at least one variable to plot.")
  }

  plotvars <- includevars

  plot.df <- data |>
    tidyr::pivot_longer(cols = all_of(plotvars), names_to = "Variable", values_to = "Value")

  # Length of strip text
  plot.df <- dplyr::mutate(plot.df, Variable = stringr::str_wrap(Variable, width = strip_width))
  plotvars <- stringr::str_wrap(plotvars, width = strip_width)

  # Preserve the order of the variables
  plot.df <- dplyr::mutate(plot.df, Variable = factor(Variable, levels = plotvars, ordered = TRUE))

  plt <- ggplot2::ggplot(plot.df, ggplot2::aes(x = Date, y = Value)) +
    ggplot2::geom_line(linewidth = linewidth, color = color) +
    ggplot2::scale_x_date(breaks = scales::pretty_breaks(n=8)) +
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

if(FALSE) {
# Set up a connection to FRED

# fredr::fredr_set_key(Sys.getenv("FRED_API_KEY"))

# Let's get some data!
varcodes <- c("AWHAETP", "SMU55000000500000002", "SMU55291000500000002")
mydata <- get_ecodata(varcodes)

# Plot a time series of the data
ggplot_ecodata_ts(mydata, title = "Weekly Average Hours") +
  geom_recession()

# Make a faceted plot
ggplot_ecodata_facet(mydata, ncol = 2) +
  geom_recession()

# I forgot. Can I add on this variable too? Sure!
mydata <- add_ecodata(mydata, "GDPC1")
mydata <- add_ecodata(mydata, "MEPAINUSA646N")

# Get information about the data
mydata_description <- ecodata_description(mydata)

# Get a pretty table of information about the data
ecodata_description_table(mydata)

# I need to cite my sources
ecodata_cite_table(mydata)
}
