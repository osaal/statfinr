#' Get data from Statistics Finland databases
#'
#' @description
#' The `get_data` function retrieves database data from Statistics Finland.
#'
#' @param db Code of database to be retrieved
#' @param code Variable(s) to use in filtering the results
#' @param filter Filtering type (see below)
#' @param values Values to filter for
#' @param format Format to return results in. Defaults to `df`
#'
#' @details
#' Define the database to be retrieved from using the argument `db`. If you are
#' unaware of your database's code, use the [explore()] function.
#'
#' The `code` argument defines which variables to filter by. Currently, it only
#' supports a single variable.
#'
#' The `filter` argument can be one of five filter types:
#'
#' - `item`: Return the data for all filter values defined in `values`
#' - `all`: Similar to `item`, but used for wildcard selection with the `*` symbol
#' - `top`: Return the `values` top-most data
#'
#' The values to be filtered by are defined in the `values` argument. Values can
#' be given as character or numeric vectors.
#'
#' The argument `format` defines in which format your results are returned.
#'
#' Currently supported formats are `df` (data frame) and `json` (JSON-formatted).
#'
#' @return Return data type depends on the value of `format`
#' @export
get_data <- function(db = NULL, code = NULL, filter = NULL, values = NULL, format = "df") {
  # Retrieve function arguments as internal variables
  db <- db
  code <- code
  filter <- match.arg(filter, c("item", "all", "top")) # This errors once filter is evaluated!
  # TODO: Handle errors thrown by match.arg
  # TODO: Enforce only one filter verb, not multiple filters (not supported)
  values <- values
  # NB: Perhaps coerce after having checked for nullness below?
  user_format <- format
  # TODO: Save the originally supplied format separately from the format given to httr2

  # Argument handling
  if (is.null(db)) {
    cli::cli_abort(c(
      "{.var db} must be defined",
      "i" = "You entered nothing",
      "i" = "Enter the full ID of a database",
      "i" = "Use the explore() function to find the ID of your preferred database"
    ))
  }

  # Get database metadata for processing later
  metadata <- get_metadata(db)

  # TODO: Build helper function to find database code based on user shortcode or search
  # TODO: Handle multiple codes correctly, currently only functional with one!
  if (is.null(code)) {
    # Default to retrieving all codes
    metadata |>
      dplyr::select(code) |>
      as.list()
  }

  if (is.null(filter)) { filter = "item" }

  if (is.null(values)) {
    # Retrieve all values
    values <- sapply(code, get_values, metadata = metadata)
  }

  # The API only allows characters, so we coerce numeric values to characters
  if (is.numeric(values)) { values <- as.character(values) }

  # This only happens if the user explicitly supplies NULL, since default is "df"
  if (is.null(user_format)) { user_format = "df" }

  # Construct the address
  address <- stringr::str_c(
    "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/",
    db
  )

  # TODO: Integrate user-supplied format with StatFin-returned formats + R-friendly formats!

  # GET query from db
  jsonresponse <- construct_json(address, code, filter, values, format)

  # If user wants a data frame, construct one
  if (user_format == "df") {
    result <- json_to_df(jsonresponse)
  }

  # Return result
  return (result)
}
