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
#' The values to be filtered by are defined in the `values` argument. Note,
#' that values *must* be entered as character strings!
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
  filter <- match.arg(filter, c("item", "all", "top")) # Will this error here or when filter is called?
  # TODO: Handle errors thrown by match.arg
  # TODO: Enforce only one filter verb, not multiple filters (not supported)
  values <- values
  # TODO: Coerce values to strings (as the API always uses strings; or at least looks like that...)
  # NB: Perhaps coerce after having checked for nullness below?
  user_format <- format
  # TODO: Save the originally supplied format separately from the format given to httr2

  # Argument handling
  if (is.null(db)) {
    cli::cli_abort(c(
      "{.var db} must be defined",
      "i" = "You entered {db}",
      "i" = "Enter the full ID of a database",
      "i" = "Use the explore() function to find the ID of your preferred database"
    ))
  }

  if (is.null(code)) {
    # Default to retrieving all codes
  }

  if (is.null(filter)) { filter = "item" } # THIS CANNOT EVALUATE RIGHT NOW BECAUSE OF MATCH.ARG IN THE START

  if (is.null(values)) {
    # Retrieve all values
  }

  if (is.null(user_format)) { user_format = "df" } # This only happens if the user explicitly supplies NULL

  # Construct the address
  address <- stringr::str_c(
    "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/",
    db
  )

  # Construct the selection rules for one code, using a list of values
  selection <- list(
    filter = filter,
    values = values
  )

  # Construct the response formatting
  if (user_format %in% c("df", "json")) {
    response = list(
      format = "json-stat2"
    )
  } else {
    # TODO: Add other options than df or json
    # Could be done with switch()?
    cli::cli_abort(c(
      "{.var format} must be either 'df' or 'json'",
      "i" = "You entered {format}"
    ))
  }

  # Construct the GET request in json-formatting
  jsonget <- list(
    query = list(list(
      code = code,
      selection = selection
    )),
    response = response
  )

  # code: Supplied by the user.
  # TODO: Build helper function to find database code based on user shortcode or search
  # TODO: Handle multiple codes correctly, currently only functional with one!
  # filter: The way in which the user is searching. Can be "item", "all", "top", "agg", "vs" (last two uncertain!)
  # values: The values that the user is retrieving data for. Depends on code and filter.

  # Use user-submitted response format
  # format: The format in which the response is returned. Defaults to data frame for user?
  # TODO: Integrate user-supplied format with StatFin-returned formats + R-friendly formats!

  # GET query from db
  jsonresponse <- construct_json(address, jsonget)

  # If user wants a data frame, construct one
  if (user_format == "df") {
    result <- json_to_df(jsonresponse)
  }

  # Return result
  return (result)
}
