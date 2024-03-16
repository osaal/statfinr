construct_json <- function(address, code, filter, values, format) {
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

  # Construct the selection section
  selection <- list(
    filter = filter,
    values = values
  )

  # Construct the full JSON query
  jsonget <- list(
    query = list(list(
      code = code,
      selection = selection
    )),
    response = response
  )

  # Execute JSON query using address and return JSON response
  json_response <- httr2::request(address) |>
    httr2::req_body_json(jsonget) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  return(json_response)
}
