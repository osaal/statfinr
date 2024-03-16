get_metadata <- function (db) {
  address <- "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/"

  address <- paste(
    address,
    db,
    sep = "/"
  )

  result <- httr2::request(address) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON() |>
    dplyr::last() # Return only the variable metadata

  return (result)
}
