get_metadata <- function (db) {
  address <- "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin"

  address <- paste(address, db, sep = "/")

  request <- rlang::try_fetch(
    httr2::req_perform(httr2::request(address)),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "The database code seems to be incorrect",
          "x" = "You supplied: {db}",
          "i" = "Use explore() to check the database code"
        ),
        parent = cnd
      )
    }
  )

  result <- request |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON() |>
    dplyr::last() # Return only variable metadata

  return (result)
}
