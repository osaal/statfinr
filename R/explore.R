#' Explore StatFin databases
#'
#' @description
#' `explore` returns the IDs and names of StatFin databases
#'
#' @details
#' The function can be called without arguments or with the `db` argument.
#' Without arguments, it returns the StatFin main databases. If supplied a
#' specific database ID with the `db` argument, it returns the sub-databases
#' for that database.
#'
#' @param db The id of a specific database (optional)
#'
#' @returns A data frame containing the `id` and `name` of the databases
#' @export
#' @importFrom rlang .data
explore <- function(db = NULL) {
  # TODO: Write unit tests
  address <- "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/"

  if (!is.null(db)) {
    # Throw error if db contains anything but letters and numbers
    if (stringr::str_detect(db, pattern = "[\\s[:punct:]]")) {
      cli::cli_abort(c(
        "{.var db} must only contain alphanumeric characters",
        "i" = "You entered {db}",
        "i" = "Enter the id of a valid database to retrieve its sub-databases"
      ))
    }

    address <- paste(
      address,
      db,
      sep = ""
    )
  }

  result <- httr2::request(address) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    Reduce(
      rbind,
      x = _
    ) |>
    data.frame() |>
    dplyr::select(.data$id, .data$text) |>
    tibble::remove_rownames()

  return(result)
}
