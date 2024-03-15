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
#' @param db The id of a specific database
#'
#' @returns A data frame containing the `id` and `name` of the databases
#' @export
explore <- function(db = NULL) {
  address <- "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/"

  if (!is.null(db)) {
    address <- paste(
      address,
      db,
      sep = ""
    )
  }

  result <- request(address) |>
    req_perform() |>
    resp_body_json() |>
    Reduce(
      rbind,
      x = _
    ) |>
    data.frame() |>
    dplyr::select(id, text) |>
    tibble::remove_rownames()

  return(result)
}
