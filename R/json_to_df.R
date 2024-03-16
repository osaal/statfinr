# Construct a tibble from "Vuosi".
# TODO: Extend to one user-named variable
# TODO: Extend from one to many user-named variables
json_to_df <- function(json_response) {
  result <- list(
    "labels" = unlist(json_response$dimension$Vuosi$category$label),
    "values" = unlist(json_response$value)
  ) |>
    do.call(cbind, args = _) |>
    dplyr::tibble()
  return(result)
}
