construct_json <- function(address, jsonget) {
  json_response <- httr2::request(address) |>
    httr2::req_body_json(jsonget) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  return(json_response)
}
