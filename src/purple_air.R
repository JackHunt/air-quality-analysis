library(dplyr)
library(httr2)
library(optparse)
library(yaml)

do_get <- function(id, fields, time_avg, api_key) {
  fields_str <- paste(unlist(fields), collapse = ",")

  url <- paste0(
    "https://api.purpleair.com/v1/sensors/",
    id,
    "/history/csv"
  )

  res <- request(url) |>
    req_url_query(average = time_avg) |>
    req_url_query(fields = fields_str) |>
    req_headers(`X-API-Key` = api_key) |>
    req_perform(verbosity = 3) |>
    resp_body_string()

  read.csv(text = res)
}

retrieve_data <- function(config_path, output_path, api_key) {
  config <- read_yaml(config_path)

  res <- lapply(config$sensors, function(sensor) {
    do_get(sensor$id, config$fields, config$time_average, api_key)
  })

  res_combined <- bind_rows(res)
  saveRDS(res_combined, output_path)
}

if (!interactive()) {
  opt_parser <- OptionParser(
    option_list = list(
      make_option(
        c("-c", "--config_path"),
        type = "character",
        help = "Path to a YAML config file."
      ),
      make_option(
        c("-o", "--output_path"),
        type = "character",
        help = "Path to an outpur RDS file.",
        default = "./out"
      )
    ),
    description = "PurpleAir Data Retrieval (via the REST API)."
  )

  args <- parse_args(opt_parser)

  keys <- read_yaml("./keys.yaml")

  retrieve_data(args$config_path, args$output_path, keys$purple_air)
}
