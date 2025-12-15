library(httr2)
library(jsonlite)
library(optparse)
library(yaml)

do_get <- function(id, fields, time_avg, api_key) {
  fields_str <- head(c(rbind(fields, c("%2C"))), -1)
  fields_str <- paste(unlist(fields_str), collapse = "")

  url <- paste0(
    "https://api.purpleair.com/v1/sensors/:",
    id,
    "/history?",
    paste0("average=", time_avg, "&"),
    paste0("fields=", fields_str)
  )

  req <- request(url) |>
    req_headers(`X-API-KEY` = api_key)

  req
}

retrieve_data <- function(config_path, output_path, api_key) {
  config <- read_yaml(config_path)

  print(do_get("WCAN-105", config$fields, config$time_average, api_key))
  stop()

  lapply(config$sensors, function(id) {
    d <- do_get(id, config$fields, config$time_average, api_key)
  })
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
        help = "Path to an outpur RDS file."
      )
    ),
    description = "PurpleAir Data Retrieval (via the REST API)."
  )

  args <- parse_args(opt_parser)

  keys <- read_yaml("./keys.yaml")

  retrieve_data(args$config_path, args$output_path, keys$purple_air)
}
