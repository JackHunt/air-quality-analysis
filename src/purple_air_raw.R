library(dplyr)
library(httr2)
library(optparse)
library(yaml)

do_get <- function(id, fields, time_avg, api_key, existing_df = NULL) {
  fields_str <- paste(unlist(fields), collapse = ",")

  url <- paste0(
    "https://api.purpleair.com/v1/sensors/",
    id,
    "/history/csv"
  )

  start_ts <- 1 # TODO: Check this will actually work.
  if (!is.null(existing_df)) {
    start_ts <- tail(existing_df, n = 1)$time_stamp
  }

  res <- request(url) |>
    req_url_query(average = time_avg) |>
    req_url_query(fields = fields_str) |>
    req_url_query(start_timestamp = start_ts) |>
    req_headers(`X-API-Key` = api_key) |>
    req_perform(verbosity = 3) |>
    resp_body_string()

  read.csv(text = res) %>%
    mutate(across(everything(), as.numeric))
}

do_get_metadata <- function(id, api_key) {
  fields_str <- paste(
    c(
      "location_type", # 0 is outside
      "latitude",
      "longitude",
      "altitude"
    ),
    collapse = ","
  )

  url <- paste0(
    "https://api.purpleair.com/v1/sensors/",
    id
  )

  request(url) |>
    req_url_query(fields = fields_str) |>
    req_headers(`X-API-Key` = api_key) |>
    req_perform(verbosity = 3) |>
    resp_body_json()
}

retrieve_data <- function(config_path, output_path, api_key) {
  config <- read_yaml(config_path)

  for (sensor in config$sensors) {
    output_fname <- paste0(output_path, "/", sensor$id, ".rds")

    existing_df <- NULL
    if (file.exists(output_fname)) {
      existing_df <- readRDS(output_fname)
      warning(paste(output_fname, "already exists and will be updated."))
    }

    metadata <- do_get_metadata(sensor$id, api_key)

    res <- do_get(
      sensor$id,
      config$fields,
      config$time_average,
      api_key,
      existing_df = existing_df
    ) %>%
      mutate(
        sensor_name = metadata$sensor$name,
        is_indoor = metadata$sensor$location_type,
        latitude = metadata$sensor$latitude,
        longitude = metadata$sensor$longitude
      )

    if (!is.null(existing_df)) {
      res <- bind_rows(existing_df, res) %>%
        distinct()
    }

    saveRDS(res, output_fname)
  }
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
        help = "Path to an output directory.",
        default = "./out"
      )
    ),
    description = "PurpleAir Data Retrieval (via the REST API)."
  )

  args <- parse_args(opt_parser)

  keys <- read_yaml("./keys.yaml")

  retrieve_data(args$config_path, args$output_path, keys$purple_air)
}
