library(httr2)
library(optparse)
library(yaml)

do_get_open_weather <- function(ts, lat, lon, api_key) {
  url <- "https://api.openweathermap.org/data/3.0/onecall/timemachine"

  res <- request(url) |>
    req_url_query(lat = lat) |>
    req_url_query(lon = lon) |>
    req_url_query(dt = ts) |>
    req_url_query(appid = api_key) |>
    req_perform(verbosity = 3) |>
    resp_body_json()
}

retrieve_open_weather_data <- function(config, output_path, api_key) {
  for (site in config$sites) {
    start_ts <- as.numeric(as.POSIXct(paste(config$start_date, "00:00:00")))
    end_ts <- as.numeric(as.POSIXct(paste(config$end_date, "23:00:00")))

    timestamps <- seq(start_ts - 3600, end_ts + 3600, by = 3600)

    res <- lapply(timestamps, function(ts) {
      do_get_open_weather(
        ts,
        site$latitude,
        site$longitude,
        api_key
      )
    })

    res <- rbind(res)

    output_fname <- paste0(output_path, "/", site$name, ".rds")
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
        default = "./out/weather"
      )
    ),
    description = "Open Weather Data Retrieval (via the REST API)."
  )

  args <- parse_args(opt_parser)

  keys <- read_yaml("./keys.yaml")
  config <- read_yaml(args$config_path)

  retrieve_open_weather_data(
    config,
    args$output_path,
    keys$open_weather
  )
}
