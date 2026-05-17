library(optparse)
library(yaml)

do_get_met_office <- function(freq, date, lat, lon, api_key) {
  url <- paste0(
    "https://data.hub.api.metoffice.gov.uk/sitespecific/v0/point/",
    freq
  )

  request(url) |>
    req_url_query(fields = fields_str) |>
    req_headers(
      apikey = api_key,
      accept = "application/json"
    ) |>
    req_perform(verbosity = 3) |>
    resp_body_json()
}

retrieve_met_office_data <- function(config, output_path, api_key) {
  for (site in config$sites) {
    output_fname <- paste0(output_path, "/", site$name, ".rds")

    dates <- NULL # TODO

    res <- lapply(dates, function(date) {
      do_get_met_office(
        config$frequency,
        date,
        site$latitude,
        site$longitude,
        api_key
      )
    })

    res <- rbind(res)
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
    description = "Met Office Data Retrieval (via the REST API)."
  )

  args <- parse_args(opt_parser)

  keys <- read_yaml("./keys.yaml")
  config <- read_yaml("./config/met_office.yaml")

  retrieve_met_office_data(
    args$config_path,
    args$output_path,
    keys$met_office
  )
}
