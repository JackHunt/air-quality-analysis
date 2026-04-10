source("src/data_load.R")

library(dplyr)
library(gstat)
library(optparse)

fit_spatiotemporal_kriging <- function(df) {
  0
}

if (!interactive()) {
  opt_parser <- OptionParser(
    option_list = list(
      make_option(
        c("-i", "--input_path"),
        type = "character",
        help = "Path to a directory of RDS files."
      ),
      make_option(
        c("-p", "--prediction_proportion"),
        type = "double",
        default = 0.1,
        help = "Proportion of last measurements to predict on (e.g. 0.1 predicts on last 10%)."
      ),
      make_option(
        c("-d", "--drop_proportion"),
        type = "double",
        default = 0.0,
        help = "Proportion of the dataset to skip (prior to splitting)."
      ),
      make_option(
        c("-o", "--output_path"),
        type = "character",
        help = "Path to an output directory.",
        default = "./out"
      )
    ),
    description = "Analyse data for multilple sensors (spatiotemporal)."
  )

  args <- parse_args(opt_parser)

  #dir.create(args$output_path)

  df <- load_purple_air_all(args$input_path)

  print(df)

  loaded_ids <- df %>%
    select(sensor_index) %>%
    distinct()

  print(loaded_ids)
}
