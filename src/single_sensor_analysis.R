library(dplyr)
library(optparse)
library(ggplot2)
library(GauPro)

load_data <- function(path, sensor_id, prediction_proportion) {
  df <- readRDS(paste0(path, "/", sensor_id, ".rds")) %>%
    select(
      time_stamp,
      temperature,
      pressure,
      humidity,
      pm2.5_alt
    ) %>%
    arrange(time_stamp) %>%
    mutate(time_stamp = time_stamp - min(time_stamp)) %>%
    mutate(time_stamp = time_stamp / 1000) %>%
    distinct()

  n <- nrow(df)
  n_fit <- floor(n * (1.0 - prediction_proportion))

  list(
    fit = df %>% slice(1 : n_fit),
    pred = df %>% slice(n_fit + 1 : n)
  )
}

plot_raw <- function() {
  stop()
}

plot_posterior_mean <- function() {
  stop()
}

fit_gp <- function(
  df_fit,
  df_pred
) {
  kernel <- k_Periodic(D = 1) * k_Matern52(D = 1)
  #kernel <- k_Periodic(D = 4) * k_RatQuad(D = 4)

  gp <- gpkm(
    pm2.5_alt ~ time_stamp,# + temperature + pressure + humidity,
    df_fit,
    kernel = kernel,
    track_optim = TRUE
  )

  gp
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
        c("-s", "--sensor_id"),
        type = "character",
        help = "ID of the sensor to analyse."
      ),
      make_option(
        c("-p", "--prediction_proportion"),
        type = "double",
        default = 0.1,
        help = "Proportion of last measurements to predict on (e.g. 0.1 predicts on last 10%)."
      ),
      make_option(
        c("-o", "--output_path"),
        type = "character",
        help = "Path to an output directory.",
        default = "./out"
      )
    ),
    description = "Analyse data for a single sensor."
  )

  args <- parse_args(opt_parser)

  df <- load_data(
    args$input_path,
    args$sensor_id,
    args$prediction_proportion
  )

  gp <- fit_gp(
    df$fit,
    df$pred
  )

  saveRDS(gp, "fit.rds")

  print(summary(gp))

  #gp$plot()
  gp$plot1D()

  out_fit <- gp$pred(gp$X)
  mu_f <- out_fit$mean
  var_f <- out_fit$s2
}