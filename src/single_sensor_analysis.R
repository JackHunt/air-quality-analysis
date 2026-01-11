library(dplyr)
library(optparse)
library(ggplot2)
library(rstan)
options(mc.cores = parallel::detectCores())

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
  df_pred,
  fit_args,
  advi = FALSE
) {
  X <- df_fit %>%
    select(-pm2.5_alt) %>%
    as.matrix()
  
  Y <- df_fit %>%
    pull(pm2.5_alt)

  model_args <- list(
    N = nrow(X),
    D = ncol(X),
    X = X,
    Y = Y
  )

  model <- stan_model("src/single_sensor_gp.stan")

  if (advi) {
    vb(
      model,
      data = model_args,
      iter = fit_args$n_iter,
      seed = fit_args$seed,
      output_samples = fit_args$n_output_samples,
      tol_rel_obj = 1e-4
    )
  } else {
    sampling(
      model,
      data = model_args,
      iter = fit_args$n_iter,
      chains = fit_args$n_chains,
      seed = fit_args$seed
    )
  }
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
      ),
      make_option(
        c("-a", "--advi"),
        action = "store_true",
        help = "Use variational inference instead of HMC.")
    ),
    description = "Analyse data for a single sensor."
  )

  args <- parse_args(opt_parser)

  df <- load_data(
    args$input_path,
    args$sensor_id,
    args$prediction_proportion
  )

  fit_args <- list(
    n_iter = 1000,
    seed = 1234,
    n_output_samples = 1000,
    n_chains = 4
  )

  gp <- fit_gp(
    df$fit,
    df$pred,
    fit_args,
    advi = args$advi
  )
}