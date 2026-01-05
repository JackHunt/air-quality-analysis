library(optparse)
library(ggplot2)
library(rstan)
options(mc.cores = parallel::detectCores())

load_data <- function(sensor_id) {
  stop()
}

plot_raw <- function() {
  stop()
}

plot_posterior_mean <- function() {
  stop()
}

fit_gp <- function(
  df,
  fit_args,
  advi = FALSE,
  feat_cols = c("TODO")) {
  # TODO: Pull out cols of interest into a matrix

  model_args <- list(
    N = NULL,
    D = NULL
  )

  model <- stan_model("single_sensor_gp.stan")

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
        c("-c", "--input_path"),
        type = "character",
        help = "Path to a directory of RDS files."
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
}