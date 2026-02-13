library(dplyr)
library(optparse)
library(gplite)

load_data <- function(path, sensor_id, prediction_proportion, drop_proportion) {
  df <- readRDS(paste0(path, "/", sensor_id, ".rds")) %>%
    select(
      time_stamp,
      temperature,
      pressure,
      humidity,
      pm2.5_alt
    ) %>%
    arrange(time_stamp) %>%
    mutate(
      original_time_stamp = time_stamp,
      date = as.Date(as.POSIXct(time_stamp, origin = "1970-01-01"))
    ) %>%
    mutate(time_stamp = (time_stamp - min(time_stamp)) / 1000) %>%
    distinct()

  if (drop_proportion > 0.0) {
    n <- nrow(df)
    n_drop <- floor(n * drop_proportion)
    df <- df %>% slice(n_drop : n)
  }

  n <- nrow(df)
  n_fit <- floor(n * (1.0 - prediction_proportion))

  list(
    fit = df %>% slice(1 : n_fit),
    pred = df %>% slice(n_fit + 1 : n)
  )
}

fit_gp <- function(df_fit) {
  kernels <- list(
    cf_periodic(),
    cf_matern52(),
    cf_lin() * cf_periodic()
  )

  gp <- gp_init(
    cfs = kernels,
    lik = lik_gaussian()
  )

  gp <- gp_optim(
    gp,
    df_fit$time_stamp,
    df_fit$pm2.5_alt,
    max_iter = 5000,
    restarts = 5,
    verbose = FALSE
  )

  gp
}

eval_gp <- function(gp, df_fit, df_pred) {
  get_eval_df <- function(df) {
    out <- gp_pred(
      gp,
      df$time_stamp,
      var = TRUE
    )

    df %>%
      select(date, temperature, pressure, humidity, pm2.5_alt) %>%
      mutate(
        mu_f = out$mean,
        var_f = out$var,
      )
  }

  list(
    fit = get_eval_df(df_fit),
    pred = get_eval_df(df_pred)
  )
}

analyse_sensor <- function(input_path,
                           sensor_id,
                           output_path,
                           prediction_proportion,
                           drop_proportion) {
  sensor_out_path <- file.path(output_path, sensor_id)
  dir.create(sensor_out_path)

  data <- load_data(
    input_path,
    sensor_id,
    prediction_proportion,
    drop_proportion
  )

  gp <- fit_gp(data$fit)
  saveRDS(gp, file.path(sensor_out_path, "gp.rds"))

  out <- eval_gp(gp, data$fit, data$pred)
  saveRDS(out, file.path(sensor_out_path, "output.rds"))
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
        help = "ID of the sensor to analyse.",
        default = NA
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
    description = "Analyse data for a single sensor."
  )

  args <- parse_args(opt_parser)

  dir.create(args$output_path)

  if (is.na(args$sensor_id)) {
    sensor_ids <- lapply(
      list.files(pattern = "\\.rds$", ignore.case = TRUE),
      function(fname) {
        sub("\\.rds$", "", fname)
      }
    )
  } else {
    sensor_ids <- c(args$sensor_id)
  }

  for (id in sensor_ids) {
    analyse_sensor(
      args$input_path,
      id,
      args$output_path,
      args$prediction_proportion,
      args$drop_proportion
    )
  }
}
