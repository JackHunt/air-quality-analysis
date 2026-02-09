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

plot_raw <- function(df) {
  ggplot(df, aes(time_stamp, pm2.5_alt)) +
    geom_point()
}

plot_posterior_mean_var <- function(t) {
  stop()
}

fit_gp <- function(df_fit) {
  kernel <- k_Periodic(D = 1) * k_Matern52(D = 1)
  #kernel <- k_Periodic(D = 4) * k_RatQuad(D = 4)

  gpkm(
    pm2.5_alt ~ time_stamp,# + temperature + pressure + humidity,
    df_fit,
    kernel = kernel,
    track_optim = TRUE
  )
}

eval_gp <- function(gp, df_fit, df_pred) {
  get_eval_df <- function(df) {
    out <- gp$pred(
      df %>% select(time_stamp),
      se.fit = TRUE
    )

    df %>%
      select(time_stamp, pm2.5_alt) %>%
      mutate(
        mu_f = out$mean,
        var_f = out$s2,
        se_f = out$se
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
                           prediction_proportion) {
  sensor_out_path <- file.path(output_path, sensor_id)
  dir.create(sensor_out_path)

  data <- load_data(
    input_path,
    sensor_id,
    prediction_proportion
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
      args$prediction_proportion
    )
  }
}