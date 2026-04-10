library(dplyr)

load_purple_air <- function(path,
                            sensor_id,
                            prediction_proportion = -1.0,
                            drop_proportion = 0.0) {
  df <- readRDS(paste0(path, "/", sensor_id, ".rds")) %>%
    select(
      sensor_index,
      time_stamp,
      temperature,
      pressure,
      humidity,
      pm2.5_alt,
      pm10.0_atm,
      latitude,
      longitude
    ) %>%
    arrange(time_stamp) %>%
    mutate(
      original_time_stamp = time_stamp,
      date = as.POSIXct(time_stamp, origin = "1970-01-01", tz = "GMT"),
      log_pm2.5_alt = log(pm2.5_alt),
      log_pm10.0_atm = log(pm10.0_atm)
    ) %>%
    mutate(time_stamp = (time_stamp - min(time_stamp)) / 1000) %>%
    distinct()

  if (drop_proportion > 0.0) {
    n <- nrow(df)
    n_drop <- floor(n * drop_proportion)
    df <- df %>% slice(n_drop : n)
  }

  if (prediction_proportion <= 0) {
    return(df)
  }

  n <- nrow(df)
  n_fit <- floor(n * (1.0 - prediction_proportion))

  list(
    fit = df %>% slice(1 : n_fit),
    pred = df %>% slice(n_fit + 1 : n)
  )
}

get_sensor_ids <- function(data_dir) {
  lapply(
    list.files(
      data_dir,
      pattern = "\\.rds$",
      ignore.case = TRUE
    ),
    function(fname) {
      sub("\\.rds$", "", fname)
    }
  )
}

load_purple_air_all <- function(input_path,
                                prediction_proportion = -1.0,
                                drop_proportion = 0.0) {
  sensor_ids <- get_sensor_ids(args$input_path)

  dfs <- lapply(sensor_ids, function(sensor_id) {
    load_purple_air(
      input_path,
      sensor_id,
      prediction_proportion,
      drop_proportion
    )
  })

  # rbind the fit and pred dfs

  list(
    df_fit = NULL,
    df_pred = NULL
  )
}
