library(optparse)
library(shiny)
library(bslib)
library(ggplot2)
library(GGally)

plot_posterior_mean <- function(df, split_date = NULL) {
  p <- ggplot(df, aes(x = date)) +
    geom_line(aes(y = pm2.5_alt, color = "True PM2.5")) +
    geom_line(aes(y = mu, color = "Posterior Mean PM2.5")) +
    geom_ribbon(
      aes(
        ymin = mu - 2 * sqrt(sigma),
        ymax = mu + 2 * sqrt(sigma)
      ),
      alpha = 0.25
    ) +
    ggtitle("PM2.5 Prediction")

  if (!is.null(split_date)) {
    p <- p +
      geom_vline(xintercept = split_date, linetype = "dashed") +
      annotate("text", x = split_date, y = 10.5, label = "OOS")
  }

  p
}

plot_rsq <- function(fit_metrics, pred_metrics) {
  ggplot(NULL, aes(x = fit_metrics$r_sq, fill = "Fit")) +
    geom_histogram() +
    geom_histogram(aes(x = pred_metrics$r_sq, fill = "Pred")) +
    ggtitle("R^2")
}

plot_gp <- function(input, output, gp) {
  df <- rbind(gp$fit, gp$pred)
  split_date <- tail(gp$fit, n = 1)$date

  output$gp_plot <- renderPlot(plot_posterior_mean(df, split_date))
}

plot_ar_p <- function(input, output, ar_p) {
  df <- rbind(ar_p$df_fit, ar_p$df_pred)
  split_date <- tail(ar_p$df_fit, n = 1)$date
  output$ar_p_plot <- renderPlot(plot_posterior_mean(df, split_date))
  output$ar_p_rsq <- renderPlot(plot_rsq(ar_p$eval_fit, ar_p$eval_pred))
}

plot_raw <- function(input, output, raw) {
  plot_ts <- function(df, var) {
    ggplot(df, aes(x = date, y = .data[[var]])) +
      geom_line() +
      ggtitle(paste("Raw", var))
  }

  df <- rbind(raw$fit, raw$pred)

  output$raw_pm2.5 <- renderPlot(plot_ts(df, "pm2.5_alt"))
  output$raw_temperature <- renderPlot(plot_ts(df, "temperature"))
  output$raw_pressure <- renderPlot(plot_ts(df, "pressure"))
  output$raw_humidity <- renderPlot(plot_ts(df, "humidity"))
}

eda <- function(input, output, raw) {
  plot_pairs <- function(df) {
    ggpairs(
      df,
      columns = c(
        "pm2.5_alt",
        "temperature",
        "pressure",
        "humidity"
      ),
      lower = list(
        continuous = "smooth",
        combo = "facetdensity",
        mapping = aes(color = date)
      )
    )
  }

  df <- rbind(raw$fit, raw$pred)

  output$eda_pairs_plot <- renderPlot(plot_pairs(df))
}

run_shiny <- function(args) {
  raw_data <- readRDS(file.path(args$input_path, "input_data.rds"))
  gp <- readRDS(file.path(args$input_path, "gp.rds"))
  ar_p <- readRDS(file.path(args$input_path, "ar_p.rds"))

  ui <- page_fillable(
    headerPanel("Air Quality"),
    navset_card_tab(
      nav_panel(
        "Raw Data",
        mainPanel(
          plotOutput("raw_pm2.5"),
          plotOutput("raw_temperature"),
          plotOutput("raw_pressure"),
          plotOutput("raw_humidity")
        )
      ),
      nav_panel(
        "EDA",
        mainPanel(
          plotOutput("eda_pairs_plot")
        )
      ),
      nav_panel(
        "GP",
        mainPanel(
          plotOutput("gp_plot")
        )
      ),
      nav_panel(
        "AR(p)",
        mainPanel(
          plotOutput("ar_p_plot"),
          plotOutput("ar_p_rsq")
        )
      )
    )
  )

  server <- function(input, output) {
    plot_raw(input, output, raw_data)
    eda(input, output, raw_data)
    plot_gp(input, output, gp)
    plot_ar_p(input, output, ar_p)
  }

  shinyApp(ui = ui, server = server)
}

if (!interactive()) {
  opt_parser <- OptionParser(
    option_list = list(
      make_option(
        c("-i", "--input_path"),
        type = "character",
        help = "Directory of GP outputs."
      )
    )
  )

  args <- parse_args(opt_parser)

  options(shiny.port = 8100)
  run_shiny(args)
}
