library(optparse)
library(shiny)
library(bslib)
library(ggplot2)
library(patchwork)

plot_raw <- function(df) {
  plot_ts <- function(x, y, title) {
    ggplot(df, aes_string(x = x, y = y)) +
      geom_line() +
      ggtitle(title)
  }

  p1 <- plot_ts(
    "date",
    "temperature",
    "Temperature"
  )

  p2 <- plot_ts(
    "date",
    "pressure",
    "Pressure"
  )

  p3 <- plot_ts(
    "date",
    "humidity",
    "Humidity"
  )

  p1 + p2 + p3
}

plot_posterior_mean <- function(df) {
  ggplot(df, aes(x = date)) +
    geom_line(aes(y = mu_f, color = "Posterior Mean")) +
    geom_ribbon(
      aes(
        ymin = mu_f - 2 * sqrt(var_f),
        ymax = mu_f + 2 * sqrt(var_f)
      ),
      alpha = 0.1
    ) +
    geom_line(aes(y = pm2.5_alt, color = "True PM2.5")) +
    ggtitle("PM2.5 Prediction")
}

run_shiny <- function(args) {
  output <- readRDS(file.path(args$input_path, "output.rds"))
  df_pred <- output$pred
  df_fit <- output$fit

  ui <- pageWithSidebar(
    headerPanel("Air Quality"),
    sidebarPanel(
      dateRangeInput("dates", "Select dates")
    ),
    mainPanel(
      plotOutput("raw_plot"),
      plotOutput("pred_plot"),
      plotOutput("fit_plot"),
    )
  )

  server <- function(input, output) {
    output$raw_plot <- renderPlot(plot_raw(df_pred))
    output$pred_plot <- renderPlot(plot_posterior_mean(df_pred))
    output$fit_plot <- renderPlot(plot_posterior_mean(df_fit))
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
