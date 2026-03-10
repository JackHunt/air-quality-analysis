library(optparse)
library(shiny)
library(bslib)
library(ggplot2)

plot_gp <- function(input, output, gp) {
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
      geom_line(aes(y = log_pm2.5_alt, color = "True PM2.5")) +
      ggtitle("PM2.5 Prediction")
  }

  df_fit <- gp$fit
  df_pred <- gp$pred

  output$pred_plot <- renderPlot(plot_posterior_mean(df_pred))
  output$fit_plot <- renderPlot(plot_posterior_mean(df_fit))
}

plot_ar_p <- function(input, output, ar_p) {
  NULL
}

plot_arma <- function(input, output, gp) {
  NULL
}

run_shiny <- function(args) {
  gp <- readRDS(file.path(args$input_path, "gp.rds"))
  ar_p <- readRDS(file.path(args$input_path, "ar_p.rds"))
  arma <- readRDS(file.path(args$input_path, "arma.rds"))

  ui <- page_fillable(
    headerPanel("Air Quality"),
    navset_card_tab(
      nav_panel(
        "GP",
        mainPanel(
          plotOutput("fit_plot"),
          plotOutput("pred_plot"),
        )
      ),
      nav_panel(
        "AR(p)",
        mainPanel(
          plotOutput("ar_p_plot")
        )
      ),
      nav_panel(
        "ARMA",
        mainPanel(
          plotOutput("arma_plot")
        )
      )
    )
  )

  server <- function(input, output) {
    plot_gp(input, output, gp)
    plot_ar_p(input, output, ar_p)
    plot_arma(input, output, arma)
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
