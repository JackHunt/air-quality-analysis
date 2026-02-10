library(optparse)
library(shiny)
library(bslib)
library(ggplot2)
library(patchwork)

plot_ts <- function(df, x, y, title) {
  ggplot(df, aes_string(x = x, y = y)) +
    geom_line() +
    ggtitle(title)
}

plot_raw <- function(df) {
  p1 <- plot_ts(
    df,
    "time_stamp",
    "temperature",
    "Temperature"
  )

  p2 <- plot_ts(
    df,
    "time_stamp",
    "pressure",
    "Pressure"
  )

  p3 <- plot_ts(
    df,
    "time_stamp",
    "humidity",
    "Humidity"
  )

  p1 + p2 + p3
}

run_shiny <- function(args) {
  output <- readRDS(file.path(args$input_path, "output.rds"))
  df <- output$pred

  ui <- pageWithSidebar(
    headerPanel("Air Quality"),
    sidebarPanel(
      dateRangeInput("dates", "Select dates")
    ),
    mainPanel(
      plotOutput("main_plot")
    )
  )

  server <- function(input, output) {
    output$main_plot <- renderPlot(plot_raw(df))
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

  run_shiny(args)
}
