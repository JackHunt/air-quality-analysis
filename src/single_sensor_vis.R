library(optparse)
library(shiny)

run_shiny <- function(args) {
  ui <- page_fluid(
    titlePanel("Air Quality"),
    layout_columns(
      col_width = 2,
      card(
        card_header("Date range input"),
        dateRangeInput("dates", "Select dates")
    ),
    )
  )

  server <- function(input, output) {
    #
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