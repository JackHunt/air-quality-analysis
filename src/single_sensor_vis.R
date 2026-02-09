library(optparse)
library(shiny)
library(bslib)

run_shiny <- function(args) {
  ui <- pageWithSidebar(
    headerPanel("Air Quality"),
    sidebarPanel(
      dateRangeInput("dates", "Select dates")
    ),
    mainPanel(
      #
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
