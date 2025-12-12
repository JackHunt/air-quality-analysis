library(optparse)

retrieve_data <- function() {
  #
}

if (!interactive()) {
  opt_parser <- OptionParser(
    option_list = list(
      make_option(
        #
      ),
      make_option(
        #
      )
    ),
    description = "PurpleAir Data Retrieval (via the REST API)."
  )
}
