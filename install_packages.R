install.packages(
  c(
    "dplyr",
    "ggplot2",
    "gplite",
    "httr2",
    "optparse",
    "shiny",
    "yaml"
  ),
  repos = "https://www.stats.bris.ac.uk/R/"
)

install.packages(
  c(
    "cmdstanr",
    "shinystan"
  ),
  repos = c(
    "https://stan-dev.r-universe.dev",
    getOption("repos")
  )
)
