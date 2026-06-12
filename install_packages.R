install.packages(
  c(
    "GGally",
    "dplyr",
    "ggplot2",
    "gplite",
    "gstat",
    "httr2",
    "optparse",
    "shiny",
    "tseries",
    "yaml"
  ),
  repos = "https://www.stats.bris.ac.uk/R/"
)

install.packages(
  c(
    "cmdstanr"
  ),
  repos = c(
    "https://stan-dev.r-universe.dev",
    getOption("repos")
  )
)
