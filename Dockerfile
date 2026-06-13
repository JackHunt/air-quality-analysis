FROM mcr.microsoft.com/devcontainers/base:ubuntu

RUN apt-get update && apt-get -y upgrade

RUN apt-get install -y \
  build-essential \
  r-base \
  r-base-dev \
  libxml2-dev \
  libuv1-dev \
  git-lfs \
  libudunits2-dev \
  libgdal-dev \
  libgeos-dev \
  libproj-dev \
  libsqlite3-dev \
  libabsl-dev \
  libharfbuzz-dev \
  libfreetype-dev \
  libfribidi-dev

RUN wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.9.38/quarto-1.9.38-linux-amd64.deb && \
  dpkg -i quarto-1.9.38-linux-amd64.deb

RUN R -e "install.packages('languageserver'); if (!library(languageserver, logical.return=T)) quit(status=10)"
RUN R -e "install.packages('tidyverse'); if (!library(tidyverse, logical.return=T)) quit(status=10)"
RUN R -e "install.packages('GGally'); if (!library(GGally, logical.return=T)) quit(status=10)"
RUN R -e "install.packages('gstat'); if (!library(gstat, logical.return=T)) quit(status=10)"
RUN R -e "install.packages('optparse'); if (!library(optparse, logical.return=T)) quit(status=10)"
RUN R -e "install.packages('shiny'); if (!library(shiny, logical.return=T)) quit(status=10)"
RUN R -e "install.packages('tseries'); if (!library(tseries, logical.return=T)) quit(status=10)"
RUN R -e "install.packages('yaml'); if (!library(yaml, logical.return=T)) quit(status=10)"

RUN R -e "install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos'))); if (!library(cmdstanr, logical.return=T)) quit(status=10)"