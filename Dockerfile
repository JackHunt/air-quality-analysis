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
  libabsl-dev

RUN wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.9.38/quarto-1.9.38-linux-amd64.deb && \
  dpkg -i quarto-1.9.38-linux-amd64.deb