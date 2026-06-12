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