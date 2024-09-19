FROM rocker/tidyverse:4.4.1

# Install jq to parse json files
RUN apt-get update && apt-get install -y \
# To install terra
  libgdal-dev \
  gdal-bin \
  libgeos-dev \
  libproj-dev \
  libsqlite3-dev