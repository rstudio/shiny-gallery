#!/bin/bash

# install depedencies for httpgd
apt-get -y install --no-install-recommends libfreetype6-dev libfontconfig1-dev

# Use littler to install packages
install2.r languageserver renv remotes
installGithub.r nx10/httpgd

# establish renv environment variables in users .Renviron file
mkdir -p /renv/cache
echo "RENV_PATHS_CACHE=/renv/cache" >> /usr/local/lib/R/etc/Renviron
