#!/bin/bash

Rscript -e "packrat::on(); source('src/data/download_metacran_db.R')" & \
Rscript -e "packrat::on(); source('src/data/pkg_dependencies.R')" & \
Rscript -e "packrat::on(); source('src/data/pkg_releases.R')" & \
Rscript -e "packrat::on(); source('src/data/prepare_data.R')"

# copy data to app/data
cp data/pkg_dependencies.csv data/pkg_releases.csv data/tile_summary.rds app/data
