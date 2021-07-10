FROM rocker/r-ver:3.6.0
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libssh2-1-dev libssl-dev libxml2-dev make zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'install.packages("golem",upgrade="never")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "0.8.5")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "1.5")'
RUN Rscript -e 'remotes::install_version("shinythemes",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.5.1")'
RUN Rscript -e 'remotes::install_github("hadley/emo@3f03b11491ce3d6fc5601e210927eff73bf8e350")'
RUN Rscript -e 'remotes::install_github("news-r/fopi@efce5c2c1361eb74f71251b58587d0963812a086")'
RUN Rscript -e 'remotes::install_github("JohnCoene/typed@31e6d3be6a82486c891fbb615b83811cf698aa5a")'
RUN Rscript -e 'remotes::install_github("RinteRface/fullPage@acc62332aeceaca5422643a2355790a1f2b942fa")'
RUN Rscript -e 'remotes::install_github("JohnCoene/echarts4r@db1a1f30c50d4837540ce529c4d07f92079311d0")'
RUN Rscript -e 'remotes::install_github("rstudio/promises@627dfc6a9189575265c744e700efcff4de94cd19")'
RUN Rscript -e 'remotes::install_github("r-lib/usethis@942c09710174b2abbe0b26dd335e1ebe338f7b82")'
RUN R -e 'remotes::install_github("news-r/fopi.app")'

EXPOSE 3838
CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');fopi.app::run_fopi()"]
