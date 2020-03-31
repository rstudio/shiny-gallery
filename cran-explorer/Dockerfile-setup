FROM rocker/rstudio-stable:3.5.2

# install additional system dependencies
RUN apt-get update && apt-get install -y libxml2-dev zlib1g-dev libgit2-dev


# set CRAN repo to the RStudio mirror
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site

# install packrat
RUN Rscript -e "install.packages('packrat')"

CMD ["/init"]
