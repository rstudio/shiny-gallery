# installing package imports packages
pkg_list <- c("mapview", "sf", "leafpop", "rgeos", "rnaturalearth")

install.packages(pkgs = pkg_list, repos = "https://cran.rstudio.com/")

install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")

for(i in pkg_list){

  if(!i %in% rownames(installed.packages())){
    stop(paste("Package", i, "is not available"))
  }
}
