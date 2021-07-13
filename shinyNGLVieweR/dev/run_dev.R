#setwd("C:/R/Github repositories/shinyNGLVieweR")

# Set options here
options(golem.app.prod = TRUE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Change package name 
# golem::set_golem_name("shinyNGLVieweR")
# golem::set_golem_wd(path = pkgload::pkg_path(), talkative = TRUE)
# golem::get_golem_wd()

options(shiny.port = 1235)
options(shiny.autoreload = TRUE)
options(r2d3.shadow = FALSE)
# Run the application
run_app()
