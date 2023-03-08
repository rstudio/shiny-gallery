# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue butto7cidn on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
options(r2d3.shadow = FALSE)
tempdir(check=TRUE)
shinyNGLVieweR::run_app() # add parameters here (if any)