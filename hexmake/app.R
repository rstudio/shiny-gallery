# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
# 
pkgload::load_all(path = here::here("hexmake/hexmake"), export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
run_app(with_mongo = FALSE) # add parameters here (if any)
