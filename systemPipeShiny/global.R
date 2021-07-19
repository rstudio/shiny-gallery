## use shiny::runApp() in console or click right top button
## '>Run App' in Rstudio to start app,
## but do not write `shiny::runApp()` in script, type it in console

time_start <- Sys.time()
library(systemPipeShiny)
library(magrittr) # load pipes
# load additional libraries that you want to use below
## Workflow module
requireNamespace("DOT"); requireNamespace("networkD3"); requireNamespace("pushbar")
requireNamespace("readr"); requireNamespace("rhandsontable"); requireNamespace("shinyTree")
requireNamespace("systemPipeR"); requireNamespace("systemPipeRdata"); requireNamespace("zip")

## RNA-Seq module
requireNamespace("DESeq2"); requireNamespace("Rtsne"); requireNamespace("SummarizedExperiment")
requireNamespace("UpSetR"); requireNamespace("ape"); requireNamespace("ggtree")
requireNamespace("glmpca"); requireNamespace("pheatmap"); requireNamespace("systemPipeR")
requireNamespace("callr")

## Quick ggplot module
requireNamespace("esquisse")


## SPS options
# read "https://systempipe.org/sps/adv_features/config/#app-options" for details
# title: dashboard and website title - any string
# title_logo: logo to display when dashboard is collapsed and on website tab - url of an image
# mode: running mode - "local", "server"
# warning_toast: some warning messages to check potential risks - TRUE, FALSE
# login_screen: to show login screen? - TRUE, FALSE
# login_theme: login screen themes, login_screen need be TRUE - "random"
# use_crayon: Do you want colorful terminal messages? TRUE, FALSE
# verbose: display some info during processing? - TRUE, FALSE
# admin_url: admin_page query url - "admin"
# note_url: User notification broadcast file url - http(s) address
# tab_welcome -- module_wf: whether to load the corresponding tab or module? - TRUE, FALSE
# traceback: for expressions wrapped inside `spsComps::shinyCatch`, show full traceback if error? TRUE, FALSE
# is_demo: useful if deploy the app as a demo - TRUE, FALSE
# welcome_guide: enable the welcome guide which show you where is guide dropdown menu? - TRUE, FALSE
options(sps = list(
    title = "systemPipeShiny",
    title_logo = "img/sps_small.png",
    mode = "server",
    warning_toast = FALSE,
    login_screen = FALSE,
    login_theme = "random",
    use_crayon = TRUE,
    verbose = FALSE,
    admin_page = TRUE,
    admin_url = "admin",
    note_url = 'https://raw.githubusercontent.com/systemPipeR/systemPipeShiny/master/inst/remote_resource/notifications_internal.yaml',
    tab_welcome = TRUE,
    tab_vs_main = TRUE,
    tab_canvas = TRUE,
    tab_about = TRUE,
    module_wf = TRUE,
    module_rnaseq = TRUE,
    module_ggplot = TRUE,
    traceback = FALSE,
    is_demo = TRUE,
    welcome_guide = TRUE
))

## An alternative is to comment above and use `spsOption` to overwrite single options, eg:
# spsOption("mode", "server")

## use `spsOptions` to check current settings
# spsOptions()


## other useful shiny options
## max upload size, 30Mb here
options(shiny.maxRequestSize = 30*1e6)

## for debugging
# options(shiny.reactlog = TRUE)
# options(shiny.trace = TRUE)
# options(shiny.fullstacktrace = TRUE)
# options(shiny.error = browser)
# options(shiny.autoreload = FALSE) # takes some computer power, you may consider turn it off

##  account information
## PLEASE use following to add your own accounts and remove the default accounts for deployment
# mydb <- spsAccount$new()
# mydb$accList()
# mydb$accAdd(acc_name = "XXX", acc_pass = "$xxxx", role = "admin")
# mydb$accRemove("admin")
# mydb$accRemove("user")

####### SPS Main App Function Starts #########

sps_app <- sps(
    tabs = c("vs_example"),
    server_expr = {
        # add you own server functions below
        msg("Custom expression runs -- Hello World", "GREETING", "green")
    }
)
