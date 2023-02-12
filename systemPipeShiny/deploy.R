#########################################
## If you use some bioconductor package,
## uncomment and run the next lin in console
## before deploy to shinyapps.io
########################################
# options(repos = BiocManager::repositories())

## if you use develop version of bioconductor packages, do following:
## E.g. current release is 3.12 and you want use 3.13 devel
# repos <- BiocManager::repositories()
# devel <- "3.13"
# repos[length(repos) + 1] <- paste0("https://bioconductor.org/packages/", devel, "/bioc")
# names(repos)[length(repos)] <- "BioC"
# options(repos = repos)
# getOption("repos")

###########################################################################
## If you are using some SPS modules, pick what modules you prefer,
## uncomment and copy following by your needs to "global.R" file in deployment:
##########################################################################
## Workflow module
# requireNamespace("DOT"); requireNamespace("networkD3"); requireNamespace("pushbar")
# requireNamespace("readr"); requireNamespace("rhandsontable"); requireNamespace("shinyTree")
# requireNamespace("systemPipeR"); requireNamespace("systemPipeRdata"); requireNamespace("zip")
# requireNamespace("callr")

## RNA-Seq module
# requireNamespace("DESeq2"); requireNamespace("Rtsne"); requireNamespace("SummarizedExperiment")
# requireNamespace("UpSetR"); requireNamespace("ape"); requireNamespace("ggtree")
# requireNamespace("glmpca"); requireNamespace("pheatmap"); requireNamespace("systemPipeR")

## Quick ggplot module
# requireNamespace("esquisse")


########################
## Set up your account:
########################

## Replace the following arguments:
## you can find them on shinyapps.io when you create an account
## - username: Name of account to save or remove
## - token: User token for the account
## - secret: User secret for the account

# setAccountInfo(name="username", token="token", secret="secret")

##########
## Deploy:
##########

# getOption("repos")
# rsconnect::deployApp(
#     appName = "systemPipeShiny",
#     account = "username",
#     lint = TRUE
#     )
