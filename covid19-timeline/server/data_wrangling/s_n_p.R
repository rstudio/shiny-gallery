# try to download the data
# if not use the backup
a <- try(getSymbols("^GSPC"), silent = TRUE)
if (a != "^GSPC") {
  gspc <- read_csv("data/gspc_backup.csv")
} else {
  gspc <- as.data.frame(GSPC)
  gspc$date <- as.Date(rownames(gspc))
}