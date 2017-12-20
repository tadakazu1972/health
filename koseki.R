setwd("~/Desktop/koseki")
lf <- list.files(path="~/Desktop/koseki", full.names=T)
data <- lapply(lf, read.csv)
data_bind <- do.call(rbind, data)