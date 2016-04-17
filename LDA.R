mydata = read.csv("train.csv")
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
getwd()