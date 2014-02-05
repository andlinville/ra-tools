library(plyr)
library(reshape2)

rm(list=ls())

#set the working directory
setwd('/Users/andrew/Desktop/Projects/ra-tools')

#source functions
for(i in list.files('R/')){
  path <- paste('R/', i, sep="")
  source(path)
}

#load data
for(i in list.files('data/')){
  path <- paste('data/', i, sep="")
  load(path, .GlobalEnv)
}
rm(i, path)