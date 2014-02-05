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

# generate keys for POT
payerKey <- GenPayerKey(molten, 'Commercial Insurance')
hospKey <- GenHospKey(molten)
overlap_ids <- list(c('149', '013', '054', '064', '025', '088', '204', '049'), c('142'), c('080'))
overlapKey <- GenOverlapKey(molten, overlap_ids, OverlapStandard)

# casting the discharge data
casted <- CastDischarge(molten, payerKey, overlapKey, .(commercial==1))


