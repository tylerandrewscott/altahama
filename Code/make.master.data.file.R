rm(list=ls())

file.list <- list.files('../Input/neep')

library(plyr)
library(dplyr)
library(data.table)

file.list <- file.list[intersect(grep('.doc',file.list,invert = TRUE),
                    grep('.txt',file.list,invert = TRUE))]

uq.wq.list <- unique(grep('.wq$', unique(gsub(' ','',gsub("[[:punct:]]",'',gsub('.csv','',gsub('[[:digit:]]','',file.list))))),value=T))

matches <- unique (grep(paste(uq.wq.list,collapse="|"), 
                        file.list, value=TRUE))

write.csv(rbindlist(lapply(paste0('../Input/neep/',matches),fread,na.strings=c("NA","","?"),sep=',',header=T),fill=TRUE),file = '../Input/master.wq.data.csv')






