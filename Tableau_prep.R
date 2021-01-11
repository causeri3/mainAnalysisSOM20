library(tidyr)

###IMPORT###

setwd("C:/Users/Vanessa Causemann/Desktop/REACH/RStuff/GitHub/mainAnalysisSOM20")

#data set with uuid's, settlement type, all area levels, weights and all indicators
indicators<-read.csv(file="input/JMCNA_SOM20_Indicators_07_01_21.csv", head=T, dec=".", sep=",",na.strings=c("NA",""," "))

#reduced data analysis plan [dap] in order to use it as labels and for sector filter
labels<-read.csv(file="input/DAP_labels.csv", head=T, dec=".", sep=",")


###LONG FORMAT & MERGE###

#make long table from indicators and merge indicators with labels
data_long <- gather(indicators,Index,scores,SD:MSNI,  factor_key=FALSE)
data_finished<-merge(data_long, labels, by = "Index", all.x = TRUE)


###EXPORT###

#date for export
today <- Sys.Date()
today<-format(today, format="_%Y_%b_%d")

#export
write.csv(data_finished,paste0("output/Tableau_Indicators",today,".csv"), row.names=FALSE)