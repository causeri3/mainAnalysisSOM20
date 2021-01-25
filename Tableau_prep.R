library(tidyr)

###IMPORT###

setwd("C:/Users/Vanessa Causemann/Desktop/REACH/RStuff/GitHub/mainAnalysisSOM20")

#whole data set
jmcna<-read.csv(file="output/REACH_SOM2006_JMCNA_IV_Data-Set_with_indicators_scored_2021_Jan_25.csv", head=T, dec=".", sep=",",na.strings=c("NA",""," "))

#reduced data analysis plan [dap] in order to use it as labels and for sector filter, added on missing labels for freshly scored variables
labels<-read.csv(file="input/DAP_labels.csv", head=T, dec=".", sep=",")

#data set with uuid's, settlement type, all area levels, weights and all indicators
vars<-names(jmcna)[993:dim(jmcna)[2]]
vars<-c(c("X_uuid","idp_settlement", "district","region", "state", "weights"),vars)
indicators<-jmcna[vars]


###LONG FORMAT & MERGE###

#make long table from indicators and merge indicators with labels
data_long <- gather(indicators,Index,scores,names(indicators)[7]:names(indicators)[dim(indicators)[2]],  factor_key=FALSE)
data_finished<-merge(data_long, labels, by = "Index", all.x = TRUE)


###EXPORT###

#date for export
today <- Sys.Date()
today<-format(today, format="_%Y_%b_%d")

#export
write.csv(data_finished, paste0("output/Tableau_Indicators",today,".csv"), row.names=FALSE)