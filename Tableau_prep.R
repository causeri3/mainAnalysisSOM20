library(tidyr)

setwd("C:/Users/Vanessa Causemann/Desktop/REACH/Data")

indicators<-read.csv(file="JMCNA_SOM20_Indicators_07_01_21.csv", head=T, dec=".", sep=",",na.strings=c("NA",""," "))
labels<-read.csv(file="labelsJMCNA2020/DAP_labels.csv", head=T, dec=".", sep=",")
#ind<- names(indicators[7:110])

#make long table fdrom indicators and merge indicators with labels
data_long <- gather(indicators,Index,scores,SD:MSNI,  factor_key=FALSE)
data_finished<-merge(data_long, labels, by = "Index", all.x = TRUE)


today <- Sys.Date()
today<-format(today, format="_%Y_%b_%d")
write.csv(data_finished,paste0("Tableau_Indicators",today,".csv"), row.names=FALSE)