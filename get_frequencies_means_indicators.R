#GET FREQUENCIES / MEANS OF SURVEY ANSWERS AGGREGATED FOR SETTLEMENT TYPE (IDP/HC) AND AREA LEVEL AS WIDE FORMAT [UNWEIGHTED, ONLY FOR SAME AEREA LEVEL AS STRATA, IF NON-PROBABILISTIC SAMPLING]
library(reshape2)


#set working directory
setwd("C:/Users/Vanessa Causemann/Desktop/REACH/RStuff/GitHub/mainAnalysisSOM20")

#import data set
df_original <- read.csv(file="output/REACH_SOM2006_JMCNA_IV_Data-Set_with_indicators_scored_2021_Jan_04.csv", head=T, dec=".", sep=",", na.strings=c(""," ","NA"))

#############################################################################################################################################################################################
#########MAKE ADAPTIONS SPECIFIC TO YOUR DATA SET############################################################################################################################################
#############################################################################################################################################################################################

#List of variables/columns you don't want to have frequencies computed on (i.e. character multiples or variables with only NA's) or ignore and continue after list
vars=c(names(df_original)[1:9],names(df_original)[12:991])

#Put the name of the column with the area level you want to compute frequencies on instead of "district"
names(df_original)[which(names(df_original) == "district")]<-"area_level"

#Put the name of the column of your settlement-type instead of "idp_settlement"
names(df_original)[which(names(df_original) == "idp_settlement")]<-"settlement_type"

#Recode your settlement type values
#to check your values unhash next line and run
#unique(df$settlement_type)                      
#replace "yes" and "no" with your values
df_original$settlement_type[df_original$settlement_type=="yes"]<-"IDP"
df_original$settlement_type[df_original$settlement_type=="no"]<-"HC"

index<-which(names(df_original)%in%vars)                                                                               #save list of indexes of columns in vars
df<-df_original[-index]                                                                                                #make subset without numeric and not wanted variables


#Change "3" to the column number you want to start the loop compute frequencies over all following variables/columns
start<-3


#############################################################################################################################################################################################
#####RUN WITHOUT FURTHER ADAPTIONS###########################################################################################################################################################
#############################################################################################################################################################################################

#####IDP-TABLE#####

u_distr_idp<-unique(df$area_level[df$settlement_type=="IDP"])                                                          #unique list of areas which have idp-settlements  

start_time <- Sys.time()                                                                                               #timer for fun

for (i in start:length(df)){                                                                                           #loop over all columns from "start" onward
  for (k in 1:length(u_distr_idp)){                                                                                    #loop over all unique idp-districts
    table_idp<-as.data.frame(prop.table(table(df[,i][df$area_level==u_distr_idp[k] & df$settlement_type=="IDP"])))     #compute frequencies, save as data frame
    if (dim(table_idp)[1]==0){                                                                                         #solution in case there is no values:
      colnames(table_idp)[1]<-"Var1"                                                                                   #name column as if there would be a value
      table_idp[1,]<-NA                                                                                                #make value NA
      table_idp$Freq<-NA                                                                                               #create second column as if there would be value and make value NA
    }                                                                    
    table_idp$var_names<- paste0(colnames(df[i]))                                                                      #add column with variable names
    table_idp$settlement_type<-"IDP"                                                                                   #add column with info about settlement-type
    table_idp$area<- u_distr_idp[k]                                                                                    #add column with area
    if (i==start & k==1){long_table_idp<-table_idp}                                                                    #make initial long table from first data frame
    else {long_table_idp<-rbind(long_table_idp,table_idp)}                                                             #add all data frames (from second run onward) row-wise onto first data frame
  }}


end_time <- Sys.time()                                                                                                 #timer
end_time - start_time                                                                                                  #timer


#####NON-IDP-TABLE#####

u_distr_host<-unique(df$area_level[df$settlement_type=="HC"])                                                           #unique list of areas which have non-idp-settlements                  

start_time <- Sys.time()                                                                                                #timer

for (i in start:length(df)){                                                                                            #loop over all columns from "start" onward
  for (k in 1:length(u_distr_host)){                                                                                    #loop over all unique non-idp-districts
    table_non_idp<-as.data.frame(prop.table(table(df[,i][df$area_level==u_distr_host[k] & df$settlement_type=="HC"])))  #compute frequencies, save as data frame
    if (dim(table_non_idp)[1]==0){                                                                                      #solution in case there is no values:
      colnames(table_non_idp)[1]<-"Var1"                                                                                #name column as if there would be a value
      table_non_idp[1,]<-NA                                                                                             #make value NA
      table_non_idp$Freq<-NA                                                                                            #create second column as if there would be value and make value NA
    }                                                                    
    table_non_idp$var_names<- paste0(colnames(df[i]))                                                                   #add column with variable names
    table_non_idp$settlement_type<-"HC"                                                                                 #add column with info about settlement-type
    table_non_idp$area<- u_distr_host[k]                                                                                #add column with area
    if (i==start & k==1){long_table_non_idp<-table_non_idp}                                                             #make initial long table from first data frame
    else {long_table_non_idp<-rbind(long_table_non_idp,table_non_idp)}                                                  #add all data frames (from second run onward) row-wise onto first data frame
  }}

end_time <- Sys.time()                                                                                                  #timer
end_time - start_time                                                                                                   #timer


#####JOIN TABLES AND FORMAT WIDE#####
long_table<-rbind(long_table_idp,long_table_non_idp)                                                                    #join tables row-wise
long_table$variables <- paste0(long_table$var_names,"-", as.character(long_table$Var1))                                 #add identifying column for making table wide
long<-long_table[,which(names(long_table)%in%c("area","settlement_type","variables","Freq"))]                           #get rid of not needed columns
wide_table<-dcast(data = long, formula = area + settlement_type ~ variables, fun.aggregate = NULL, value.var = "Freq")  #convert to wide format
index2<-which(grepl("-NA",names(wide_table)))                                                                           #get index of all columns containing "NA" in column name                                                                                                  #add the "_other" columns to the list of indexes which will be deleted          
wide_table<-wide_table[-index2]                                                                                         #subset without NA columns     


#####EXPORT#####

write.csv(wide_table,"output/frequency_means_table_indicatorscsv", row.names=FALSE)