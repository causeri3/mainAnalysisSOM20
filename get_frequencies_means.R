#GET FREQUENCIES / MEANS OF SURVEY ANSWERS AGGREGATED FOR SETTLEMENT TYPE (IDP/HC) AND AREA LEVEL AS WIDE FORMAT [UNWEIGHTED, ONLY FOR SAME AEREA LEVEL AS STRATA, IF NON-PROBABILISTIC SAMPLING]
library(reshape2)


#set working directory
setwd("C:/Users/Vanessa Causemann/Desktop/REACH/RStuff/GitHub/mainAnalysisSOM20")

#import data set
df_original <- read.csv(file="input/REACH_SOM2006_JMCNA_IV_Data-Set_August2020_October_27_2020.csv", head=T, dec=".", sep=",", na.strings=c(""," ","NA"))

#############################################################################################################################################################################################
#########MAKE ADAPTIONS SPECIFIC TO YOUR DATA SET############################################################################################################################################
#############################################################################################################################################################################################

#grab "other" variables to exclude in vars-list
other <- grep("_other", names(df_original), value = TRUE)

#List of variables/columns you don't want to have frequencies computed on (i.e. character multiples or variables with only NA's) or ignore and continue after list
vars=c('chronic_illness_hh_members',
       'dis_seeing_demog',
       'dis_hearing_demog',
       'dis_walk_demog',
       'dis_concentr_demog',
       'dis_selfcare_demog',
       'dis_communi_demog',
       'income_src',
       'employ_loss_why',
       'education_type',
       'attend_covid19',
       'remote_edu_via',
       'school_material',
       'school_barrier_note',
       'school_barrier_girls_note',
       'cash_education',
       'home_learning',
       'health_household',
       'health_seek',
       'humanitarian_assistance',
       'unvaccinated_why',
       'barriers_health',
       'cash_health',
       'nutrition_barriers',
       'enough_water',
       'water_barrier',
       'of_water',
       'latrine_features',
       'sanitation_barriers',
       'sanitation_coping',
       'hygeine_coping',
       'menstrual_barriers',
       'hand_washing_times',
       'lack_enclosure',
       'shelter_damage',
       'unable_repair',
       'shelter_issues',
       'hlp_problems',
       'nfi_access',
       'nfi_market',
       'shetler_support',
       'hh_restrictions',
       'restriction_faced',
       'where_not_safe_boys',
       'where_not_safe_girls',
       'main_source_food',
       'phone_type',
       'aid_barriers',
       'prefer_feedback_channel',
       'trust_covidinfo',
       'prefer_covidinfo',
       'action_to_prevent',
       'why_no_action',
       'signs_covid',
       'hh_covid_action',
       'factors_aid',
       'services_affected',
       'X_id',
       'X_uuid',
       'X_submission_time',
       'X_validation_status',
       'X_index',
       'state',
       'weights',
       'hh_size_note',
       'X_uuid.1',
       'left_aoo',
       'arrived_current',
       'today',
       'other',
       'hh_difference',
       'start',
       'end',
       'jmcna',
       'deviceid',
       'consensus',
       'region',
       'children_away_why_note',
       'disp_current_note',
       'displacement_note',
       'district_idp',
       'district_intended',
       'eating_abnormally_length',
       'education_level_note',
       'health_issue_note',
       'note_households_support',
       'note_limitations',
       'note_safety_affected',
       'rooms_difference',
       'shelther_rooms_confirm_note',
       other
       )

#list of all numeric variables (minus vars-list and multiples)
num <- which(unlist(lapply(df_original, is.numeric)))
num<-names(df_original[,num])
multiple <- grep("[.]", names(df_original), value = TRUE)
num<-num[num %in% multiple ==FALSE]
num<-num[num %in% vars ==FALSE]

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
index_num<-which(names(df_original)%in%num)                                                                            #save list of indexes of columns in num
index<-c(index,index_num)                                                                                              #list of indexes which will be removed          
df<-df_original[-index]                                                                                                #make subset without numeric and not wanted variables
df_num<-df_original[index_num]                                                                                         #make data frame only with numerical variables
df_num$settlement_type<-df$settlement_type                                                                             #add settlement type to subset
df_num$area_level<-df$area_level                                                                                       #add area to subset 

#Change "5" to the column number you want to start the loop compute frequencies over all following variables/columns
start<-5


#############################################################################################################################################################################################
#####RUN WITHOUT FURTHER ADAPTIONS###########################################################################################################################################################
#############################################################################################################################################################################################

#####IDP-TABLE#####

u_distr_idp<-unique(df$area_level[df$settlement_type=="IDP"])                                                           #unique list of areas which have idp-settlements  

start_time <- Sys.time()                                                                                                #timer for fun

for (i in start:length(df)){                                                                                            #loop over all columns from "start" onward
  for (k in 1:length(u_distr_idp)){                                                                                     #loop over all unique idp-districts
    table_idp<-as.data.frame(prop.table(table(df[,i][df$area_level==u_distr_idp[k] & df$settlement_type=="IDP"])))      #compute frequencies, save as data frame
    if (dim(table_idp)[1]==0){                                                                                          #solution in case there is no values:
      colnames(table_idp)[1]<-"Var1"                                                                                    #name column as if there would be a value
      table_idp[1,]<-NA                                                                                                 #make value NA
      table_idp$Freq<-NA                                                                                                #create second column as if there would be value and make value NA
    }                                                                    
    table_idp$var_names<- paste0(colnames(df[i]))                                                                       #add column with variable names
    table_idp$settlement_type<-"IDP"                                                                                    #add column with info about settlement-type
    table_idp$area<- u_distr_idp[k]                                                                                     #add column with area
    if (i==start & k==1){long_table_idp<-table_idp}                                                                     #make initial long table from first data frame
    else {long_table_idp<-rbind(long_table_idp,table_idp)}                                                              #add all data frames (from second run onward) row-wise onto first data frame
  }}


end_time <- Sys.time()                                                                                                  #timer
end_time - start_time                                                                                                   #timer
#50 min

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
#1.1h

#####IDP-TABLE [NUMERIC]#####

u_distr_idp<-unique(df$area_level[df$settlement_type=="IDP"])                                                           #unique list of areas which have idp-settlements                  

start_time <- Sys.time()                                                                                                #timer
                                                                                                                         
  for (k in 1:length(u_distr_idp)){                                                                                     #loop over all areas
    df_num_idp<-df_num[df_num$area_level==u_distr_idp[k] & df_num$settlement_type=="IDP",]                              #subset with only survey with area and IDP                          
    means_idp<-data.frame(sapply(df_num_idp[1:(length(df_num_idp)-2)], function(x) mean(x, na.rm=T)))                   #compute means
    table_num_idp <- cbind(variables=rownames(means_idp), data.frame(means_idp, row.names=NULL))                        #save variables and means as data frame
    table_num_idp$settlement_type<-"IDP"                                                                                #add column with info about settlement-type
    table_num_idp$area<- u_distr_idp[k]                                                                                 #add column with area
    if (k==1){long_table_num_idp<-table_num_idp}                                                                        #make initial long table from first data frame
    else {long_table_num_idp<-rbind(long_table_num_idp,table_num_idp)}                                                  #add all data frames (from second run onward) row-wise onto first data frame
    }
end_time <- Sys.time()                                                                                                  #timer
end_time - start_time                                                                                                   #timer


#####NON-IDP-TABLE [NUMERIC]#####

u_distr_host<-unique(df$area_level[df$settlement_type=="HC"])                                                           #unique list of areas which have idp-settlements                  

start_time <- Sys.time()                                                                                                #timer

for (k in 1:length(u_distr_host)){                                                                                      #loop over all areas
  df_num_non_idp<-df_num[df_num$area_level==u_distr_host[k] & df_num$settlement_type=="HC",]                            #subset with only survey with area and IDP                          
  means_hc<-data.frame(sapply(df_num_non_idp[1:(length(df_num_non_idp)-2)], function(x) mean(x, na.rm=T)))              #compute means
  table_num_non_idp <- cbind(variables=rownames(means_hc), data.frame(means_hc, row.names=NULL))                        #save variables and means as data frame
  table_num_non_idp$settlement_type<-"HC"                                                                               #add column with info about settlement-type
  table_num_non_idp$area<- u_distr_host[k]                                                                              #add column with area
  if (k==1){long_table_num_non_idp<-table_num_non_idp}                                                                  #make initial long table from first data frame
  else {long_table_num_non_idp<-rbind(long_table_num_non_idp,table_num_non_idp)}                                        #add all data frames (from second run onward) row-wise onto first data frame
}
end_time <- Sys.time()                                                                                                  #timer
end_time - start_time                                                                                                   #timer


#####JOIN TABLES AND FORMAT WIDE#####
colnames(long_table_num_non_idp)[1:2]<-c("variables", "Freq")                                                                 #fit column naming                  
colnames(long_table_num_idp)[1:2]<-c("variables", "Freq")                                                                     #fit column naming              
long_table<-rbind(long_table_idp,long_table_non_idp)                                                                          #join tables row-wise
long_table$variables <- paste0(long_table$var_names,"_", as.character(long_table$Var1))                                       #add identifying column for making table wide
long<-long_table[,which(names(long_table)%in%c("area","settlement_type","variables","Freq"))]                                 #get rid of not needed columns                                                                
long<-rbind(long,long_table_num_non_idp,long_table_num_idp)                                                                   #add means from numeric tables
names(long)[which(names(long)=="area")]<-"area_level"                                                                         #fix in order to be able to afterwards only multiply the frequencies with 100
wide_table<-dcast(data = long, formula = area_level + settlement_type ~ variables, fun.aggregate = NULL, value.var = "Freq")  #convert to wide format
index2<-which(grepl("_NA",names(wide_table)))                                                                                 #get index of all columns containing "NA" in column name                                                                                                          
wide_table<-wide_table[-index2]                                                                                               #subset without NA columns     

#make frequencies to percentages 
wide_table_perc<-wide_table
wide_table_perc[,-which(names(wide_table_perc)%in%names(df_num))]<-wide_table_perc[,-which(names(wide_table_perc)%in%names(df_num))]*100

#####EXPORT#####
write.csv(wide_table_perc,"output/percentage_means_district_settlement.csv", row.names=FALSE)
write.csv(wide_table,"output/frequency_means_district_settlement.csv", row.names=FALSE)