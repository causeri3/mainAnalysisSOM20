#_________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
#                                                         FUNCTIONS TO ADAPT HYPEGRAMMAR OUTPUT TO NEEDS OF THE TEAM
#_________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
#
#Make long table to wide table; input from hypegrammaR-package as e.g.: 
#list_of_results <-  from_analysisplan_map_to_output(df,
#                                                    analysisplan = dap,
#                                                    weighting = weight.function,
#                                                    cluster_variable_name = NULL,
#                                                    questionnaire = questionnaire, confidence_level = 0.90)
#long_results<-list_of_results$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
#e.g.: wide_results<-wide_table(long_results)

wide_table<-function(x){ 
  x[x=="NA"]<-NA
  if (length(which(is.na(x["numbers"])))!=0){
      x<-x[-which(is.na(x["numbers"])),]
    }
  if (length(which(is.na(x["independent.var"])))<length(x[,"independent.var"]) & length(which(is.na(x["repeat.var.value"])))==0){                                           #for area and settlement levels
    table<-x[c("dependent.var","dependent.var.value" , "independent.var.value" ,"repeat.var.value", "numbers")]                                                             #take only relevant variables
    if (length(which(is.na(table["independent.var.value"])))==0){                                                                                                           #if no NA results
       wide<-dcast(data = table, formula = repeat.var.value + independent.var.value ~ dependent.var + dependent.var.value , fun.aggregate = NULL, value.var = "numbers")    #wide format
    }
    else{                                                                                                                                                                   #if NA-results:
      table<-table[-which(is.na(x["independent.var.value"])),]                                                                                                              #get rid of NA-rows in case 
      wide<-dcast(data = table, formula = repeat.var.value + independent.var.value ~ dependent.var + dependent.var.value , fun.aggregate = NULL, value.var = "numbers")     #wide format
    }
  }
  else{
    if (length(is.na(x["independent.var"]))==length(x["independent.var"]) & length(is.na(x["repeat.var.value"]))==length(x["repeat.var.value"])){                            #for national level
       table<-x[c("dependent.var","dependent.var.value", "numbers", "repeat.var.value")]                                                                                     #take only relevant variables
       wide<-dcast(data = table, formula = repeat.var.value ~ dependent.var + dependent.var.value , fun.aggregate = NULL, value.var = "numbers")                             #wide format
       wide<-wide[2:length(wide)]                                                                                                                                            #delete unnecessary column 
    }
    else{
      if (length(which(is.na(x["independent.var"])))==dim(x)[1]){                                                                                                            #for only area levels
        table<-x[c("dependent.var","dependent.var.value" , "repeat.var.value", "numbers")]                                                                                   #take only relevant variables
        wide<-dcast(data = table, formula = repeat.var.value ~ dependent.var + dependent.var.value , fun.aggregate = NULL, value.var = "numbers")                            #wide format
      }
      else{                                                                                                                                                                  #for national and settlement level
        table<-x[c("dependent.var","dependent.var.value" , "independent.var.value", "numbers")]                                                                              #take only relevant variables
         if (length(which(is.na(table["independent.var.value"])))==0){                                                                                                       #if no NA results
           wide<-dcast(data = table, formula = independent.var.value ~ dependent.var + dependent.var.value , fun.aggregate = NULL, value.var = "numbers")                    #wide format
         }
         else{                                                                                                                                                               #if NA-results:
           table<-table[-which(is.na(table["independent.var.value"])),]                                                                                                      #get rid of NA-rows
           wide<-dcast(data = table, formula = independent.var.value ~ dependent.var + dependent.var.value , fun.aggregate = NULL, value.var = "numbers")                    #wide format
         }
      }
    }
  }
  return(wide)
}
#_________________________________________________________________________________________________________________________________________________________________________________________________________________________________________

#Make hypegrammaR frequencies to % in wide table (excluding means)
#e.g. wide_perc(wide_results)
#or in one move: 
#e.g. wide_perc(wide_table(long_results))

wide_perc<-function(x){
  index<-c(which(grepl("_NA",names(x))), which(names(x)=="repeat.var.value"), which(names(x)=="independent.var.value"))                                                       #get indices for means and (area/settlement) level variables
  if (length(index)==0){                                                                                                                                                      #if no indexes:
    wide_perc<-x*100                                                                                                                                                          #multiply whole data frame with 100
  }
  else{                                                                                                                                                                       #if index not 0:
    wide_perc<-x                                                                                                                                                              #make new data frame
    wide_perc[-index]<-wide_perc[-index]*100                                                                                                                                  #multiply only frequency-columns by 100 
  }
  return(wide_perc)
}

