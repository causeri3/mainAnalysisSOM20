library(hypegrammaR)
library(dplyr)

#set working directory
setwd("C:/Users/Vanessa Causemann/Desktop/REACH/RStuff/GitHub/mainAnalysisSOM20")

#import all necessary files (data set, dap, sampling frame, choices and questions from kobo-tool)
jmcna<-read.csv(file="input/REACH_SOM2006_JMCNA_IV_Data-Set_August2020_October_27_2020.csv", head=T, dec=".", sep=",", stringsAsFactors = F)
dap_state_settlement <- read.csv("input/dap_jmcna_all_var_state_settlement.csv", header = T, stringsAsFactors = F)
sf<-read.csv(file="input/sampling_frame.csv", head=T, dec=".", sep=",", stringsAsFactors = F)         #from population_patching_and_weighting.R
kobochoices <- read.csv("input/choices.csv", header = T, stringsAsFactors = F)
koboquestions <- read.csv("input/questions.csv", header = T, stringsAsFactors = F)

#adjust data set to sampling frame formatting
jmcna$settlement_type[jmcna$idp_settlement=="yes"]<-"IDP"
jmcna$settlement_type[jmcna$idp_settlement=="no"]<-"HC"

sf$stratum_id <- paste(sf$district, sf$settlement_type, sep = "_")
jmcna$stratum_id <- paste(jmcna$district, jmcna$settlement_type, sep = "_")

#re-renaming (because of renaming made for Tableau output)
names(jmcna)[which(names(jmcna) == "weights")]<-"weights_tableau"
names(jmcna)[which(names(jmcna) == "hand_washing_times.cough_sneeze")]<-"hand_washing_times.coughing"
names(jmcna)[which(names(jmcna) == "nutrition_barriers.not_enough_prov")]<-"nutrition_barriers.not"
jmcna$school_transport[jmcna$school_transport=="bus"]<-"business"

#weighting and save weights as column
weight.function <- map_to_weighting(sampling.frame = sf, 
                                    data.stratum.column = "stratum_id",
                                    sampling.frame.population.column = "Population",
                                    sampling.frame.stratum.column = "stratum_id",
                                    data = jmcna)

jmcna$weights_dap <- weight.function(jmcna)

#check difference in weights
#sum(jmcna$weights_tableau-jmcna$weights_dap)

#load questionnaire
questionnaire<-load_questionnaire(data = jmcna,
                                  questions = koboquestions,
                                  choices = kobochoices,
                                  choices.label.column.to.use = "label::English")

#test if all variable names fit
#dap_state_settlement$dependent.variable %in% names(jmcna)

#adapt naming to hypegrammaR-style
dap_state_settlement$dependent.variable<-hypegrammaR:::to_alphanumeric_lowercase(dap_state_settlement$dependent.variable)

##############SKIP FUNCTIONS FOR DIRECT REPORTING#################################################################################################################################################

#################
### FUNCTIONS ###
#################

# Getting and export table with p-value

table_wrangling <- function(x) {
  stats <- x[["summary.statistic"]]
  pval_table <- as.data.frame(x[["hypothesis.test"]])
  num_col = ncol(pval_table)
  if (num_col != 5) {
    if (num_col == 0) {
      pval_table <- data.frame(F = NA, p.value = NA, ndf = NA, ddf = NA, name = NA)
    } else {
      pval_table <- full_join(data.frame(F = numeric(), p.value = numeric(), ndf = numeric(), ddf = numeric(), name = character()), pval_table)
    }
  } else {
    names(pval_table) <- c("F", "p.value", "ndf", "ddf", "name")
  }
  cbind(stats, pval_table)
}

pval_table <- function(results, filename) {
  lapply(results, table_wrangling) %>%
    do.call(rbind, .) %>%
    write.csv(filename, row.names = F)
}

##########GET RESULTS AND SAVE IN DIFFERENT FORMATS########################################################################################################################

#######################################
##########REPEAT FOR NATIONAL##########
#######################################

#create DAP
dap_national<-dap_state_settlement
dap_national$repeat.for.variable<-NA
dap_national$independent.variable<-NA
dap_national$independent.variable.type<-NA

#results-function (can take very long)
start_time <- Sys.time()                                                                                                                    #timer for fun
list_of_results_national <-  from_analysisplan_map_to_output(jmcna,
                                                             analysisplan = dap_national,
                                                             weighting = weight.function,
                                                             cluster_variable_name = NULL,
                                                             questionnaire = questionnaire, confidence_level = 0.90)
end_time <- Sys.time()                                                                                                                      #timer
end_time - start_time                                                                                                                       #timer

#save results as R file and get it 
list_of_results_national %>% saveRDS("output/list_of_results_national.RDS")
#list_of_results_national <- readRDS("output/list_of_results_national.RDS")

#get long table of results 
long_table_national <- list_of_results_national$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)

#re-format wide and as % for data merge in InDesign (FactSheets)
table_national<-long_table_national[c("dependent.var","dependent.var.value" ,"repeat.var.value", "numbers")]
wide_table_national<-dcast(data = table_national, formula = repeat.var.value ~ dependent.var + dependent.var.value , fun.aggregate = NULL, value.var = "numbers")

#export results as CSV files
write.csv(wide_table_perc_national, file= paste0("output/wide_table_perc_national.csv"), row.names=FALSE)
write.csv(long_table_national, file= paste0("output/long_table_national.csv"), row.names=FALSE)

#get table with p-values and F-statistic (if not only direct reporting)
#pval_table(list_of_results_national$results, "output/list_of_results_hypo_national.csv")

#get html output
hypegrammaR:::map_to_generic_hierarchical_html(list_of_results_national,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir ="output",
                                               filename = "list_of_results_national.html")

#######################################
###REPEAT FOR NATIONAL [IDP/NON-IDP]###
#######################################

#create DAP
dap_national_settlement<-dap_state_settlement
dap_national_settlement$repeat.for.variable<-NA

#results-function (can take very long)
start_time <- Sys.time()                                                                                                                    #timer for fun
list_of_results_national_settlement <-  from_analysisplan_map_to_output(jmcna,
                                                                        analysisplan = dap_national_settlement,
                                                                        weighting = weight.function,
                                                                        cluster_variable_name = NULL,
                                                                        questionnaire = questionnaire, confidence_level = 0.90)
end_time <- Sys.time()                                                                                                                      #timer
end_time - start_time                                                                                                                       #timer

#save results as R file and get it 
list_of_results_national_settlement %>% saveRDS("output/list_of_results_national_settlement.RDS")
#list_of_results_national_settlement <- readRDS("output/list_of_results_national_settlement.RDS")

#get long table of results 
long_table_national_settlement <- list_of_results_national_settlement$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)

#re-format wide and as % for data merge in InDesign (FactSheets)
table_national_settlement<-long_table_national_settlement[c("dependent.var","dependent.var.value" , "independent.var.value" ,"repeat.var.value", "numbers")]
wide_table_national_settlement<-dcast(data = table_national_settlement, formula = repeat.var.value + independent.var.value ~ dependent.var + dependent.var.value , fun.aggregate = NULL, value.var = "numbers")

#export results as CSV files
write.csv(wide_table_national_settlement, file= paste0("output/wide_table_national_settlement.csv"), row.names=FALSE)
write.csv(long_table_national_settlement, file= paste0("output/long_table_national_settlement.csv"), row.names=FALSE)

#get table with p-values and F-statistic (if not only direct reporting)
#pval_table(list_of_results_national_settlement$results, "output/list_of_results_hypo_national_settlement.csv")

#get html output
hypegrammaR:::map_to_generic_hierarchical_html(list_of_results_national_settlement,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir ="output",
                                               filename = "list_of_results_national_settlement.html")


#####################################
##########REPEAT FOR STATES##########
#####################################

#create DAP
dap_state<-dap_state_settlement
dap_state$independent.variable<-NA
dap_state$independent.variable.type<-NA

#results-function (can take very long)
start_time <- Sys.time()                                                                                                                    #timer for fun
list_of_results_state <-  from_analysisplan_map_to_output(jmcna,
                                                          analysisplan = dap_state,
                                                          weighting = weight.function,
                                                          cluster_variable_name = NULL,
                                                          questionnaire = questionnaire, confidence_level = 0.90)
end_time <- Sys.time()                                                                                                                      #timer
end_time - start_time                                                                                                                       #timer

#save results as R file and get it 
list_of_results_state %>% saveRDS("output/list_of_results_state.RDS")
#list_of_results_state <- readRDS("output/list_of_results_state.RDS")

#get long table of results 
long_table_state <- list_of_results_state$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)

#re-format wide and as % for data merge in InDesign (FactSheets)
table_state<-long_table_state[c("dependent.var","dependent.var.value" ,"repeat.var.value", "numbers")]
wide_table_state<-dcast(data = table_state, formula = repeat.var.value ~ dependent.var + dependent.var.value , fun.aggregate = NULL, value.var = "numbers")

#export results as CSV files
write.csv(wide_table_state, file= paste0("output/wide_table_state.csv"), row.names=FALSE)
write.csv(long_table_state, file= paste0("output/long_table_state.csv"), row.names=FALSE)

#get table with p-values and F-statistic (if not only direct reporting)
#pval_table(list_of_results_state$results, "output/list_of_results_hypo_state.csv")


#get html output
hypegrammaR:::map_to_generic_hierarchical_html(list_of_results_state,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir ="output",
                                               filename = "list_of_results_state.html")



#####################################
###REPEAT FOR STATES [IDP/NON-IDP]###
#####################################

#results-function (can take very long)
start_time <- Sys.time()                                                                                                                    #timer for fun
list_of_results_state_settlement <-  from_analysisplan_map_to_output(jmcna,
                                                                     analysisplan = dap_state_settlement,
                                                                     weighting = weight.function,
                                                                     cluster_variable_name = NULL,
                                                                     questionnaire = questionnaire, confidence_level = 0.90)
end_time <- Sys.time()                                                                                                                      #timer
end_time - start_time                                                                                                                       #timer

#save results as R file and get it 
list_of_results_state_settlement %>% saveRDS("output/list_of_results_state_settlement.RDS")
#list_of_results_state_settlement <- readRDS("output/list_of_results_state_settlement.RDS")

#get long table of results 
long_table_state_settlement <- list_of_results_state_settlement$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)

#re-format wide and as % for data merge in InDesign (FactSheets)
table_state_settlement<-long_table_state_settlement[c("dependent.var","dependent.var.value" , "independent.var.value" ,"repeat.var.value", "numbers")]
wide_table_state_settlement<-dcast(data = table_state_settlement, formula = repeat.var.value + independent.var.value ~ dependent.var + dependent.var.value , fun.aggregate = NULL, value.var = "numbers")

#export results as CSV files
write.csv(wide_table_state_settlement, file= paste0("output/wide_table_state_settlement.csv"), row.names=FALSE)
write.csv(long_table_state_settlement, file= paste0("output/long_table_state_settlement.csv"), row.names=FALSE)

#get table with p-values and F-statistic (if not only direct reporting)
#pval_table(list_of_results_state_settlement$results, "output/list_of_results_hypo_state_settlement.csv")


#get html output
hypegrammaR:::map_to_generic_hierarchical_html(list_of_results_state_settlement,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir ="output",
                                               filename = "list_of_results_state_settlement.html")


######################################
##########REPEAT FOR REGIONS##########
######################################

#create DAP
dap_region<-dap_state_settlement
dap_region$repeat.for.variable<-"region"
dap_region$independent.variable<-NA
dap_region$independent.variable.type<-NA

#results-function (can take very long)
start_time <- Sys.time()                                                                                                                    #timer for fun
list_of_results_region <-  from_analysisplan_map_to_output(jmcna,
                                                           analysisplan = dap_region,
                                                           weighting = weight.function,
                                                           cluster_variable_name = NULL,
                                                           questionnaire = questionnaire, confidence_level = 0.90)
end_time <- Sys.time()                                                                                                                      #timer
end_time - start_time                                                                                                                       #timer

#save results as R file and get it 
list_of_results_region %>% saveRDS("output/list_of_results_region.RDS")
#list_of_results_region <- readRDS("output/list_of_results_region.RDS")

#get long table of results 
long_table_region <- list_of_results_region$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)

#re-format wide and as % for data merge in InDesign (FactSheets)
table_region<-long_table_region[c("dependent.var","dependent.var.value" ,"repeat.var.value", "numbers")]
wide_table_region<-dcast(data = table_region, formula = repeat.var.value ~ dependent.var + dependent.var.value , fun.aggregate = NULL, value.var = "numbers")

#export results as CSV files
write.csv(wide_table_region, file= paste0("output/wide_table_region.csv"), row.names=FALSE)
write.csv(long_table_region, file= paste0("output/long_table_region.csv"), row.names=FALSE)

#get table with p-values and F-statistic (if not only direct reporting)
#pval_table(list_of_results_region$results, "output/list_of_results_hypo_region.csv")

#get html output
hypegrammaR:::map_to_generic_hierarchical_html(list_of_results_region,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir ="output",
                                               filename = "list_of_results_region.html")


######################################
###REPEAT FOR REGIONS[IDP/NON-IDP]####
######################################

#create DAP
dap_region_settlement<-dap_state_settlement
dap_region_settlement$repeat.for.variable<-"region_settlement"

#results-function (can take very long)
start_time <- Sys.time()                                                                                                                    #timer for fun
list_of_results_region_settlement <-  from_analysisplan_map_to_output(jmcna,
                                                                      analysisplan = dap_region_settlement,
                                                                      weighting = weight.function,
                                                                      cluster_variable_name = NULL,
                                                                      questionnaire = questionnaire, confidence_level = 0.90)
end_time <- Sys.time()                                                                                                                      #timer
end_time - start_time                                                                                                                       #timer

#save results as R file and get it 
list_of_results_region_settlement %>% saveRDS("output/list_of_results_region_settlement.RDS")
#list_of_results_region_settlement <- readRDS("output/list_of_results_region_settlement.RDS")

#get long table of results 
long_table_region_settlement <- list_of_results_region_settlement$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)

#re-format wide and as % for data merge in InDesign (FactSheets)
table_region_settlement<-long_table_region_settlement[c("dependent.var","dependent.var.value" , "independent.var.value" ,"repeat.var.value", "numbers")]
wide_table_region_settlement<-dcast(data = table_region_settlement, formula = repeat.var.value + independent.var.value ~ dependent.var + dependent.var.value , fun.aggregate = NULL, value.var = "numbers")

#export results as CSV files
write.csv(wide_table_region_settlement, file= paste0("output/wide_table_region_settlement.csv"), row.names=FALSE)
write.csv(long_table_region_settlement, file= paste0("output/long_table_region_settlement.csv"), row.names=FALSE)

#get table with p-values and F-statistic (if not only direct reporting)
#pval_table(list_of_results_region_settlement$results, "output/list_of_results_hypo_region_settlement.csv")

#get html output
hypegrammaR:::map_to_generic_hierarchical_html(list_of_results_region_settlement,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir ="output",
                                               filename = "list_of_results_region_settlement.html")

######################################
#########REPEAT FOR DISTRICTS#########
######################################

#create DAP
dap_district<-dap_state_settlement
dap_district$repeat.for.variable<-"district"
dap_district$independent.variable<-NA
dap_district$independent.variable.type<-NA

#results-function (can take very long)
start_time <- Sys.time()                                                                                                                    #timer for fun
list_of_results_district <-  from_analysisplan_map_to_output(jmcna,
                                                             analysisplan = dap_district,
                                                             weighting = weight.function,
                                                             cluster_variable_name = NULL,
                                                             questionnaire = questionnaire, confidence_level = 0.90)
end_time <- Sys.time()                                                                                                                      #timer
end_time - start_time                                                                                                                       #timer

#save results as R file and get it 
list_of_results_district %>% saveRDS("output/list_of_results_district.RDS")
#list_of_results_district <- readRDS("output/list_of_results_district.RDS")

#get long table of results 
long_table_district <- list_of_results_district$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)

#re-format wide and as % for data merge in InDesign (FactSheets)
table_district<-long_table_district[c("dependent.var","dependent.var.value" ,"repeat.var.value", "numbers")]
wide_table_district<-dcast(data = table_district, formula = repeat.var.value ~ dependent.var + dependent.var.value , fun.aggregate = NULL, value.var = "numbers")

#export results as CSV files
write.csv(wide_table_district, file= paste0("output/wide_table_district.csv"), row.names=FALSE)
write.csv(long_table_district, file= paste0("output/long_table_district.csv"), row.names=FALSE)

#get table with p-values and F-statistic (if not only direct reporting)
#pval_table(list_of_results_district$results, "output/list_of_results_hypo_district.csv")

#get html output
hypegrammaR:::map_to_generic_hierarchical_html(list_of_results_district,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir ="output",
                                               filename = "list_of_results_district.html")
