library(hypegrammaR)
library(dplyr)

#set working directory
setwd("C:/Users/Vanessa Causemann/Desktop/REACH/RStuff/GitHub/mainAnalysisSOM20")

#import all necessary files (data set, dap, sampling frame, choices and questions from kobo-tool)
df<-read.csv(file="output/REACH_SOM2006_JMCNA_IV_Data-Set_with_indicators_scored_2021_Jan_04.csv", head=T, dec=".", sep=",", stringsAsFactors = F)
sf<-read.csv(file="input/sampling_frame.csv", head=T, dec=".", sep=",", stringsAsFactors = F)         #from population_patching_and_weighting.R
kobochoices <- read.csv("input/choices.csv", header = T, stringsAsFactors = F)
koboquestions <- read.csv("input/questions.csv", header = T, stringsAsFactors = F)

#create dap:
dependent.variable<-names(df)[992:dim(df)[2]]
research.question<-rep(NA, length(dependent.variable))
sub.research.question<-rep(NA, length(dependent.variable))
repeat.for.variable<-rep(NA, length(dependent.variable))
#independent.variable<-rep("settlement_type", length(dependent.variable))
independent.variable<-rep(NA, length(dependent.variable))
#independent.variable.type<-rep("categorical", length(dependent.variable))
independent.variable.type<-rep(NA, length(dependent.variable))
dependent.variable.type<-rep("categorical", length(dependent.variable))
hypothesis.type<-rep("direct_reporting", length(dependent.variable))

dap_indicators<-data.frame(research.question,
                           sub.research.question,
                           repeat.for.variable,
                           independent.variable,
                           independent.variable.type,
                           dependent.variable,
                           dependent.variable.type,
                           hypothesis.type)



#adjust data set to sampling frame formatting
df$settlement_type[df$idp_settlement=="yes"]<-"IDP"
df$settlement_type[df$idp_settlement=="no"]<-"HC"

sf$stratum_id <- paste(sf$district, sf$settlement_type, sep = "_")
df$stratum_id <- paste(df$district, df$settlement_type, sep = "_")

#re-renaming (because of renaming made for Tableau output)
names(df)[which(names(df) == "weights")]<-"weights_tableau"
names(df)[which(names(df) == "hand_washing_times.cough_sneeze")]<-"hand_washing_times.coughing"
names(df)[which(names(df) == "nutrition_barriers.not_enough_prov")]<-"nutrition_barriers.not"
df$school_transport[df$school_transport=="bus"]<-"business"

#weighting and save weights as column
weight.function <- map_to_weighting(sampling.frame = sf, 
                                    data.stratum.column = "stratum_id",
                                    sampling.frame.population.column = "Population",
                                    sampling.frame.stratum.column = "stratum_id",
                                    data = df)

df$weights_dap <- weight.function(df)

#check difference in weights
#sum(df$weights_tableau-df$weights_dap)

#load questionnaire
questionnaire<-load_questionnaire(data = df,
                                  questions = koboquestions,
                                  choices = kobochoices,
                                  choices.label.column.to.use = "label::English")

#test if all variable names fit
#dap_indicators$dependent.variable %in% names(df)

#adapt naming to hypegrammaR-style
dap_indicators$dependent.variable<-hypegrammaR:::to_alphanumeric_lowercase(dap_indicators$dependent.variable)

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

dap_national<-dap_indicators

#results-function (can take very long)
start_time <- Sys.time()                                                                                                                    #timer for fun
list_of_results_indicators_national <-  from_analysisplan_map_to_output(df,
                                                                        analysisplan = dap_national,
                                                                        weighting = weight.function,
                                                                        cluster_variable_name = NULL,
                                                                        questionnaire = questionnaire, confidence_level = 0.90)
end_time <- Sys.time()                                                                                                                      #timer
end_time - start_time                                                                                                                       #timer
#34 min for 15%, estimate 4h, was true

#save results as R file and get it 
list_of_results_indicators_national %>% saveRDS("output/list_of_results_indicators_national.RDS")
#list_of_results_indicators_national <- readRDS("output/list_of_results_indicators_national.RDS")

#get table of results and save as csv
big_table_indicators_national <- list_of_results_indicators_national$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
write.csv(big_table_indicators_national, file= paste0("output/big_table_indicators_national.csv"), row.names=FALSE)

#get table with p-values and F-statistic (if not only direct reporting)
#pval_table(list_of_results_indicators_national$results, "output/list_of_results_indicators_hypo_national.csv")

#get html output
hypegrammaR:::map_to_generic_hierarchical_html(list_of_results_indicators_national,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir ="output",
                                               filename = "list_of_results_indicators_national.html")



#######################################
###REPEAT FOR NATIONAL [IDP/NON-IDP]###
#######################################

dap_national_settlement<-dap_indicators
dap_national_settlement$independent.variable<-"settlement_type"
dap_national_settlement$independent.variable.type<-"categorical"

#results-function (can take very long)
start_time <- Sys.time()                                                                                                                    #timer for fun
list_of_results_indicators_national_settlement <-  from_analysisplan_map_to_output(df,
                                                             analysisplan = dap_national_settlement,
                                                             weighting = weight.function,
                                                             cluster_variable_name = NULL,
                                                             questionnaire = questionnaire, confidence_level = 0.90)
end_time <- Sys.time()                                                                                                                      #timer
end_time - start_time                                                                                                                       #timer
#34 min for 15%, estimate 4h, was true

#save results as R file and get it 
list_of_results_indicators_national_settlement %>% saveRDS("output/list_of_results_indicators_national_settlement.RDS")
#list_of_results_indicators_national_settlement <- readRDS("output/list_of_results_indicators_national_settlement.RDS")

#get table of results and save as csv
big_table_indicators_national_settlement <- list_of_results_indicators_national_settlement$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
write.csv(big_table_indicators_national, file= paste0("output/big_table_indicators_national_settlement.csv"), row.names=FALSE)

#get table with p-values and F-statistic (if not only direct reporting)
#pval_table(list_of_results_indicators_national_settlement$results, "output/list_of_results_indicators_hypo_national_settlement.csv")

#get html output
hypegrammaR:::map_to_generic_hierarchical_html(list_of_results_indicators_national_settlement,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir ="output",
                                               filename = "list_of_results_indicators_national_settlement.html")



#####################################
##########REPEAT FOR STATES##########
#####################################

dap_states<-dap_indicators
dap_states$repeat.for.variable<-"states"


#results-function (can take very long)
start_time <- Sys.time()                                                                                                                    #timer for fun
list_of_results_indicators_state <-  from_analysisplan_map_to_output(df,
                                                          analysisplan = dap_indicators_state,
                                                          weighting = weight.function,
                                                          cluster_variable_name = NULL,
                                                          questionnaire = questionnaire, confidence_level = 0.90)
end_time <- Sys.time()                                                                                                                      #timer
end_time - start_time                                                                                                                       #timer


#save results as R file and get it 
list_of_results_indicators_state %>% saveRDS("output/list_of_results_indicators_state.RDS")
#list_of_results_indicators_state <- readRDS("output/list_of_results_indicators_state.RDS")

#get table of results and save as csv
big_table_indicators_state <- list_of_results_indicators_state$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
write.csv(big_table_indicators_state, file= paste0("output/big_table_indicators_state.csv"), row.names=FALSE)

#get table with p-values and F-statistic (if not only direct reporting)
#pval_table(list_of_results_indicators_state$results, "output/list_of_results_indicators_hypo_state.csv")

#get html output
hypegrammaR:::map_to_generic_hierarchical_html(list_of_results_indicators_state,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir ="output",
                                               filename = "list_of_results_indicators_state.html")

#####################################
###REPEAT FOR STATES [IDP/NON-IDP]###
#####################################

dap_states_settlement<-dap_indicators
dap_states_settlement$repeat.for.variable<-"states"
dap_states_settlement$independent.variable<-"settlement_type"
dap_states_settlement$independent.variable.type<-"categorical"

#results-function (can take very long)
start_time <- Sys.time()                                                                                                                    #timer for fun
list_of_results_indicators_state <-  from_analysisplan_map_to_output(df,
                                                                     analysisplan = dap_states_settlement,
                                                                     weighting = weight.function,
                                                                     cluster_variable_name = NULL,
                                                                     questionnaire = questionnaire, confidence_level = 0.90)
end_time <- Sys.time()                                                                                                                      #timer
end_time - start_time                                                                                                                       #timer
#34 min for 15%, estimate 4h, was true


#save results as R file and get it 
list_of_results_indicators_state_settlement %>% saveRDS("output/list_of_results_indicators_state_settlement.RDS")
#list_of_results_indicators_state_settlement <- readRDS("output/list_of_results_indicators_state_settlement.RDS")

#get table of results and save as csv
big_table_indicators_state_settlement <- list_of_results_indicators_state_settlement$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
write.csv(big_table_indicators_state_settlement, file= paste0("output/big_table_indicators_state_settlement.csv"), row.names=FALSE)

#get table with p-values and F-statistic (if not only direct reporting)
#pval_table(list_of_results_indicators_state_settlement$results, "output/list_of_results_indicators_hypo_state_settlement.csv")

#get html output
hypegrammaR:::map_to_generic_hierarchical_html(list_of_results_indicators_state_settlement,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir ="output",
                                               filename = "list_of_results_indicators_state_settlement.html")


######################################
##########REPEAT FOR REGIONS##########
######################################

dap_region<-dap_indicators
dap_region$repeat.for.variable<-"region"

#results-function (can take very long)
start_time <- Sys.time()                                                                                                                    #timer for fun
list_of_results_indicators_region <-  from_analysisplan_map_to_output(df,
                                                          analysisplan = dap_region,
                                                          weighting = weight.function,
                                                          cluster_variable_name = NULL,
                                                          questionnaire = questionnaire, confidence_level = 0.90)
end_time <- Sys.time()                                                                                                                      #timer
end_time - start_time                                                                                                                       #timer
#34 min for 15%, estimate 4h, was true


#save results as R file and get it 
list_of_results_indicators_region %>% saveRDS("output/list_of_results_indicators_region.RDS")
#list_of_results_indicators_region <- readRDS("output/list_of_results_indicators_region.RDS")

#get table of results and save as csv
big_table_indicators_region <- list_of_results_indicators_region$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
write.csv(big_table_indicators_region, file= paste0("output/big_table_indicators_region.csv"), row.names=FALSE)

#get table with p-values and F-statistic (if not only direct reporting)
#pval_table(list_of_results_indicators_region$results, "output/list_of_results_indicators_hypo_region.csv")

#get html output
hypegrammaR:::map_to_generic_hierarchical_html(list_of_results_indicators_region,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir ="output",
                                               filename = "list_of_results_indicators_region.html")

######################################
###REPEAT FOR REGIONS[IDP/NON-IDP]####
######################################

dap_region_settlement<-dap_indicators
dap_region_settlement$repeat.for.variable<-"region"
dap_region_settlement$independent.variable<-"settlement_type"
dap_region_settlement$independent.variable.type<-"categorical"

#results-function (can take very long)
start_time <- Sys.time()                                                                                                                    #timer for fun
list_of_results_indicators_region_settlement <-  from_analysisplan_map_to_output(df,
                                                                                 analysisplan = dap_region_settlement,
                                                                                 weighting = weight.function,
                                                                                 cluster_variable_name = NULL,
                                                                                 questionnaire = questionnaire, confidence_level = 0.90)
end_time <- Sys.time()                                                                                                                      #timer
end_time - start_time                                                                                                                       #timer
#34 min for 15%, estimate 4h, was true


#save results as R file and get it 
list_of_results_indicators_region_settlement %>% saveRDS("output/list_of_results_indicators_region_settlement.RDS")
#list_of_results_indicators_region_settlement <- readRDS("output/list_of_results_indicators_region_settlement.RDS")

#get table of results and save as csv
big_table_indicators_region_settlement <- list_of_results_indicators_region_settlement$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
write.csv(big_table_indicators_region, file= paste0("output/big_table_indicators_region_settlement.csv"), row.names=FALSE)

#get table with p-values and F-statistic (if not only direct reporting)
#pval_table(list_of_results_indicators_region_settlement$results, "output/list_of_results_indicators_hypo_region_settlement.csv")

#get html output
hypegrammaR:::map_to_generic_hierarchical_html(list_of_results_indicators_region_settlement,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir ="output",
                                               filename = "list_of_results_indicators_region_settlement.html")

