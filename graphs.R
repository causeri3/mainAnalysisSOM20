#devtools::install_github("impact-initiatives/Setviz")
#devtools::install_github("impact-initiatives/msni19")
#devtools::install_github("impact-initiatives/surveyweights")
#install.packages("ggradar")
#install.packages("backports")
library(msni19)
library(Setviz)
library(surveyweights)
sessionInfo()

#set working directory
setwd("C:/Users/Vanessa Causemann/Desktop/REACH/RStuff/GitHub/mainAnalysisSOM20")

jmcna<-read.csv(file="output/REACH_SOM2006_JMCNA_IV_Data-Set_with_indicators_scored_2021_Jan_07.csv", head=T, dec=".", sep=",", stringsAsFactors = F)
sf<-read.csv(file="input/sampling_frame.csv", head=T, dec=".", sep=",", stringsAsFactors = F)

#adjust data set to sampling frame formatting
jmcna$settlement_type[jmcna$idp_settlement=="yes"]<-"IDP"
jmcna$settlement_type[jmcna$idp_settlement=="no"]<-"HC"

sf$stratum_id <- paste(sf$district, sf$settlement_type, sep = "_")
jmcna$stratum_id <- paste(jmcna$district, jmcna$settlement_type, sep = "_")

names(jmcna)[which(names(jmcna) == "weights")]<-"weights_tableau"

weighting_function <- surveyweights::weighting_fun_from_samplingframe(sampling.frame = sf,
                                                                      data = jmcna,
                                                                      sampling.frame.population.column ="Population",
                                                                      sampling.frame.stratum.column = "stratum_id",
                                                                      data.stratum.column = "stratum_id"
                                                                      )



LSG<-c("LSG_education", "LSG_health", "LSG_nutrition", "LSG_FSC", "LSG_WASH", "LSG_SNFI", "LSG_protection" )

#re-code LSG's to numeric 
jmcna[LSG][jmcna[LSG]=="4+"]<-5
jmcna[LSG]<-lapply(jmcna[LSG],as.numeric)

#decided on threshold of 3% which means 10 intersections plotted
inter<- index_intersections(jmcna, 
                            lsg = LSG, 
                            lsg_labels = c("Education", "Health", "Nutrition","FSC", "WASH",  "SNFI", "Protection"),
                            y_label = "% in need per combination of sectors",
                            index_filter = c(3, 4, 5),
                            weighting_function = weighting_function,
                            nintersects = 10,
                            exclude_unique = T,
                            mutually_exclusive_sets = T,
                            round_to_1_percent = T,
                            print_plot = F,
                            plot_name = "intersection"
                            )

pdf("output/graphs/LSG_intersection.pdf", width = ((11.4*1.5)/cm(1)), height = ((9.3*1.5)/cm(1))) #InDesign: H: 9.3 cm X W: 8.5 cm, since ~ 1/3 chopping off: W:11.4
inter
dev.off()


radar<- msni19::radar_graph(jmcna, 
                            lsg = LSG,
                            lsg_labels = c("Education", "Health", "Nutrition","FSC", "WASH",  "SNFI", "Protection"),
                            group = "settlement_type",
                            group_order = c("IDP","HC"),
                            group_labels =  c("IDP","HC"),
                            weighting_function = weighting_function,
                            legend_position = "left",
                            print_plot = F,
                            plot_name = "LSG_radar"
                            )

pdf("output/graphs/LSG_radar.pdf", width = ((9.3*2)/cm(1)), height = ((6.6*2)/cm(1))) #InDesign: H: 6.6cm X W: 9.3 
radar
dev.off()

##########################
#####JUST FOR 4 an 4+#####
##########################

#decided on threshold of 3% which means 6 intersections plotted
inter2<- index_intersections(jmcna,
                             lsg = LSG, 
                             lsg_labels = c("Education", "Health", "Nutrition","FSC", "WASH",  "SNFI", "Protection"),
                             y_label = "% in need per combination of sectors",
                             index_filter = c(4, 5),
                             weighting_function = weighting_function,
                             nintersects = 6,
                             exclude_unique = T,
                             mutually_exclusive_sets = T,
                             round_to_1_percent = T,
                             print_plot = F,
                             plot_name = "intersection"
                             )

pdf("output/graphs/LSG_intersection_4s.pdf", ((11.4*1.5)/cm(1)), height = ((9.3*1.5)/cm(1))) #InDesign: H: 9.3 cm X W: 8.5 cm, since ~ 1/3 chopping off: W:11.4
inter2
dev.off()

#no argument index filter. therefore:
jmcna[LSG][jmcna[LSG]==3]<-2

radar2<- msni19::radar_graph(jmcna,
                             lsg = LSG, 
                             lsg_labels = c("Education", "Health", "Nutrition","FSC", "WASH",  "SNFI", "Protection"),
                             group = "settlement_type",
                             group_order = c("IDP","HC"),
                             group_labels =  c("IDP","HC"),
                             weighting_function = weighting_function,
                             legend_position = "left",
                             print_plot = F,
                             plot_name = "LSG_radar"
                             )

pdf("output/graphs/LSG_radar_4s.pdf", width = ((9.3*2)/cm(1)), height = ((6.6*2)/cm(1))) #InDesign: H: 6.6cm X W:9.3 
radar2
dev.off()

###graph not very informative: no good cross-cutting CG and 99% scoring LSG 3 and above#####################################################################################################################################################################

#don't have a good cross-cutting coping gap index (capacity gap)
msni19::venn_msni(jmcna, 
                  lsg = LSG, 
                  capacity_gaps = "K9",
                  weighting_function = weighting_function,
                  print_plot = F,
                  plot_name = "LSG_CG_K9_venn",
                  path = "output/graphs")

#"plot_set_percentages is the generic function, the index_intesections is a wrapper around it and it calculates around the lsg specfically"
#plot_set_percentages(jmcna,#hno,
#                     LSG,#varnames = c("Sev..score.IPC", "Sev...score.GAM", "Sev.score..Access.to.an.improved.water.source", "Sev.score..Access.to.a.sufficient.quantity.of.water", "Sev.score..Access.to.adequate..appropriate.and.functional.sanitation.facilities", "Sev..score.HFCs"),
#                     mutually_exclusive_sets = F,
#                     exclude_unique = T,
#                     round_to_1_percent = F,
#                     nintersects = 10,
#                     weighting_function = weighting_function#,
#weight_variable = "weights_tableau"
#                     )