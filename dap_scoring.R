######IMPORT#######################################################################################################################################################################################################################################################################################
setwd("C:/Users/Vanessa Causemann/Desktop/REACH/RStuff/GitHub/mainAnalysisSOM20")

df<-read.csv(file="input/REACH_SOM2006_JMCNA_IV_Data-Set_August2020_October_27_2020.csv", head=T, dec=".", sep=",",na.strings=c("NA",""," "))
raw<-read.csv(file="C:/Users/Vanessa Causemann/Desktop/REACH/Data/REACH_SOM2006_JMCNA_IV_Data-Set_August2020_October_27_2020_RAW.csv", head=T, dec=".", sep=",")


######FUNCTIONS######################################################################################################################################################################################################################################################################################

#get all dummy multiples, by their stacked character variable
#input: variable name and data frame ("variable", df) i.e. dot_multiples("market_goods", df) or dot_multiples("market_goods[.]",df) to only get the binaries
#output: string with all varaibles containing the name 
#_______________________________________________________________________________________________________________________________________________________________________________________
dot_multiples<-function(x,y){
  choice_var<-grep(x, names(y), value = TRUE)
  print(choice_var)
}


#recode to 0/1 and NA and aggregate non-critical indicators by guidance 0-33%->1; 34-66%->2 67-100%->3
#input: data frame with only non-critical indicators, scored 1/0, NA or 1-4+, NA
#output:one score per household 1-3, NA
#_______________________________________________________________________________________________________________________________________________________________________________________
agg_binary<-function(x){
  for (i in 1:dim(x)[2]){                                                           #loop over all columns in non-critical data frame
    if (length(which(x[i]==0))==0){                                                 #if not binary coded:
      x[i][(x[i]==1 | x[i]==2) & (!is.na(x[i]))]<-0                                 #recode 1&2 (if not NA) to 0
      x[i][(x[i]==3 | x[i]==3 | x[i]==4 | x[i]=="4+" )& (!is.na(x[i]))]<-1          #recode 3 &4 & 4+ (if not NA) to 1
    } 
    x[,i]<-as.numeric(x[,i])                                                        #make all columns numeric (issues due to 4+)
    #print(names(x[i]))                                                              #for debugging
    #print(table(x[i]))                                                              #for debugging
  }
  
  agg_score<-rep(NA, dim(x)[1])                                                     #initiate variable
  
  for (j in 1:dim(x)[1]){                                                           #loop over all rows in non-critical data frame
    ratio<- round((sum(x[j,], na.rm=T) / length(which(!is.na(x[j,])))), digits=2)   #calculate ratio of 1 scored indicators from all possible scored indicators (exclude NA)
    #print(ratio)                                                                   #for debugging
    if (is.na(ratio)){                                                              #if ratio NA
      agg_score[j]<-NA                                                              #make score NA 
    }
      else{
        if (ratio<=0.33){                                                           #score 1 if ratio smaller or equal 33%
          agg_score[j]<-1
        }
          else{ 
            if (ratio<=0.66){                                                       #score 2 if ratio bigger 33% and smaller or equal 66%
              agg_score[j]<-2
            }
              else{                                                                 #score 3 if ratio bigger 66%
                agg_score[j]<-3
              }
            }
        }
    }
    return(agg_score)
}


#aggregate critical and non-critical indicators by taking the max
#input: one data frame with non-critical, one with only critical indicators (non-critical,critical)
#output: one score per household 1-4+, NA
#_______________________________________________________________________________________________________________________________________________________________________________________
agg_LSG<-function(x,y){
  y[y=="4+"]<-5                                                                     #recode 4+ to 5
  for (i in 1:dim(y)[2]){
    if (length(which(y[i]==0))!=0)                                                  #loop over columns of critical indicators
    {print(paste0("Warning:binary scored:",names(y[i])))                            
    }
    y[,i]<-as.numeric(y[,i])                                                        #make all columns numeric (issues due to 4+)
  }
  agg_score<-as.numeric(agg_binary(x))                                              #get overall score from non-critical indicators
  sectoral_df<-data.frame(y, agg_score)                                             #make data frame out of the aggregated non-citical and critical indicators
  sectoral_df[is.na(sectoral_df)]<-0                                                #recode NA to 0 (causing issues if one row has only NA)
  sectoral_agg<-rep(NA, dim(x)[1])                                                  #initiate variable
  for (j in 1:dim(x)[1]){                                                           #loop over all households
    sectoral_agg[j]<-max(sectoral_df[j,], na.rm=T)                                  #take max 
  }
  sectoral_agg[sectoral_agg==5]<-"4+" 
  sectoral_agg[sectoral_agg==0]<-NA                                                 #recode 0 to NA
  return(sectoral_agg)                                                              #recode 5 to 4+
}


#aggregate critical indicators by taking the max
#input: one data frame with only critical indicators
#output: one score per household 1-4+, NA
#_______________________________________________________________________________________________________________________________________________________________________________________
agg_critical<-function(y){
  y[y=="4+"]<-5                                                                     #recode 4+ to 5
  for (i in 1:dim(y)[2]){
    if (length(which(y[i]==0))!=0){                                                 #loop over columns of critical indicators
      print(paste0("Warning:binary scored:",names(y[i])))                            
    }
    y[,i]<-as.numeric(y[,i])                                                        #make all columns numeric (issues due to 4+)
  }
  y[is.na(y)]<-0
  critical_agg<-rep(NA, dim(y)[1])                                                  #initiate variable
  for (j in 1:dim(y)[1]){                                                           #loop over all households
    critical_agg[j]<-max(y[j,], na.rm=T)                                            #take max 
  }
  critical_agg[critical_agg==5]<-"4+" 
  critical_agg[critical_agg==0]<-NA                                                 #recode 0 to NA
  return(critical_agg)                                                              #recode 5 to 4+
}

#recode 1-4+ to 1/0 indicators
#input: data frame with variables coded in 1-4+, NA
#output: data frame with variables coded 1/0, NA
#___________________________________________________________________________________________________________________________________________
re_code<-function(x){
  for (i in 1:dim(x)[2]){                                                            #loop over all columns
    if (length(which(x[i]==0))==0){                                                  #if not binary coded:
      x[i][(x[i]==1 | x[i]==2) & (!is.na(x[i]))]<-0                                  #recode 1&2 (if not NA) to 0
      x[i][(x[i]==3 | x[i]==3 | x[i]==4 | x[i]=="4+" )& (!is.na(x[i]))]<-1           #recode 3 &4 & 4+ (if not NA) to 1
    } 
    x[i]<-as.numeric(as.character(unlist(x[i])))                                     #make all columns numeric
    #print(names(x[i]))                                                               #for debugging
    #print(table(x[i]))                                                               #for debugging
    names(x)[i]<-paste0(names(x)[i], "_CG")
    }
  return(as.data.frame(x))
}


####################################################################################################################################################################################################################################################################################################
######NEW VARIABLES TO HELP CALCULATE SCORES########################################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################################################################

child_elderly<- (df$females_0m_5y + df$males_0m_5y + df$females_6_12+ df$males_6_12 + df$females_13_15 + df$males_13_15 + df$females_60_over + df$males_60_over)
adultish <- (df$females_16_17 + df$males_16_17 + df$females_18_40 + df$males_18_40 + df$females_41_59 + df$males_41_59)

df$today<-as.Date(df$today-1, origin = '1900-01-01')                                          #because of Excel formatting
days_displaced<-df$today - as.Date(df$left_aoo, "%d/%m/%Y")

#for adding and subtracting: data set with 0's instead of NA's
df_nona<-df
df_nona[is.na(df_nona)]=0

#SD: Shelter Density
df$SD<-df$hh_size/(df$sum_rooms-(df_nona$kitchens+df_nona$toilets))

df$no_nfi_items<-(df$nfi_access.wintting_blankets+
                    df$nfi_access.mattting_mats+
                    df$nfi_access.cookting_utensils+
                    df$nfi_access.cookting_fuel+
                    df$nfi_access.wateting_containers+
                    df$nfi_access.jerrting_can+
                    df$nfi_access.Torches+
                    df$nfi_access.solating_lamps+
                    df$nfi_access.solating_panels+
                    df$nfi_access.Generators+
                    df$nfi_access.Batteries+
                    df$nfi_access.Clothing+
                    df$nfi_access.wintting_clothing+
                    df$nfi_access.Shoes+
                    df$nfi_access.wintting_shoes+
                    df$nfi_access.wintting_heaters+
                    df$nfi_access.heatting_fuel+
                    df$nfi_access.dispting_diapers+
                    df$nfi_access.saniting_pads+
                    df$nfi_access.Soap+
                    df$nfi_access.washting_powder+
                    df$nfi_access.cleating_house+
                    df$nfi_access.deteting_dishes+
                    df$nfi_access.hygiting_kits+
                    df$nfi_access.handting_sanitiser+
                    df$nfi_access.faceting_masks+
                    df$nfi_access.dispting_gloves+
                    df$nfi_access.mosqting_Nets)


######FSC-Indicators##################################################################################################################################################

#rSCI: reduced Coping Strategy Index; multiply relevant questions with USW (universal severity weight) or with nothing if USW=1 and add
df$rCSI<- (df$rely_on_lessprefered + df$portion_limit + df$reduce_meals +(df$restrict_consumption*3) + (df$borrow_food*2))

#HHS: Household Hunger Scale; create numeric variables and add to up
#often,sometimes=1 rarely=0 
hhs1<-rep(0, dim(df)[1])
hhs1[df$fre_enough_food=="often"]<-2
hhs1[df$fre_enough_food=="sometimes"]<-1
hhs2<-rep(0, dim(df)[1])
hhs2[df$fre_night_day_nofood=="often"]<-2
hhs2[df$fre_night_day_nofood=="sometimes"]<-1
hhs3<-rep(0, dim(df)[1])
hhs3[df$fre_sleep_hungry=="often"]<-2
hhs3[df$fre_sleep_hungry=="sometimes"]<-1
df$HHS<-(hhs1+hhs2+hhs3)

######FSC-INDICATORS CHECK######

#install.packages("psych", dep = TRUE) 
#library(psych)
#IrCSI<-select(df,rely_on_lessprefered,portion_limit,reduce_meals,restrict_consumption,borrow_food)
#omega(IrCSI, nfactors=1)
#IHHS<-data.frame(hhs1,hhs2,hhs3)
#omega(IHHS, nfactors=1)


####################################################################################################################################################################################################################################################################################################
######PRE-EXISTING VULNERABILITIES##################################################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################################################################

######demographic#####################################################################################################################################################
df$A1<- rep(NA, nrow(df))
df$A1[df$household_expenditure=="adult_female" |df$household_expenditure=="adult_male"]<-1
df$A1[df$household_expenditure=="eldery_female" |df$household_expenditure=="eldery_male"]<-3
df$A1[df$household_expenditure=="female_14_17" |df$household_expenditure=="male_14_17"]<-4
df$A1[df$household_expenditure=="female_13" |df$household_expenditure=="male_13"]<-"4+"

df$A2[df$breadwinner=="adult_female" |df$breadwinner=="adult_male"]<-1
df$A2[df$breadwinner=="eldery_female" |df$breadwinner=="eldery_male"]<-3
df$A2[df$breadwinner=="female_14_17" |df$breadwinner=="male_14_17"]<-4
df$A2[df$breadwinner=="female_13" |df$breadwinner=="male_13"]<-"4+"

df$A3<- rep(NA, nrow(df))
df$A3[df$chronic_illness=="no"]<-0
df$A3[df$chronic_illness=="yes"]<-1

df$A4<- rep(NA, nrow(df))
df$A4[df$pregnancy=="no"]<-0
df$A4[df$pregnancy=="yes"]<-1

df$A5<- rep(NA, nrow(df))
df$A5[(child_elderly/adultish)<0.8]<-0
df$A5[(child_elderly/adultish)>=0.8]<-1


######socioeconomic###################################################################################################################################################

df$B1<- rep(NA, nrow(df))
df$B1[df$income_src.business==1 | df$income_src.cash_fishing==1 |df$income_src.livestock_production==1 | df$income_src.contracted_job==1|df$income_src.cash_crop_farming==1 | df$income_src.rent_of_land==1]<-1
df$B1[df$income_src.subsistence_farming_or_fishing==1 |df$income_src.remittances==1| df$income_src.daily_labour==1]<-3
df$B1[df$income_src.humanitarian_assistance==1|df$income_src.sale_of_humanitarian_assistance==1 | df$income_src.none==1]<-4

df$B2<- rep(NA, nrow(df))
df$B2[df$main_source_food.purchased_market==1 | df$main_source_food.own_cultivation==1 |df$main_source_food.own_livestock==1]<-1
df$B2[df$main_source_food.bartering_Bartering==1 | df$main_source_food.fishing_Fishing==1 |df$main_source_food.foraging_Foraging==1 | df$main_source_food.hunting_Hunting==1| df$main_source_food.other==1]<-3
df$B2[df$main_source_food.reliant_friends==1 | df$main_source_food.reliant_assistance==1| df$main_source_food.reliant_gov_assist==1]<-4

df$B3<- rep(NA, nrow(df))
df$B3[df$hh_members_new_unemployed==0]<-0
df$B3[df$hh_members_new_unemployed>0]<-1

df$B4<- rep(NA, nrow(df))
df$B4[df$hh_members_income>0]<-0
df$B4[df$hh_members_income==0]<-1

df$B5<- rep(NA, nrow(df))
df$B5[df$employ_loss_why.end_contract==1 | df$employ_loss_why.other==1]<-2
df$B5[df$employ_loss_why.displacement==1 | df$employ_loss_why.locusts==1 |df$employ_loss_why.covid==1 | df$employ_loss_why.ill==1]<-3
df$B5[df$employ_loss_why.flooding==1 | df$employ_loss_why.drought==1]<-4
df$B5[df$employ_loss_why.conflict==1]<-"4+"

######displacement####################################################################################################################################################

df$C1<- rep(NA, nrow(df))
df$C1[days_displaced<366]<-0
df$C1[days_displaced>=366]<-1

df$C2<- rep(NA, nrow(df))
df$C2[df$displaced_locs<2]<-0
df$C2[df$displaced_locs>1]<-1

#C3 not scored: df$hosting_idp_number
#C4 not scored: df$disp_why1 df$disp_why2
#C5 not scored: df$dest_loc_why1 df$dest_loc_why2
#C6 not scored: df$returnee_remain_here
#C7 not scored: df$when_continue
#C8 not scored: df$district


######documentation###################################################################################################################################################

df$D1<- rep(NA, nrow(df))
df$D1[df$certificate_ids=="all_id"|df$certificate_ids=="yes_id"]<-0
df$D1[df$certificate_ids=="some_id"|df$certificate_ids=="no_id"]<-1


######discrimination##################################################################################################################################################

df$E1<- rep(NA, nrow(df))
df$E1[df$factors_aid.None==1]<-0
df$E1[df$factors_aid.30.==1|df$factors_aid.60.==1|df$factors_aid.Disability._Person_living_with_a_disability==1|df$factors_aid.Heritage._A_member_of_a_minority_or_marginalised_community==1]<-1

#E2 not scored: length(dot_multiples("services_affected[.]",df))[df$services_affected.access_remedies df$services_affected.security df$services_affected.health df$services_affected.education df$services_affected.water df$services_affected.food df$services_affected.cash df$services_affected.work df$services_affected.other]


######COVID-related###################################################################################################################################################

df$F1<- rep(NA, nrow(df))
df$F1[df$soap_access=="yes"]<-0
df$F1[df$soap_access=="no"]<-1

df$F2<- rep(NA, nrow(df))
df$F2[df$hand_washing_facility=="buckets_with_taps"|df$hand_washing_facility=="sink_with_tap"|df$hand_washing_facility=="tippy_tap"]<-0
df$F2[df$hand_washing_facility=="no_specific"]<-1

df$F3<- rep(NA, nrow(df))
df$F3[df$health_time=="less15"|df$health_time=="16_30"|df$health_time=="31_60"]<-1
df$F3[df$health_transport=="car"|df$health_transport=="cart"|df$health_transport=="moto"|df$health_transport=="bus"]<-1
df$F3[df$health_transport=="walking" & (df$health_time=="60_180"|df$health_time=="above180")]<-4

df$F4<- rep(NA, nrow(df))
df$F4[df$market_time=="less15"|df$market_time=="min_15_29"|df$market_time=="min_30_59"]<-0
df$F4[df$market_transport=="car"|df$market_transport=="cart"|df$market_transport=="moto"|df$market_transport=="bus"]<-0
df$F4[(df$market_transport=="walking") & (df$market_time=="hr_1_2"|df$market_time=="more2h")]<-1

df$F5<- rep(0, nrow(df))
df$F5[df$chronic_illness=="yes" | df$females_60_over>0 | df$males_60_over>0]<-1

df$F6<- rep(NA, nrow(df))
df$F6[df$SD<=1]<-1
df$F6[1<df$SD & df$SD <=2]<-2
df$F6[2<df$SD & df$SD <=2.5]<-3
df$F6[df$SD>2.5]<-4

#____________________________aggregate to overall PEV ____________________________

df_PEV<-data.frame(df$A1, df$A2, df$A3,df$A4, df$A5, df$B1,df$B2, df$B3, df$B4,df$B5, df$C1, df$C2,df$D1, df$E1, df$F1,df$F2, df$F3, df$F4,df$F5, df$F6)
df$PEV<-agg_binary(df_PEV)

#_________________________________________________________________________________

####################################################################################################################################################################################################################################################################################################
######LIVING STANDARD & COPING GAPS [LSG's & CG's]##################################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################################################################

######education#######################################################################################################################################################

df$G1<- rep(NA, nrow(df))
df$G1[df$HH_schoolaged_children==df$enrollement_note]<-1
df$G1[df$HH_schoolaged_children>df$enrollement_note]<-3
df$G1[df$HH_schoolaged_children>0 & df$enrollement_note<1]<-4

df$G2<- rep(NA, nrow(df))
df$G2[df$HH_schoolaged_children!=0 & df$covid_enrollement==0]<-0
df$G2[df$covid_enrollement>0]<-1

#G3 not scored: length(dot_multiples("attend_covid19[.]",df)) [df$attend_covid19.children_stay_home df$attend_covid19.no_teachers df$attend_covid19.lack_transport df$attend_covid19.parent_stay_home df$attend_covid19.school_closed df$attend_covid19.other df$attend_covid19.no_answer]

df$G4<- rep(NA, nrow(df))
df$G4[df$HH_schoolaged_children>0 & df$enrollement_note2<1]<-1
df$G4[df$HH_schoolaged_children>df$enrollement_note2 & df$enrollement_note2>0]<-3
df$G4[df$HH_schoolaged_children==df$enrollement_note2]<-4

df$G5<- rep(NA, nrow(df))
df$G5[df$school_time=="less15"|df$school_time=="16_30"|df$school_time=="31_60"]<-0
df$G5[df$school_transport=="car"|df$school_transport=="cart"|df$school_transport=="moto"|df$school_transport=="bus"]<-0
df$G5[(df$school_transport=="walking") & (df$school_time=="above180"|df$school_time=="60_180")]<-1

df$G6<- rep(NA, nrow(df))
df$G6[df$HH_schoolaged_children>0 & df$education_type.don_know!=1 & (!is.na(df$education_type.don_know))]<-0
df$G6[df$HH_schoolaged_children>0 & df$education_type.none==1]<-1

df$G7<- rep(NA, nrow(df))
df$G7[df$HH_schoolaged_children>0 & df$HH_schoolaged_children==df$remote_education]<-0
df$G7[df$HH_schoolaged_children>0 & df$HH_schoolaged_children>df$remote_education]<-1

#G8 not scored: length(dot_multiples("remote_edu_via[.]",df))
#G9 not scored: length(dot_multiples("school_material[.]",df))

df$G10<- rep(NA, nrow(df))
df$G10[df$HH_schoolaged_children>0 & (df$school_barrier_note.no_barriers==1| df$school_barrier_note.other==1)]<-1
df$G10[df$HH_schoolaged_children>0 & (df$school_barrier_note.displaced==1| df$school_barrier_note.poor_performance==1 | df$school_barrier_note.curriculum==1| df$school_barrier_note.parents==1 |df$school_barrier_note.no_interest==1)]<-2
df$G10[df$HH_schoolaged_children>0 & (df$school_barrier_note.schools_closed==1| df$school_barrier_note.schools_overcrowded==1 | df$school_barrier_note.distance_to==1| df$school_barrier_note.school_fees==1 |df$school_barrier_note.registration==1 | df$school_barrier_note.overcrowded==1| df$school_barrier_note.lack_teachers==1 | df$school_barrier_note.poor_infrastrcture==1| df$school_barrier_note.wash==1 |df$school_barrier_note.wash_gender==1)]<-3
df$G10[df$HH_schoolaged_children>0 & (df$school_barrier_note.no_school==1| df$school_barrier_note.discrimination_enroll==1 | df$school_barrier_note.cannot_go==1| df$school_barrier_note.chores==1)]<-4
df$G10[df$HH_schoolaged_children>0 & df$school_barrier_note.security_concerns==1]<-"4+"

df$G11<- rep(NA, nrow(df))
df$G11[df$HH_schoolaged_children>0 & (df$school_barrier_girls_note.no_barriers==1| df$school_barrier_girls_note.other==1)]<-1
df$G11[df$HH_schoolaged_children>0 & (df$school_barrier_girls_note.displaced==1| df$school_barrier_girls_note.poor_performance==1 | df$school_barrier_girls_note.curriculum==1| df$school_barrier_girls_note.parents==1 |df$school_barrier_girls_note.no_interest==1)]<-2
df$G11[df$HH_schoolaged_children>0 & (df$school_barrier_girls_note.schools_closed==1| df$school_barrier_girls_note.schools_overcrowded==1 | df$school_barrier_girls_note.distance_to==1| df$school_barrier_girls_note.school_fees==1 |df$school_barrier_girls_note.registration==1 | df$school_barrier_girls_note.overcrowded==1| df$school_barrier_girls_note.lack_teachers==1 | df$school_barrier_girls_note.poor_infrastrcture==1| df$school_barrier_girls_note.wash==1 |df$school_barrier_girls_note.wash_gender==1)]<-3
df$G11[df$HH_schoolaged_children>0 & (df$school_barrier_girls_note.no_school==1| df$school_barrier_girls_note.discrimination_enroll==1 | df$school_barrier_girls_note.cannot_go==1| df$school_barrier_girls_note.chores==1)]<-4
df$G11[df$HH_schoolaged_children>0 & df$school_barrier_girls_note.security_concerns==1]<-"4+"

#G12 not scored: length(dot_multiples("cash_education[.]",df))
#G13 not scored: length(dot_multiples("home_learning[.]",df))


#____________________________aggregate to sectoral LSG____________________________

df_G_bin<-data.frame(df$G5, df$G6, df$G7)
df_G_crit<-data.frame(df$G1, df$G10, df$G11)
df$LSG_education<-agg_LSG(df_G_bin,df_G_crit)

#_________________________________________________________________________________

######health##########################################################################################################################################################

df$H1<- rep(NA, nrow(df))
df$H1[df$health_household.none==1]<-1
df$H1[df$health_household.yes_diarhea==1 | df$health_household.yes_fever==1 |df$health_household.yes_cough==1 | df$health_household.yes_skin==1 |df$health_household.yes_eye==1 | df$health_household.yes_wound==1 ]<-4

df$H2<- rep(NA, nrow(df))
df$H2[df$health_access=="no" | df$health_access=="no_seek"]<-0
df$H2[df$health_access=="yes"]<-1

df$H3<- rep(NA, nrow(df))
df$H3[df$health_seek.private_clinic==1 | df$health_seek.private_physician==1 |df$health_seek.private_pharmacy==1 | df$health_seek.other_private_medical==1|df$health_seek.gov_hospital==1 | df$health_seek.government_center==1 | df$health_seek.government_post==1|df$health_seek.other_public==1 | df$health_seek.community_worker==1|df$health_seek.mobile_clinic_outreach==1 |df$health_seek.community_health_worker==1 |df$health_seek.mobile_clinic==1]<-0
df$H3[df$health_seek.relative_friend==1|df$health_seek.shop_street==1 |df$health_seek.other==1 |df$health_seek.traditional_practitioner==1]<-1

df$H4<- rep(NA, nrow(df))
df$H4[df$children_vaccine_age>0 & df$unvaccinated_child=="no"]<-0
df$H4[df$children_vaccine_age>0 & df$unvaccinated_child=="yes"]<-1

df$H5<- rep(NA, nrow(df))
df$H5[df$unvaccinated_why.no_issues==1 | df$unvaccinated_why.have_not==1]<-1
df$H5[df$unvaccinated_why.public_not_open==1 | df$unvaccinated_why.lack_staff== 1]<-2
df$H5[df$unvaccinated_why.cost_high==1 | df$unvaccinated_why.problems_civil== 1 | df$unvaccinated_why.public_clinic==1 | df$unvaccinated_why.treatment_toofar== 1| df$unvaccinated_why.no_medicine==1 | df$unvaccinated_why.no_treament== 1]<-3
df$H5[df$unvaccinated_why.medical_refused==1 | df$unvaccinated_why.no_pwd==1]<-4

df$H6<- rep(NA, nrow(df))
df$H6[df$birth_where=="doctor" | df$birth_where=="nurse" | df$birth_where=="health_center" | df$birth_where=="government_health" | df$birth_where=="other_public" | df$birth_where=="private_hospital" | df$birth_where=="private_clinic" | df$birth_where=="private_maternity" | df$birth_where=="other_private"]<-0
df$H6[df$birth_where=="respondent_s" | df$birth_where=="other_home" ]<-1

df$H7<- rep(NA, nrow(df))
df$H7[df$who_assist=="government_hospital" | df$who_assist=="government_clinic" | df$who_assist=="other_health" | df$who_assist=="traditional" | df$who_assist=="community"]<-1
df$H7[df$who_assist=="relative" | df$who_assist=="other" ]<-3
df$H7[df$who_assist=="no" ]<-"4+"

df$H8<- rep(NA, nrow(df))
df$H8[df$health_time=="less15"|df$health_time=="16_30"|df$health_time=="31_60"]<-1
df$H8[df$health_transport=="car"|df$health_transport=="cart"|df$health_transport=="moto"|df$health_transport=="bus"]<-1
df$H8[(df$health_transport=="walking"|df$health_transport=="have_not"|df$health_transport=="treatment_toofar") & (df$health_time=="60_180"|df$health_time=="above180")]<-4

df$H9<- rep(NA, nrow(df))
df$H9[df$health_mobile=="yes"]<-0
df$H9[df$health_mobile=="no"]<-1

df$H10<- rep(NA, nrow(df))
df$H10[df$barriers_health.no_issues==1 | df$barriers_health.have_not==1]<-1
df$H10[df$barriers_health.lack_staff==1 | df$barriers_health.public_not_open==1]<-2
df$H10[df$barriers_health.cost_high==1 | df$barriers_health.problems_civil==1 | df$barriers_health.public_clinic==1 | df$barriers_health.treatment_toofar==1 | df$barriers_health.no_medicine==1 | df$barriers_health.no_treament==1]<-3
df$H10[df$barriers_health.medical_refused==1 | df$barriers_health.no_pwd==1]<-4

#H11 not scored: length(dot_multiples("cash_health[.]",df))


#____________________________aggregate to sectoral LSG____________________________

df_H_bin<-data.frame(df$H2, df$H3, df$H4, df$H5, df$H6, df$H10)
df_H_crit<-data.frame(df$H1, df$H7, df$H8)
df$LSG_health<-agg_LSG(df_H_bin,df_H_crit)

#_________________________________________________________________________________

######nutrition#######################################################################################################################################################

df$I1<- rep(NA, nrow(df))
df$I1[df$unusually_sleepy=="no" & df$child_fever=="no"]<-1
df$I1[df$unusually_sleepy=="yes_7less" & df$child_fever=="yes_7less"]<-3
df$I1[df$unusually_sleepy=="yes_7more" & df$child_fever=="yes_7more"]<-3

df$I2<- rep(NA, nrow(df))
df$I2[df$child_thin=="no"]<-0
df$I2[df$child_thin=="yes"]<-1

df$I3<- rep(NA, nrow(df))
df$I3[df$eating_normally=="yes_eating"]<-1
df$I3[df$eating_normally=="no_less2"]<-3
df$I3[df$eating_normally=="no_less3"]<-4

df$I4<- rep(NA, nrow(df))
df$I4[df$child_thin=="no"]<-0
df$I4[df$child_thin=="yes"]<-1

df$I5<- rep(NA, nrow(df))
df$I5[df$nutrition_time=="less15"|df$nutrition_time=="16_30"|df$nutrition_time=="31_60"]<-0
df$I5[df$Nutrition_transport=="car"|df$Nutrition_transport=="cart"|df$Nutrition_transport=="moto"|df$Nutrition_transport=="bus"]<-0
df$I5[df$Nutrition_transport=="walking" & (df$nutrition_time=="60_180"|df$nutrition_time=="above180")]<-1

df$I6<- rep(NA, nrow(df))
df$I6[df$nutrition_barriers.none==1]<-1
df$I6[df$nutrition_barriers.unaware_services==1 | df$nutrition_barriers.unaware_supplements==1 | df$nutrition_barriers.facilities_not_staffed==1 | df$nutrition_barriers.not_enough_prov==1]<-2
df$I6[df$nutrition_barriers.difficulty==1 | df$nutrition_barriers.facilities==1 | df$nutrition_barriers.prohibitive==1 ]<-3
df$I6[df$nutrition_barriers.insecurity==1 | df$nutrition_barriers.inaccessible==1 | df$nutrition_barriers.inaccessible_clans==1 ]<-4

#____________________________aggregate to sectoral LSG____________________________

df_I_bin<-data.frame(df$I2, df$I4, df$I5)
df_I_crit<-data.frame(df$I1, df$I3, df$I6)
df$LSG_nutrition<-agg_LSG(df_I_bin,df_I_crit)

#_________________________________________________________________________________


######food security [SFC]###############################################################################################################################################

df$J1<- rep(NA, nrow(df))
df$J1[df$main_source_food.purchased_market==1 | df$main_source_food.own_cultivation==1 | df$main_source_food.own_livestock==1]<-1
df$J1[df$main_source_food.fishing_Fishing==1 | df$main_source_food.foraging_Foraging==1 | df$main_source_food.hunting_Hunting==1 | df$main_source_food.bartering_Bartering==1 | df$main_source_food.other==1]<-3
df$J1[df$main_source_food.reliant_friends==1 | df$main_source_food.reliant_assistance==1 | df$main_source_food.reliant_gov_assist==1 ]<-4

df$J2<- rep(NA, nrow(df))
df$J2[df$HHS==0]<-1
df$J2[df$HHS==1]<-2
df$J2[df$HHS==2 | df$HHS==3]<-3
df$J2[df$HHS==4]<-4
df$J2[df$HHS==5 | df$HHS==6]<-"4+"

df$J3<- rep(NA, nrow(df))
df$J3[df$rCSI<=3]<-1
df$J3[df$rCSI>3 & df$rCSI<=18]<-2
df$J3[df$rCSI>18 & df$rCSI<=42]<-3
df$J3[df$rCSI>42]<-4

df$J4<- rep(NA, nrow(df))
df$J4[df$market_time=="less15"|df$market_time=="min_15_29"|df$market_time=="min_30_59"]<-0
df$J4[df$market_transport=="car"|df$market_transport=="cart"|df$market_transport=="moto"|df$market_transport=="bus"]<-0
df$J4[(df$market_transport=="walking") & (df$market_time=="hr_1_2"|df$market_time=="more2h")]<-1

#____________________________aggregate to sectoral LSG____________________________

df_J_bin<-data.frame(df$J4)
df_J_crit<-data.frame(df$J1, df$J2, df$J3)
df$LSG_FSC<-agg_LSG(df_J_bin,df_J_crit)

#_________________________________________________________________________________


######water sanitation & hygiene [WASH] ##############################################################################################################################

df$K1<- rep(NA, nrow(df))
df$K1[(df$water_source_time=="water_premises" | df$water_source_time=="less_5_min" | df$water_source_time=="between_and_15_min" |df$water_source_time=="between_and_30_min") & (df$water_source=="public_tap" | df$water_source=="handpumps_Handpump"| df$water_source=="protected_well"| df$water_source=="piped_neighbors"| df$water_source=="protected_Protected"| df$water_source=="bottled_water"| df$water_source=="private_tap"| df$water_source=="neighbourhood_support")]<-1
df$K1[df$water_source_time=="more_31min"| df$water_source=="unprotected_well" | df$water_source=="water_seller"| df$water_source=="unprotected_spring"| df$water_source=="rain_water"| df$water_source=="tanker_Tanker"]<-3
df$K1[df$water_source=="surface_dam"]<-"4+"

df$K2<- rep(NA, nrow(df))
df$K2[df$enough_water.drinking==1]<-1
df$K2[df$enough_water.drinking==0]<-4

df$K3<- rep(NA, nrow(df))
df$K3[df$enough_water.drinking==1 & df$enough_water.cooking==1 & df$enough_water.personal_hygiene_==1 & df$enough_water.other__domestic_purposes_==1 & df$enough_water.not_enough_water_==0]<-1
df$K3[df$enough_water.other__domestic_purposes_==0] <-2
df$K3[df$enough_water.personal_hygiene_==0] <-3
df$K3[df$enough_water.cooking==0] <-4
df$K3[which(df$enough_water.not_enough_water_==1 & (df$enough_water.drinking==1 | df$enough_water.cooking==1 | df$enough_water.personal_hygiene_==1 | df$enough_water.other__domestic_purposes_==1))]<-NA
#ran into issues, see above, 244 survey getting NA due to inconsistency and see below:
#which(is.na(df$K3)&df$enough_water.not_enough_water_==0)         #75 NA surveys no drinking water, covered by K2

df$K4<- rep(NA, nrow(df))
df$K4[df$sanitation_facility=="flush_toilet"| df$sanitation_facility=="pit_latrine_with"| df$sanitation_facility=="pit_VIP" ]<-1
df$K4[df$sanitation_facility=="pit_latrine_without" | df$sanitation_facility=="open_hole"| df$sanitation_facility=="other" | df$no_toilet>3]<-3
df$K4[df$sanitation_facility=="none_of"]<-4

df$K5<- rep(NA, nrow(df))
df$K5[df$soap_access=="yes"]<-1
df$K5[df$soap_access=="no"]<-3

df$K6<- rep(NA, nrow(df))
df$K6[(df$latrine_features.door + df$latrine_features.access + df$latrine_features.walls_ + df$latrine_features.inside + df$latrine_features.lock + df$latrine_features.outside + df$latrine_features.close + df$latrine_features.marked + df$latrine_features.soap)>6]<-0
df$K6[(df$latrine_features.door + df$latrine_features.access + df$latrine_features.walls_ + df$latrine_features.inside + df$latrine_features.lock + df$latrine_features.outside + df$latrine_features.close + df$latrine_features.marked + df$latrine_features.soap)<7]<-1

df$K7<- rep(NA, nrow(df))
df$K7[df$waste_disposal=="covered_pit" | df$waste_disposal=="burial"]<-0
df$K7[df$waste_disposal=="burning" | df$waste_disposal=="In_open"]<-1

df$K8<- rep(NA, nrow(df))
df$K8[df$water_barrier.no_problem==1]<-1
df$K8[df$water_barrier.water_expensive==1 | df$water_barrier.not_water==1]<-2
df$K8[df$water_barrier.water_market==1 | df$water_barrier.insufficient_points.==1 | df$water_barrier.water_close==1 | df$water_barrier.waterpoints_far==1 | df$water_barrier.don_water==1]<-3
df$K8[df$water_barrier.waterpoints_disabilities==1 | df$water_barrier.some_waterpoints==1]<-4
df$K8[df$water_barrier.fetching_activity==1]<-"4+"

df$K9<- rep(NA, nrow(df))
df$K9[df$of_water.doesn_t_have_issue==1]<-1
df$K9[df$of_water.less_preferred_cooking==1 | df$of_water.surface_water_cooking==1| df$of_water.unsual_source==1]<-2
df$K9[df$of_water.rely_less_preferred==1 | df$of_water.dangerious_other==1]<-3
df$K9[df$of_water.or_credit_for==1 | df$of_water.consumption_less==1]<-4
df$K9[df$of_water.surface_water==1| df$of_water.send_children_water==1 | df$of_water.dangerious_source==1 ]<-"4+"

df$K10<- rep(NA, nrow(df))
df$K10[df$sanitation_barriers.no_problem==1]<-1
df$K10[df$sanitation_barriers.lack_crowded==1 | df$sanitation_barriers.sanitation_unhygienic==1]<-2
df$K10[df$sanitation_barriers.sanitation_full==1 | df$sanitation_barriers.sanitation_far==1]<-3
df$K10[df$sanitation_barriers.sanitation_disabilities==1 | df$sanitation_barriers.some_toilets==1 | df$sanitation_barriers.sanitation_women==1 | df$sanitation_barriers.sanitation_etc==1]<-4
df$K10[df$sanitation_barriers.going_dangerous==1]<-"4+"
#df[which(is.na(df$K10) & df$sanitation_barriers.other==0 & df$sanitation_barriers.don_know==0), 488:500]

df$K11<- rep(NA, nrow(df))
df$K11[df$sanitation_coping.no_problem==1]<-1
df$K11[df$sanitation_coping.less_prefered_toilets==1]<-2
df$K11[df$sanitation_coping.latrines_toilets==1]<-3
df$K11[df$sanitation_coping.usual_one==1 | df$sanitation_coping.dangerous_place==1 | df$sanitation_coping.at_night==1]<-4
df$K11[df$sanitation_coping.plastic_bag==1 | df$sanitation_coping.the_open==1]<-"4+"
#df[which(is.na(df$K11) & (!is.na(df$sanitation_coping.plastic_bag)) & df$sanitation_coping.don_know!=1 & df$sanitation_coping.other!=1), 502:512]

df$K12<- rep(NA, nrow(df))
df$K12[df$hygeine_coping.any_issue==1]<-1
df$K12[df$hygeine_coping.rely_less_preferred==1 | df$hygeine_coping.buying_at_further==1]<-2
df$K12[df$hygeine_coping.rely_soap_substitutes==1 | df$hygeine_coping.borrow_from_relative==1 | df$hygeine_coping.spend_or_used==1 | df$hygeine_coping.reduce_consumption_consumption==1 | df$hygeine_coping.reduce_consumption_cleaning==1]<-3
df$K12[df$hygeine_coping.buying_at_dangerious==1 ]<-"4+"

df$K13<- rep(NA, nrow(df))
df$K13[df$menstrual_barriers.no_problem==1]<-0
df$K13[df$menstrual_barriers.no_access==1 | df$menstrual_barriers.no_money==1]<-1

#K14 not scored: length(dot_multiples("hand_washing_times[.]",df))

#____________________________aggregate to sectoral LSG____________________________

df_K_bin<-data.frame(df$K3, df$K9, df$K6, df$K10, df$K11, df$K12, df$K7, df$K13)
df_K_crit<-data.frame(df$K1, df$K2, df$K4, df$K5, df$K8)
df$LSG_WASH<-agg_LSG(df_K_bin,df_K_crit)

#_________________________________________________________________________________


######shelter & non-food items [SNFI]#################################################################################################################################

df$L1<- rep(NA, nrow(df))
df$L1[df$SD<=1]<-1
df$L1[1<df$SD & df$SD <=2]<-2
df$L1[2<df$SD & df$SD <=2.5]<-3
df$L1[df$SD>2.5]<-4

df$L2<- rep(NA, nrow(df))
df$L2[df$shelter_type=="brick" | df$shelter_type=="stone" |df$shelter_type=="normal house"]<-1
df$L2[df$shelter_type=="cgi" | df$shelter_type=="mud" | df$shelter_type=="collective" | (df$shelter_type=="buul" & df$idp_settlement=="no")| df$shelter_type=="stick"| df$shelter_type=="timer_"]<-2
df$L2[df$shelter_type=="unfinished" | df$shelter_type=="tent"]<-3
df$L2[(df$shelter_type=="buul" & df$idp_settlement=="yes") | df$shelter_type=="makeshift" ]<-4
df$L2[(is.na(df$shelter_type) & df$hh_shelter==0) | df$shelter_type=="none" ]<-"4+"

df$L3<- rep(NA, nrow(df))
df$L3[df$lack_enclosure.none_of==1 | df$lack_enclosure.leaks_light_rain==1 | df$lack_enclosure.limited_room==1 | df$lack_enclosure.presence_removable==1]<-0
df$L3[df$lack_enclosure.lack_insulation==1 | df$lack_enclosure.leaks_heavy_rain==1 | df$lack_enclosure.presence_debris==1 ]<-1

df$L4<- rep(NA, nrow(df))
df$L4[df$shelter_damage.none_of==1 |df$shelter_damage.opening_cracks==1 | df$shelter_damage.broken_or==1 | df$shelter_damage.some_in==1 | df$shelter_damage.damaged_Damage==1  | df$shelter_damage.foundation_damaged==1| df$shelter_damage.gas_sewage==1 | df$shelter_damage.electricity_and==1  | df$shelter_damage.other==1]<-1
df$L4[df$shelter_damage.roof_Roof==1 | df$shelter_damage.exterior_to==1 | df$shelter_damage.exterior_or==1 | df$shelter_damage.large_in==1 | df$shelter_damage.total_Total==1 | df$shelter_damage.some_walls==1]<-3
df$L4[df$shelter_damage.severe_unsafe==1]<-4
#df[which(is.na(df$L4)&df$shelter_damage.not_sure!=1),557:574]

#L5: not scored: length(dot_multiples("unable_repair[.]",df)) 

df$L6<- rep(NA, nrow(df))
df$L6[df$shelter_issues.none==1 |df$shelter_issues.unable_home==1 | df$shelter_issues.other==1 ]<-0
df$L6[df$shelter_issues.lack_partitions==1 |df$shelter_issues.lack_inside==1 | df$shelter_issues.cooking_are==1 | df$shelter_issues.lack_per==1 | df$shelter_issues.lack_around==1  | df$shelter_issues.bathing_are==1| df$shelter_issues.lack_bathing==1 | df$shelter_issues.lack_cooking==1  | df$shelter_issues.theft==1| df$shelter_issues.other_security==1 | df$shelter_issues.fire==1  | df$shelter_issues.poor_of==1]<-1

#L7: not scored: df$hh_cook

df$L8<- rep(NA, nrow(df))
df$L8[df$occupancy_arrangement=="ownership" | df$occupancy_arrangement=="rented" |df$occupancy_arrangement=="hosted"]<-0
df$L8[df$occupancy_arrangement=="no_occu" | df$occupancy_arrangement=="other" ]<-1

df$L9<- rep(NA, nrow(df))
df$L9[df$written_documentation=="yes" ]<-0
df$L9[df$written_documentation=="no"]<-1

df$L10<- rep(NA, nrow(df))
df$L10[df$hlp_problems.disputes_tenant==1 |df$hlp_problems.rules_clear==1 | df$hlp_problems.none==1  | df$hlp_problems.inheritance_issues==1]<-0
df$L10 [df$hlp_problems.lack_documents==1 |df$hlp_problems.looting_property==1 | df$hlp_problems.threat_others==1 | df$hlp_problems.disputed_ownership==1 | df$hlp_problems.property_occupation==1  | df$hlp_problems.other==1]<-1
df$L10[which(df$hlp_problems.none==1 & df$hlp_problems.not_sure==1)]<-NA

df$L11<- rep(NA, nrow(df))
df$L11[df$no_nfi_items==28]<-1
df$L11[df$no_nfi_items<28 & df$no_nfi_items>4]<-2
df$L11[df$no_nfi_items<5 & df$no_nfi_items>1]<-3
df$L11[df$no_nfi_items<2]<-4

#L12: not scored: length(dot_multiples("shetler_support[.]",df))

#____________________________aggregate to sectoral LSG____________________________

df_L_bin<-data.frame(df$L3, df$L6, df$L8, df$L9, df$L10, df$L1)
df_L_crit<-data.frame(df$L2, df$L4, df$L11)
df$LSG_SNFI<-agg_LSG(df_L_bin,df_L_crit)

#_________________________________________________________________________________


######protection######################################################################################################################################################

df$M1<- rep(NA, nrow(df))
df$M1[df$child_labor_note=="no" & df$HH_schoolaged_children>0]<-1
df$M1[df$child_labor_note=="yes" &df$HH_schoolaged_children>0]<-3

df$M2<- rep(NA, nrow(df))
df$M2[df$children_away=="no"]<-1
df$M2[df$child_study>0 | df$child_employment>0]<-2
df$M2[df$child_married>0]<-3
df$M2[df$child_armed>0 | df$child_abducted>0 | df$child_missing>0 | df$child_detained>0]<-"4+"
#df[which(is.na(df$M2) & df$children_away!="dontknow"), 720:735]

df$M3<- rep(NA, nrow(df))
df$M3[df$hh_restrictions.no==1]<-1
df$M3[df$hh_restrictions.yes_3==1]<-3
df$M3[df$hh_restrictions.yes_2==1]<-4
df$M3[df$hh_restrictions.yes_1==1]<-"4+"
df$M3[df$hh_restrictions.not_tried==1]<-NA
#df[which(!is.na(df$M3) & df$hh_restrictions.not_tried==1),  731:737]

df$M4<- rep(NA, nrow(df))
df$M4[df$not_safe_boys=="no"]<-1
df$M4[df$not_safe_boys=="yes"]<-"4+"

#M5: not scored: length(dot_multiples("where_not_safe_boys[.]",df))

df$M6<- rep(NA, nrow(df))
df$M6[df$not_safe_girls=="no"]<-1
df$M6[df$not_safe_girls=="yes"]<-"4+"

#M7: not scored: length(dot_multiples("where_not_safe_girls[.]",df))

df$M8<- rep(NA, nrow(df))
df$M8[df$hh_incidents=="no"]<-1
df$M8[df$hh_incidents=="yes"]<-3

df$M9<- rep(NA, nrow(df))
df$M9[df$child_friendly=="yes"]<-0
df$M9[df$child_friendly=="no"]<-1

df$M10<- rep(NA, nrow(df))
df$M10[df$medical_services=="yes"]<-0
df$M10[df$medical_services=="no"]<-1

df$M11<- rep(NA, nrow(df))
df$M11[df$gbv_incidents=="no"]<-1
df$M11[df$gbv_incidents=="yes"]<-4

df$M12<- rep(NA, nrow(df))
df$M12[df$medical_services=="yes"]<-0
df$M12[df$medical_services=="no"]<-1

#____________________________aggregate to sectoral LSG____________________________

df_M_bin<-data.frame(df$M9, df$M10, df$M12, df$M6, df$M4, df$M11)
df_M_crit<-data.frame(df$M1, df$M8, df$M2, df$M3)
df$LSG_protection<-agg_LSG(df_M_bin,df_M_crit)

#_________________________________________________________________________________


####################################################################################################################################################################################################################################################################################################
######ACCOUNTABILITY TO AFFECTED PEOPLE [AAP] & COVID-INFO##########################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################################################################

######AAP#############################################################################################################################################################

#N1: not scored: length(dot_multiples("phone_type[.]",df))

df$N2<- rep(NA, nrow(df))
df$N2[df$read_write=="yes_without"| df$read_write=="yes_some"]<-0
df$N2[df$read_write=="yes_alot"| df$read_write=="no"]<-1

#N3: not scored: table(df$aid_received)
#N4: not scored: table(df$information_channel)
#N5: not scored: table(df$aid_satisfaction)

df$N6<- rep(NA, nrow(df))
df$N6[df$aid_access=="no"]<-0
df$N6[df$aid_access=="yes"]<-1

df$N7<- rep(NA, nrow(df))
df$N7[df$aid_barriers.lack_information==1 |df$aid_barriers.physical_access==1 | df$aid_barriers.other==1]<-0
df$N7[df$aid_barriers.insecure_route==1 |df$aid_barriers.insecure_site==1 | df$aid_barriers.exclusion_gatekeepers==1]<-1
#df[which(is.na(df$N7) & (!is.na(df$aid_barriers.exclusion_gatekeepers))),830:835]

#N8: not scored: table(df$trust_aid_workers)
#N9: not scored: table(df$feedback_mechanisms)
#N10: not scored: length(dot_multiples("prefer_feedback_channel[.]",df))
#N11: not scored: length(dot_multiples("humanitarian_assistance[.]",df))
#N12: not scored: table(df$note_households_support), all NA
#N13: not scored: table(df$first_priority)
#N14: not scored: table(df$second_priority)
#N15: not scored: table(df$third_priority)
#N16: not scored: table(df$un_continue)
#N17: not scored: table(df$un_stop)
#N18: not scored: length(dot_multiples("factors_aid[.]",df))
#N18: not scored: length(dot_multiples("services_affected[.]",df))


######COVID#############################################################################################################################################################

#O1: not scored: length(dot_multiples("trust_covidinfo[.]",df))
#O2: not scored: length(dot_multiples("prefer_covidinfo[.]",df))
#O3: not scored: length(dot_multiples("action_to_prevent[.]",df))
#O4: not scored: length(dot_multiples("why_no_action[.]",df))
#O5: not scored: length(dot_multiples("signs_covid[.]",df))
#O6: not scored: length(dot_multiples("hh_covid_action[.]",df))


####################################################################################################################################################################################################################################################################################################
######COPING GAPS & MULTI-SECTOR NEEDS INDEX [CG'S & MSNI]##########################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################################################################

CG<-c("G2", "G4", "J3", "K9", "K11", "K12", "M1", "M2")
df_CG<-df[CG]
df_CG_recoded<-re_code(df_CG)
df<-cbind(df, df_CG_recoded)

LSG<-c("LSG_education", "LSG_health", "LSG_nutrition", "LSG_FSC", "LSG_WASH", "LSG_SNFI", "LSG_protection" )
df_MSNI<-df[LSG]
df$MSNI<-agg_critical(df_MSNI)


####################################################################################################################################################################################################################################################################################################
######EXPORT########################################################################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################################################################

today <- Sys.Date()
today<-format(today, format="_%Y_%b_%d")

write.csv(df, paste0("output/REACH_SOM2006_JMCNA_IV_Data-Set_with_indicators_scored",today,".csv"), row.names=FALSE)
#write.csv(df_MSNI, paste0("output/MSNI_LSG",today,".csv"), row.names=FALSE)

