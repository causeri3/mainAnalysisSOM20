######IMPORT#######################################################################################################################################################################################################################################################################################
setwd("C:/Users/Vanessa Causemann/Desktop/REACH/RStuff/GitHub/mainAnalysisSOM20")

df<-read.csv(file="input/REACH_SOM2006_JMCNA_IV_Data-Set_August2020_October_27_2020.csv", head=T, dec=".", sep=",",na.strings=c("NA",""," "))
raw<-read.csv(file="C:/Users/Vanessa Causemann/Desktop/REACH/Data/REACH_SOM2006_JMCNA_IV_Data-Set_August2020_October_27_2020_RAW.csv", head=T, dec=".", sep=",")

####################################################################################################################################################################################################################################################################################################
######NEW VARIABLES TO HELP CALCULATE SCORES########################################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################################################################

child_elderly<- (df$females_0m_5y + df$males_0m_5y + df$females_6_12+ df$males_6_12 + df$females_13_15 + df$males_13_15 + df$females_60_over + df$males_60_over)
adultish <- (df$females_16_17 + df$males_16_17 + df$females_18_40 + df$males_18_40 + df$females_41_59 + df$males_41_59)

df$today<-as.Date(df$today-1, origin = '1900-01-01')                                          #because of Excel formatting
days_displaced<-df$today - as.Date(df$left_aoo, "%d/%m/%Y")

#SD: Shelter Density
kitchens<-df$kitchens
kitchens[is.na(kitchens)]<-0
toilets<-df$toilets
toilets[is.na(toilets)]<-0
df$SD<-df$hh_size/(df$sum_rooms-(kitchens+toilets))
######FSC-Indicators####################################################################################################

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

######demographic######################################################################################################
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

######socioeconomic######################################################################################################

df$B1<- rep(NA, nrow(df))
df$B1[df$income_src.business=="1" | df$income_src.cash_fishing=="1" |df$income_src.livestock_production=="1" | df$income_src.contracted_job=="1"|df$income_src.cash_crop_farming=="1" | df$income_src.rent_of_land=="1"]<-1
df$B1[df$income_src.subsistence_farming_or_fishing=="1" |df$income_src.remittances=="1"| df$income_src.daily_labour]<-3
df$B1[df$income_src.humanitarian_assistance=="1"|df$income_src.sale_of_humanitarian_assistance=="1" | df$income_src.none=="1"]<-4

df$B2<- rep(NA, nrow(df))
df$B2[df$main_source_food.purchased_market=="1" | df$main_source_food.own_cultivation=="1" |df$main_source_food.own_livestock=="1"]<-1
df$B2[df$main_source_food.bartering_Bartering=="1" | df$main_source_food.fishing_Fishing=="1" |df$main_source_food.foraging_Foraging=="1" | df$main_source_food.hunting_Hunting=="1"| df$main_source_food.other=="1"]<-3
df$B2[df$main_source_food.reliant_friends=="1" | df$main_source_food.reliant_assistance=="1"| df$main_source_food.reliant_gov_assist=="1"]<-4

#check<-(df$main_source_food.purchased_market + df$main_source_food.own_cultivation + df$main_source_food.own_livestock + df$main_source_food.bartering_Bartering + df$main_source_food.fishing_Fishing + df$main_source_food.foraging_Foraging + df$main_source_food.hunting_Hunting + df$main_source_food.other + df$main_source_food.reliant_friends + df$main_source_food.reliant_assistance + df$main_source_food.reliant_gov_assist)


#ran into issue, see:
#df$main_source_food[which(is.na(df$B2))]
#df$main_source_food.purchased_market[which(is.na(df$B2))]
#there are more wrong entries than the ones causing issues
#df$main_source_food.purchased_market[which(df$main_source_food=="purchased_market ")]

#0 have income_scr.none=1
#check to see which surveys got changed to 0 in the cleaned data set from the raw one
#u_raw_income<-raw$X_uuid[which(raw$income_src.none==1)]
#index<-rep(0, length(u_raw_income))
#for (i in 1:length(u_raw_income)){
#if (is.integer(which(df$X_uuid==u_raw_income[i])) && length(which(df$X_uuid==u_raw_income[i])) == 0) {index[i]<-NA}
#  else{index[i]<-which(df$X_uuid==u_raw_income[i])}}
#index<-index[!is.na(index)]
#length(index)
#304
#df$B2[index]
#one of those is causing an NA in B2

#discussions about inverse????
#df$B3<- rep(NA, nrow(df))
#df$B3[(df$hh_members_income/df$hh_size)<0.8]<-0
#df$B3[(df$hh_members_income/df$hh_size)>=0.8]<-1

#mean(df$hh_members_income/df$hh_size)
#0.02711341
#about 3% is the mean of hh_members working

df$B3<- rep(NA, nrow(df))
df$B3[df$hh_members_new_unemployed==0]<-0
df$B3[df$hh_members_new_unemployed>0]<-1

df$B4<- rep(NA, nrow(df))
df$B4[df$hh_members_income>0]<-0
df$B4[df$hh_members_income==0]<-1


df$B5<- rep(NA, nrow(df))
df$B5[df$employ_loss_why.end_contract=="1" | df$employ_loss_why.other=="1"]<-1
df$B5[df$employ_loss_why.displacement=="1" | df$employ_loss_why.locusts=="1" |df$employ_loss_why.covid=="1" | df$employ_loss_why.ill=="1"]<-3
df$B5[df$employ_loss_why.floodings=="1" | df$employ_loss_why.drought=="1"]<-4
df$B5[df$employ_loss_why.conflict=="1"]<-"4+"


######displacement######################################################################################################

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

#df$C4<- rep(NA, nrow(df))
#df$C4[df$hosting_idp=="no"]<-0
#df$C4[df$hosting_idp=="yes"]<-1

#df$displaced_days<-(df$difference_arrived_left_days+df$difference_arrived_today_days)
#df$C5<- rep(NA, nrow(df))
#df$C5<-((df$difference_arrived_left_days+df$difference_arrived_today_days)/df$displaced_locs)
#length(which(df$C5==Inf)) #344
#length(which(df$displaced_locs==0))
#df$difference_arrived_today_days[df$displaced_locs==0 & !is.na(df$displaced_loc)]
#df$difference_arrived_left_days[df$displaced_locs==0& !is.na(df$displaced_loc)]
#df$difference_arrived_today_days[which(df$displaced_locs==0)]
#df$difference_arrived_left_days[which(df$displaced_locs==0)]
#df$difference_arrived_left_days[which(df$displaced_locs==0)]
#df$difference_arrived_today_days[which(df$displaced_locs==0)]


#df$displaced_locs[which(df$displaced_locs==0)]<-1

#df$displaced_days<-(df$difference_arrived_left_days+df$difference_arrived_today_days)
#df$C5<- rep(NA, nrow(df))
#df$C5<-((df$difference_arrived_left_days+df$difference_arrived_today_days)/df$displaced_locs)

#boxplot(df$C5,main = "difference days / displaced locs", add = F, ann = FALSE, yaxt = "n")
#axis(2, at = seq(0, 15000, 500), las = 2)
#grid()

#hist(df$C5,breaks = seq(0, 100000, 100))

#ggplot(df) + 
#  geom_histogram(aes(x = C5),# y = (..count..)), 
#                 bins = 100, fill = "gray", colour = "black") +
  #stat_function(fun = dnorm, 
  #              args = list(mean = mean(df$C5, na.rm=T), 
  #                          sd = sd(df$C5, na.rm=T)),
  #              colour = "salmon", size = 1.25) +
#  xlab("Incidator displaced days/locations") +
#  ylab("Frequency")

#library(ggplot2)

#ggplot(data = df, aes(y=C5)) +
#  geom_boxplot() +
  #xlab("Number of Cylinders") +
#  ylab("difference days / displaced locs") +
#  theme_bw()

#ggplot(data = df, aes(x=displaced_days, y=displaced_locs)) +
#  geom_boxplot() +
#  xlab("days displaced") +
#  ylab("# displaced locations") +
#  theme_bw()

######documentation######################################################################################################

df$D1<- rep(NA, nrow(df))
df$D1[df$certificate_ids=="all_id"|df$certificate_ids=="yes_id"]<-0
df$D1[df$certificate_ids=="some_id"|df$certificate_ids=="no_id"]<-1


#df$D1<- rep(NA, nrow(df))
#df$D1[df$written_documentation=="no"]<-0
#df$D1[df$written_documentation=="yes"]<-1

#Faraz: barriers due to documentation, does not fit with answers in dap
#df$D2<- rep(NA, nrow(df))
#df$D2[df$hlp_problems.none==1]<-0
#df$D2[df$written_documentation=="yes"]<-1

#options:
#hlp_problems.disputed_ownership
#hlp_problems.property_occupation
#hlp_problems.disputes_tenant
#hlp_problems.rules_clear
#hlp_problems.inheritance_issues
#hlp_problems.lack_documents
#hlp_problems.looting_property
#hlp_problems.threat_others
#hlp_problems.other
#hlp_problems.not_sure

######discrimination######################################################################################################

df$E1<- rep(NA, nrow(df))
df$E1[df$factors_aid.None==1]<-0
df$E1[df$factors_aid.30.==1|df$factors_aid.60.==1|df$factors_aid.Disability._Person_living_with_a_disability==1|df$factors_aid.Heritage._A_member_of_a_minority_or_marginalised_community==1]<-1

#E2 not scored: df$services_affected (all dummies: df$services_affected.access_remedies df$services_affected.security df$services_affected.health df$services_affected.education df$services_affected.water df$services_affected.food df$services_affected.cash df$services_affected.work df$services_affected.other)

######COVID-related#######################################################################################################

df$F1<- rep(NA, nrow(df))
df$F1[df$soap_access=="buckets_with_taps"]<-0
df$F1[df$soap_access=="no"]<-1

df$F2<- rep(NA, nrow(df))
df$F3[df$hand_washing_facility=="buckets_with_taps"|df$hand_washing_facility=="sink_with_tap"|df$hand_washing_facility=="tippy_tap"]<-0
df$F2[df$hand_washing_facility=="no_specific"]<-1

df$F3<- rep(NA, nrow(df))
df$F3[df$health_time=="less15"|df$health_time=="16_30"|df$health_time=="31_60"]<-1
#df$F3[(df$health_time=="60_180"|df$health_time=="above180")&(df$health_transport=="car"|df$health_transport=="cart"|df$health_transport=="moto"|df$health_transport=="bus")]<-1
#35 households with 0 instead NA for following version:
df$F3[df$health_transport=="car"|df$health_transport=="cart"|df$health_transport=="moto"|df$health_transport=="bus"]<-1
df$F3[(df$health_transport=="walking"|df$health_transport=="have_not"|df$health_transport=="treatment_toofar") & (df$health_time=="60_180"|df$health_time=="above180")]<-4

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

#df$chronic_illness_hh_members.eldery_male
#df$chronic_illness_hh_members.eldery_female
#df$chronic_illness_hh_members.adult_male
#df$chronic_illness_hh_members.adult_female
#df$chronic_illness_hh_members.male_14_17
#df$chronic_illness_hh_members.female_14_17
#df$chronic_illness_hh_members.male_13
#df$chronic_illness_hh_members.female_13

####################################################################################################################################################################################################################################################################################################
######LIVING STANDARD & COPING GAPS#################################################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################################################################

######education######################################################################################################

df$G1<- rep(NA, nrow(df))
df$G1[df$HH_schoolaged_children==df$enrollement_note]<-1
df$G1[df$HH_schoolaged_children>df$enrollement_note]<-3
df$G1[df$HH_schoolaged_children>0 & df$enrollement_note<1]<-4

df$G2<- rep(NA, nrow(df))
df$G2[df$HH_schoolaged_children!=0 & df$covid_enrollement==0]<-0
df$G2[df$covid_enrollement>0]<-1

#G3 not scored: df$attend_covid19 (all dummies: df$attend_covid19.children_stay_home df$attend_covid19.no_teachers df$attend_covid19.lack_transport df$attend_covid19.parent_stay_home df$attend_covid19.school_closed df$attend_covid19.other df$attend_covid19.no_answer)

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

#G8 not scored: df$remote_edu_via (all dummies)
#G9 not scored: df$school_material (all dummies)

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

#G12 not scored: df$cash_education (all dummies)
#G13 not scored: df$home_learning (all dummies)

######health######################################################################################################

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
df$H7[df$who_assist=="government_hospital" | df$who_assist=="government_clinic" | df$who_assist=="other_health" | df$who_assist=="traditional" | df$who_assist=="community"]<-0
df$H7[df$who_assist=="relative" | df$who_assist=="other" ]<-1


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

#H11 not scored: df$cash_health (all dummies)

######nutrition######################################################################################################

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

######food security######################################################################################################

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

######Water Sanitation & Hygiene [WASH] ############################################################################################







######################Pregancy issue###############################################################################################
incon30<-(df$pregnancy=="yes" & (df$females_16_17+df$females_13_15+df$females_18_40+df$females_41_59)==0)
i30<-which(incon30)
u30<-df$X_uuid[which(incon30)]
#v30<-rep("pregnancy=yes, but no females between 13&59 years in hh", length(i30))
#uuid<-c(uuid,u30)
#index<-c(index,i30)
#variables<-c(variables,v30)

incon31<-(df$pregnancy=="" & (df$females_16_17+df$females_13_15+df$females_18_40+df$females_41_59)>0)
i31<-which(incon31)
u31<-df$X_uuid[which(incon31)]

incon31<-(df$pregnancy=="" & (df$females_16_17+df$females_18_40+df$females_13_15)>0)
i31<-which(incon31)
u31<-df$X_uuid[which(incon31)]

#incon31<-(df2$pregnancy=="" & (df2$females_16_17+df2$females_13_15+df2$females_18_40+df2$females_41_59)>0)
#i31<-which(incon31)
#u31<-df2$X_uuid[which(incon31)]

incon32<-(raw$pregnancy=="" & (raw$females_16_17+raw$females_13_15+raw$females_18_40+raw$females_41_59)>0)
i32<-which(incon32)
u32<-raw$X_uuid[which(incon32)]

incon32<-(raw$pregnancy=="" & (raw$females_16_17+raw$females_13_15+raw$females_18_40)>0)
i32<-which(incon32)
u32<-raw$X_uuid[which(incon32)]

u<-c(u31,u32)
length(unique(u))