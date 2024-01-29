#===========================================================================

# File: ELE data prep 
# Author: scarlet cockell
# Update Date: 7/11/2023

#===========================================================================

#===========================================================================
# Purpose:
#  This script serves as a combination of Yuan's code and mine, with 
#  updated inclusion & exclusion criteria created 7/2023
#  one code to rule all data prep for the ELE project 
#===========================================================================

#=================================================================================================================
# **Data Set Explanation**
#
#  "ELE_43233obs_1104.RData": 
#      Yuans earliest data set i could find before most data cleaning was done
#                       
#
#  "ELE_43233_212_smc.RData": 
#      Exact data set above  but I selected only for variables we need (and the cognition data is merged)
#
#  "cogfinalimp_9520wide.sas7bdat"
#      cognition data set from HRS website- years 1995-2020- not cleaned   
#
#
#  "ELE_COGNITION.RData":
#       Cognition data cleaned, selected years 2008-2018 & ready for merge 
#       This is due to differences in coding the cognition variable.
#       
#
#  "ELE_16254_240_smc.RData":
#       This is the final data set "one_hrs" that I have been using. This includes all variables run in the 
#        Analysis and all combination variables. 
#
#
# #put in full source path!!
#  /AD_R01_WB/Staff/yuanjinn/earlylife/source/
#==================================================================================================================

#path of folders
script_path= "G:/.shortcut-targets-by-id/1dGRzB0bSRxxSqHT9WaXQ13_WuVFFZaE7/earlylife/script/"
source_path= "G:/.shortcut-targets-by-id/1dGRzB0bSRxxSqHT9WaXQ13_WuVFFZaE7/earlylife/source/"

#libraries
library(tidyverse)
library(data.table)
library(vcd)
library(generics)
library(plyr)
library(ggplot2)
library(visR)
library(gtsummary)
library(haven)
library(dplyr)
library(mde)
library(car)
library(epitools)
library(reshape2)
library(ggflowchart)
library(generics)
library(naniar)


############################--ALL THE DATASETS--##########################################
#Yuans full data set                                                                     #
load(paste0(source_path, "ELE_43233obs_1104.RData"))                                     #
                                                                                         #
#Scarlets data set before data cleaning (selected only variables needed for analysis)    #
one_hrs<-readRDS(paste0(source_path, "ELE_43233_212_smc.RData"))                         #
                                                                                         #
#cognition data before merge & cleaning                                                  #
df<-readRDS(paste0(source_path, "cogfinalimp_9520wide.sas7bdat"))                        #
                                                                                         #
#cognition data after cleaning                                                           #                           
df<-readRDS(paste0(source_path, "ELE_COGNITION.RData"))                                  #
                                                                                         #
#final data set                                                                          #
one_hrs<-readRDS(paste0(source_path, "ELE_16509_241_smc.RData")) #with NEWcombo vars     #
##########################################################################################
eleclocks<-readRDS(paste0(source_path, "ele_clocks_01192024.RData")) #with NEWcombo vars

#load functions 
source(paste0(script_path, "functions.R")) 


###########################--how scarlet selected only variables needed for analysis from Yuans full data set--#######################################################
one_hrs<- select(hrs.new,1:4,"RACE_HISP","childrural","CHILDHOODSES",8,16,1273:1313) #WITH ONLY B VARS
one_hrs<- select(hrs.new,1:4,"MAX_PARENTAL_EDU","RACE_HISP","childrural","CHILDHOODSES",8,16,1076:1114, 1116:1154, 1156:1234, 1273:1313)#WITH ALL YEARS AND B VARS 

######################################################################################################################################################################


#-------------------------------PART 1 
#merging in 2016/2018 cognition data 

#load data 
df<-read_sas(paste0(source_path, "cogfinalimp_9520wide.sas7bdat"))
df <- select(df, HHID, PN, cogfunction2008,cogfunction2010,cogfunction2012,cogfunction2014,cogfunction2016, cogfunction2018) #subset only columns we need

#reformat PN & HHID to remove leading zeros
df$HHID<-as.numeric(df$HHID)  
df$PN<-as.numeric(df$PN) 

#rename columns to match one_hrs cogfunction2016-> AD16, cogfunction2018-> AD18
setnames(df, old=c("cogfunction2008","cogfunction2010","cogfunction2012","cogfunction2014","cogfunction2016","cogfunction2018"), new=c("AD08","AD10","AD12","AD14","AD16", "AD18"))

#merge with cleaned data set 
#returns all matching rows from df
one_hrs= one_hrs %>% left_join(df,by=c("HHID","PN")) #43233obs 213var


#------------------------------------------DATA CLEANING-------------------------------------------------------------------------#

exposures <- c(
  #1-
  "B049", # "LIVED RURAL AREA DURING SCHOOL"
  #2-CHILDHOOD
  "B019", # "RATE HEALTH AS CHILD"
  "B099", # "CHILDHOOD - MISSED SCHOOL"
  "B119", # "CHILDHOOD- CONCUSSION OR SEVERE HEAD INJURY"
  "B120", # "CHILDHOOD- DISABILITY"
  "B121M1M", # "CAUSE OF DISABILITY -1 MASKED"
  "B121M2M", # "CAUSE OF DISABILITY -2 MASKED"
  "B122", # "CHILDHOOD-SMOKING"
  "B123", # "CHILDHOOD-LEARNING PROBLEMS"
  "B124", # "CHILDHOOD-ANY OTHER CONDITIONS"
  "B125M1M", # "CHILDHOOD CONDITIONS - 1 MASKED"
  "B125M2M", # "CHILDHOOD CONDITIONS - 2 MASKED"
  "B126", # "CHILDHOOD HEALTH FOLLOW-UPS INTRO"
  "B022", # "FAMILY GET FINANCIAL HELP IN CHILDHOOD"
  "B023", # "FATHER UNEMPLOYED DURING CHILDHOOD"
  "B025", # "LIVE WITH GRANDPARENTS DURING CHILDHOOD"
  "B088", # "MOTHER WORK DURING CHILDHOOD"
  #3-Before Age 16
  "B100", # "MEASLES BEFORE AGE 16"
  "B101", # "MUMPS BEFORE AGE 16"
  "B102", # "CHICKEN POX BEFORE AGE 16"
  "B103", # "DIFFICULTY SEEING BEFORE AGE 16"
  "B104", # "PARENTS/GUARDIANS SMOKE"
  "B105", # "ASTHMA BEFORE AGE 16"
  "B106", # "DIABETES BEFORE AGE 16"
  "B107", # "RESPIRATORY DISORDER BEFORE AGE 16"
  "B108", # "SPEECH IMPAIRMENT BEFORE AGE 16"
  "B109", # "ALLERGIC CONDITION BEFORE AGE 16"
  "B110", # "HEART TROUBLE BEFORE AGE 16"
  "B111", # "EAR PROBLEMS BEFORE AGE 16"
  "B112", # "EPILEPSY OR SEIZURES BEFORE AGE 16"
  "B113", # "HEADACHES OR MIGRAINES BEFORE AGE 16"
  "B114", # "STOMACH PROBLEMS BEFORE AGE 16"
  "B115", # "HIGH BP BEFORE AGE 16"
  "B116", # "DEPRESSION BEFORE AGE 16"
  "B117", # "DRUGS/ALCOHOL PROBLEMS BEFORE AGE 16"
  "B118", # "OTHER PSYCH PROBLEMS BEFORE AGE 16"
  #4-EDU
  "B026", # "FATHER EDUCATION- HIGHEST GRADE"
  "B027", # "MOTHER EDUCATION- HIGHEST GRADE"
  #5-
  "B020"  # "RATE FAMILY FINANCIAL SITUATION - SES"
) 


#-------------------------------Applying INCLUSION/EXCLUSION CRITERIA-------------------------------------------------------------------------#

#-------------------------------PART 2.1 
#Exclude Incomplete Demographics 
one_hrs <- one_hrs[!is.na(one_hrs$SEX),] #43210
one_hrs <- one_hrs[!is.na(one_hrs$RACE_HISP),] #42102
one_hrs <- one_hrs[!is.na(one_hrs$SCHLYRS),] #41915
one_hrs <- one_hrs[!is.na(one_hrs$MAX_PARENTAL_EDU),] #34248

#Exclude missing all exposures
Allmiss <- apply(one_hrs[,exposures], 1, function(row) all(is.na(row))) #34248
one_hrs <- one_hrs[!Allmiss,] #22018




#-------------------------------PART 2.2- 
#code categories with small sample size as NA                                                                                                                   #
#"Never lived with Mother/Mother not alive" from mother work during childhood           
one_hrs<- one_hrs %>%                                                                           
  mutate(mother_wrk= ifelse2(B088=="Never lived with Mather/ Mather was not alive" | B088=="ICON", NA, B088)) 
     

#add in labels
y_n <- c("mother_wrk")
for (var in y_n ) { 
  
  one_hrs[,var]<- factor(one_hrs[,var], 
                         levels= c(1,2,3),
                         labels= c("Not at all",          
                                   "All of the time",                       
                                   "Some of the time"))
}


#"it varied" from family financial situation                                            
one_hrs<- one_hrs %>%                                                                         
  mutate(fam_finance= ifelse2(B020=="It varied" | B020=="ICON", NA, B020))                                   

#add in labels
y_n <- c("fam_finance")
for (var in y_n ) { 
  
  one_hrs[,var]<- factor(one_hrs[,var], 
                         levels= c(1,3,4),
                         labels= c("Poor",          
                                   "About Avg",                       
                                   "Pretty well off financially"))
}


#-------------------------------PART 3    
##recode "Inconsistent" answers as NA for those who answered questions twice during exposure measure
#ICON <- apply(one_hrs2[,exposures], 1, function(row) any(row %in% c("ICON"))) 
one_hrs<- one_hrs %>% replace_with_na_all(condition = ~.x == "ICON")



#-------------------------------PART 4
#finding what year people answered ELE questions

##creating indicator variable for each wave
for(x in exposures) {
  
  dat <- select(one_hrs,(starts_with("LB")))
  dat2 <- select(one_hrs,(starts_with("MB")))
  dat3 <- select(one_hrs,(starts_with("NB")))
  dat4 <- select(one_hrs,(starts_with("OB")))
  wave = rep(NA,nrow(one_hrs))
  
  for(i in 1:nrow(one_hrs)) {
    
    if (sum(!is.na(dat[i,])))                #year 2008
      wave[i] = 2008
    else if (sum(!is.na(dat2[i,])))           #year 2010
      wave[i] = 2010
    else if (sum(!is.na(dat3[i,])))           #year 2012
      wave[i] = 2012
    else if (sum(!is.na(dat4[i,])))           #year 2014
      wave[i] = 2014
  }
  
  one_hrs["wave"]<-wave #make new column saving output
  
}



#-------------------------------PART 5
#select for normal cognitive status (3) at entry to cohort 

#identifies earliest cognition visit 
one_hrs<- one_hrs %>% 
  mutate(firstC= if_else(!is.na(AD08), 2008,  #first visit in 2008
                 if_else(!is.na(AD10), 2010,  #first visit in 2012
                 if_else(!is.na(AD12), 2012,  #first visit in 2010
                 if_else(!is.na(AD14), 2014,  #first visit in 2014
                 if_else(!is.na(AD16), 2016,
                 if_else(!is.na(AD18), 2018,
                                       NA_real_)))))))  #no cognitive measure after 2008


#identifies most recent cognition visit: updated to include 16 and 18 cognition data
one_hrs<- one_hrs %>% 
  mutate(.end= if_else(!is.na(AD18), 2018,  #most recent visit in 2018
               if_else(!is.na(AD16), 2016,  #most recent visit in 2016
               if_else(!is.na(AD14), 2014,  #most recent visit in 2014
               if_else(!is.na(AD12), 2012,  #most recent visit in 2012
               if_else(!is.na(AD10), 2010,  #most recent visit in 2010
               if_else(!is.na(AD08), 2008,  #most recent visit in 2008
                                     NA_real_)))))))  #no cognitive measure after 2008


#indicator to tell which year had normal cognition: updated to include 16 and 18 cognition data
one_hrs<- one_hrs %>% 
  mutate(dem_status= ifelse2(AD08==1, "norml08",  
                     ifelse2(AD10==1, "norml10",  
                     ifelse2(AD12==1, "norml12",  
                     ifelse2(AD14==1, "norml14",
                     ifelse2(AD16==1, "norml16",
                     ifelse2(AD18==1, "norml18",
                                      "cog_imparment")))))))  





#indicator telling us what year they completed ELE and their cognitive status 
one_hrs = one_hrs %>% 
     mutate(normal_ele = case_when(wave== "2008" ~ AD08,
                                   wave== "2010" ~ AD10,
                                   wave== "2012" ~ AD12,
                                   wave== "2014" ~ AD14))



#-------------------------------PART 6
#creating outcome var
#create Y: Dementia History
#Create Y1- "Most recent cognition what is the most recent cognition (1,2,3) 
#Create Y2- Ever dementia, then most recent cognition ever dementia, if not - then the most recent cognition (1,2,3)
#1-normal, 2-CIND, 3-Dementia #works for dementia but missing some ppl diagnosed with CIND ever--fixed
one_hrs= one_hrs %>%
  mutate(EVER_DEMENTIA_1 = coalesce(AD18, AD16, AD14, AD12, AD10, AD08), #ADDED 18/16
         EVER_DEMENTIA_2 = ifelse2(AD08==3|AD10==3|AD12==3|AD14==3|AD16==3|AD18==3, 3, 
                                   ifelse2(AD08==2|AD10==2|AD12==2|AD14==2|AD16==2|AD18==2, 2, EVER_DEMENTIA_1))) #ADDED 18/16

#create X- AGE_BASELINE
#create X- Time from first cogniton visit until last cognition visit 
one_hrs= one_hrs %>% 
  mutate(BIRTHYR= ifelse(BIRTHYR==0, NA, BIRTHYR),
         AGE_BASELINE= wave-BIRTHYR-BIRTHMO/12,     #age at the time of exposure -> should use wave and AD! this is looking at age at most recent visit 
         TIME_EXP_COG= .end- firstC)                #time from first cognition visit till last cognition assessment 
                                                    


#-------------------------------PART 7
#exclude those without complete baseline cognition
one_hrs <- one_hrs[!is.na(one_hrs$firstC),] #22018




#-------------------------------PART 8
#drop those who are cognitively impaired at time of ELE assessment 
one_hrs<- one_hrs[one_hrs$normal_ele!= 2 ,] #18545
one_hrs<- one_hrs[one_hrs$normal_ele!= 3 ,] #17698




#exclude people with less than 2 cognition measures 
one_hrs <- one_hrs %>% 
  mutate(visit_numb= rowSums(is.na(cbind(AD08,AD10,AD12,AD14,AD16, AD18)))) #how many rows of cog data there are: UPATE include 16&18

one_hrs <- one_hrs[one_hrs$visit_numb != "5", ] #16509




#-------------------------------------------------VARIABLE CREATION/MAINTENANCE-----------------------------------------------#



#-------------------------------PART 9

#drop ICON category from variables 
y_n <- c("B088","B109","B100","B020", "B101", "B102", "B111","B113","B116","B118","B123","B119","B112","B117","B104","B122","B105","B107","B103","B108", "B115","B106",
         "B110","B124","B120", "B114","B019", "B099", "B025", "B023","B088", "B022", "B121M1M", "B121M2M", "B125M1M", "B125M2M")

for (var in y_n) {
  
  one_hrs[,var] <-  droplevels(one_hrs[,var])
}



hlp<-one_hrs
one_hrs<- hlp


#-------------------------------PART 9

#re-code gateway variables to make NA to no etc. 
#fix missingness for masked variables based on gateway variables B120 & B124

#make all NA's -> NO ---THIS WORKS BUT TAKES AWAY THE LABELS
one_hrs<- recode_na_as(one_hrs, value="NO", subset_cols = "B121M1M")
one_hrs<- recode_na_as(one_hrs, value="NO", subset_cols = "B121M2M")
one_hrs<- recode_na_as(one_hrs, value="NO", subset_cols = "B125M1M")
one_hrs<- recode_na_as(one_hrs, value="NO", subset_cols = "B125M2M")

##
table(one_hrs$B121M1M, useNA = "always")
table(one_hrs$B121M2M, useNA = "always")
table(one_hrs$B125M1M, useNA = "always")
table(one_hrs$B125M2M, useNA = "always")
###


#add labels back in for B121M1M B121M1M B121M1M B121M2M
one_hrs$B121M1M<- factor(one_hrs$B121M1M, 
                       levels= c(1,10,2,3,4,5,6,7,8, 9, "NO"),
                       labels= c("Cancers and tumors; skin conditions",          #1
                                 "Other health condition",                       #10
                                 "Musculoskeletal system and connective tissue", #2
                                 "Heart, circulatory and blood conditions",      #3
                                 "Allergies; hay fever; sinusitis; tonsillitis", #4
                                 "Endocrine, metabolic and nutritional conditions", #5
                                 "Digestive system (stomach, liver, gallbladder, kidney, bladder)", #6
                                 "Neurological and sensory conditions",          #7
                                 "Reproductive system and prostate conditions",  #8
                                 "Emotional and psychological conditions",       #9                      
                                 "NO"))

one_hrs$B125M1M<- factor(one_hrs$B125M1M, 
                         levels= c(1,10,2,3,4,5,6,7,8, 9, "NO"),
                         labels= c("Cancers and tumors; skin conditions",          #1
                                   "Other health condition",                       #10
                                   "Musculoskeletal system and connective tissue", #2
                                   "Heart, circulatory and blood conditions",      #3
                                   "Allergies; hay fever; sinusitis; tonsillitis", #4
                                   "Endocrine, metabolic and nutritional conditions", #5
                                   "Digestive system (stomach, liver, gallbladder, kidney, bladder)", #6
                                   "Neurological and sensory conditions",          #7
                                   "Reproductive system and prostate conditions",  #8
                                   "Emotional and psychological conditions",       #9                      
                                   "NO"))

one_hrs$B125M2M<- factor(one_hrs$B125M2M, 
                         levels= c(1,10,2,3,4,5,6,7,8, 9, "NO"),
                         labels= c("Cancers and tumors; skin conditions",          #1
                                   "Other health condition",                       #10
                                   "Musculoskeletal system and connective tissue", #2
                                   "Heart, circulatory and blood conditions",      #3
                                   "Allergies; hay fever; sinusitis; tonsillitis", #4
                                   "Endocrine, metabolic and nutritional conditions", #5
                                   "Digestive system (stomach, liver, gallbladder, kidney, bladder)", #6
                                   "Neurological and sensory conditions",          #7
                                   "Reproductive system and prostate conditions",  #8
                                   "Emotional and psychological conditions",       #9                      
                                   "NO"))



#add labels back in for B121M2M 
#(done sep due to different number of categories & want to ensure they are labeled correctly) 

  
  one_hrs$B121M2M<- factor(one_hrs$B121M2M, 
                         levels= c(1,10,2,3,4,5,6,7,8, "NO"),
                         labels= c("Cancers and tumors; skin conditions",  
                                   "Other health condition",
                                   "Musculoskeletal system and connective tissue", #2
                                   "Heart, circulatory and blood conditions",      #3
                                   "Allergies; hay fever; sinusitis; tonsillitis", #4
                                   "Endocrine, metabolic and nutritional conditions", #5
                                   "Digestive system (stomach, liver, gallbladder, kidney, bladder)", #6
                                   "Neurological and sensory conditions",          #7
                                   "Reproductive system and prostate conditions",  #8      #9                      
                                   "NO"))


#-------------------------------PART 11

#assign missing for masked variables based on gateway vars
one_hrs$B121M1M[is.na(one_hrs$B120)] <- NA
one_hrs$B121M2M[is.na(one_hrs$B120)] <- NA
one_hrs$B125M1M[is.na(one_hrs$B124)] <- NA
one_hrs$B125M2M[is.na(one_hrs$B124)] <- NA


#-------------------------------PART 10

one_hrs <- one_hrs %>% 
  mutate(CANCER=(case_when(B121M1M == "Cancers and tumors; skin conditions" | 
                           B121M2M == "Cancers and tumors; skin conditions"|
                           B125M1M == "Cancers and tumors; skin conditions" | 
                           B125M2M == "Cancers and tumors; skin conditions"  ~ "YES",   
                           is.na(B120) |is.na(B124) ~ NA,
                           TRUE ~ "NO")))


one_hrs <- one_hrs %>% 
  mutate(MUSCUL=(case_when(B121M1M == "Musculoskeletal system and connective tissue" |
                           B121M2M == "Musculoskeletal system and connective tissue"|
                           B125M1M == "Musculoskeletal system and connective tissue" | 
                           B125M2M == "Musculoskeletal system and connective tissue"  ~ "YES",   
                           is.na(B120) |is.na(B124) ~ NA,
                           TRUE ~ "NO")))


table(one_hrs$MUSCUL, useNA = "always")


one_hrs <- one_hrs %>% 
  mutate(METABOLIC=(case_when(B121M1M == "Endocrine, metabolic and nutritional conditions" |
                              B121M2M == "Endocrine, metabolic and nutritional conditions"|
                              B125M1M == "Endocrine, metabolic and nutritional conditions" | 
                              B125M2M == "Endocrine, metabolic and nutritional conditions"  ~ "YES",   
                           is.na(B120) |is.na(B124) ~ NA,
                           TRUE ~ "NO")))



table(one_hrs$METABOLIC, useNA = "always")


one_hrs <- one_hrs %>% 
  mutate(DIGESTIVE=(case_when(B121M1M == "Digestive system (stomach, liver, gallbladder, kidney, bladder)" | 
                              B121M2M == "Digestive system (stomach, liver, gallbladder, kidney, bladder)"|
                              B125M1M == "Digestive system (stomach, liver, gallbladder, kidney, bladder)" | 
                              B125M2M == "Digestive system (stomach, liver, gallbladder, kidney, bladder)"  ~ "YES",   
                           is.na(B120) |is.na(B124) ~ NA,
                           TRUE ~ "NO")))




one_hrs <- one_hrs %>% 
  mutate(NEUROL=(case_when(B121M1M == "Neurological and sensory conditions" | 
                           B121M2M == "Neurological and sensory conditions"|
                           B125M1M == "Neurological and sensory conditions" | 
                           B125M2M == "Neurological and sensory conditions"  ~ "YES",   
                           is.na(B120) |is.na(B124) ~ NA,
                           TRUE ~ "NO")))




one_hrs <- one_hrs %>% 
  mutate(REPROD=(case_when(B121M1M == "Reproductive system and prostate conditions" | 
                           B121M2M == "Reproductive system and prostate conditions"|
                           B125M1M == "Reproductive system and prostate conditions" | 
                           B125M2M == "Reproductive system and prostate conditions"  ~ "YES",   
                           is.na(B120) |is.na(B124) ~ NA,
                           TRUE ~ "NO")))



one_hrs <- one_hrs %>% 
  mutate(OTHER=(case_when(B121M1M == "Other health condition" | 
                          B121M2M == "Other health condition"|
                          B125M1M == "Other health condition" | 
                          B125M2M == "Other health condition"  ~ "YES",   
                           is.na(B120) |is.na(B124) ~ NA,
                           TRUE ~ "NO")))

one_hrs <- one_hrs %>% 
  mutate(ALLERG=(case_when(B109=="YES"| B121M1M == "Allergies; hay fever; sinusitis; tonsillitis" | 
                           B121M2M == "Allergies; hay fever; sinusitis; tonsillitis"|
                           B125M1M == "Allergies; hay fever; sinusitis; tonsillitis" | 
                           B125M2M == "Allergies; hay fever; sinusitis; tonsillitis"  ~ "YES",   
                           is.na(B120) |is.na(B124) ~ NA,
                           TRUE ~ "NO")))

one_hrs <- one_hrs %>% 
  mutate(HEART=(case_when(B110=="YES"|
                            B121M1M=="Heart, circulatory and blood conditions"|
                            B121M2M=="Heart, circulatory and blood conditions"|
                            B125M1M=="Heart, circulatory and blood conditions"|
                            B125M2M=="Heart, circulatory and blood conditions"  ~ "YES",   
                           is.na(B120) |is.na(B124) ~ NA,
                           TRUE ~ "NO")))



one_hrs <- one_hrs %>% 
  mutate(PSYCHO=(case_when(B118=="YES"|
                             B121M1M=="Emotional and psychological conditions"|
                             B121M2M=="Emotional and psychological conditions"|
                             B125M1M=="Emotional and psychological conditions"|
                             B125M2M=="Emotional and psychological conditions"  ~ "YES",   
                           is.na(B120) |is.na(B124) ~ NA,
                           TRUE ~ "NO")))













#-------------------------------PART 13

#create combination variables for variables with rare exposure (<1% of data) 
#drug, alcohol or smoking use: smk + drg/alc
one_hrs<- one_hrs %>% 
  mutate(drugAlcUse=(case_when(B117=="YES"|B122=="YES"~ "YES",   
                          is.na(B117) |is.na(B122) ~ NA,
                          TRUE ~ "NO")))




#childhood any other condition: metabolic, cancer, reprod, diabetes
one_hrs<- one_hrs %>% 
  mutate(other_c_cond_2= (case_when(METABOLIC == "YES" | B106 == "YES"|CANCER == "YES"|
                                   REPROD == "YES"| B124=="YES"| OTHER=="YES" ~"YES",   
                                   is.na(B120) |is.na(B124) ~ NA,
                                   TRUE ~ "NO")))



#Heart conditions: high bp + HEART  
one_hrs<- one_hrs %>% 
  mutate(HEART_2.0= (case_when(B115 == "YES" | HEART =="YES" ~"YES",   
                               is.na(B115) |is.na(HEART) ~ NA,
                               TRUE ~ "NO")))



#Neurological conditions: Epilepsy/seizures + NEUROL 
one_hrs<- one_hrs %>% 
  mutate(NEURO_2.0= (case_when( B112=="YES"| NEUROL=="YES" ~"YES",   
                                is.na(B112) |is.na(NEUROL) ~ NA,
                                TRUE ~ "NO")))






#-------------------------------CLEAN UP SOME LOOSE ENDS---------------------------------------------------#


#-------------------------------PART 14
#combine levels of 'lived in rural area'
one_hrs$childrural<-car::recode(one_hrs$childrural, "1='YES'; 5='NO'; c(7,8,9)= NA")

#combine levels of 'Father unemployed during childhood'
one_hrs$B023<-car::recode(one_hrs$B023, "'Never lived with Father/ Father was not alive'='Never lived with Father/ Father was not alive';c('Father never worked/ Always disabled','NO')= 'NO'; 'YES'='YES'")





#-------------------------------PART 15



#renew exposures list
new_expo_lab= c(#Infectious
  "Measles before age 16",
  "Mumps before age 16",
  "Chicken pox before age 16",
  "Allergies; hay fever; sinusitis; tonsillitis",
  
  #Neurological
  "Ear problems before age 16",
  "Headaches or migraines before age 16",
  "Depression before age 16",
  "Emotional and psychological conditions",
  "Neurological and sensory conditions2",
  "Childhood- Learning problems",
  "Childhood- Concussion or severe head injury",
  
  #Respiratory
  "Parents or guardians smoke",
  "Asthma before age 16",
  "Respiratory disorder before age 16",
  
  #behavioral/substance use
  "Drugs, alchol, or smoking use before age 16",
  
  #Sensory
  "Speech impairment before age 16",
  "Difficulty seeing before age 16",
  
  #Other
  "Heart, circulatory and blood conditions2",
  "Childhood health conditions",
  "Childhood- Disability",
  "Stomach problems before age 16",
  "Rate health as child",
  "Digestive system (stomach, liver, gallbladder, kidney, bladder)",
  "Musculoskeletal system and connective tissue",
  
  #child home environment
  "Lived rural area during school",
  "Childhood- Missed school",
  "Live with grandparents during childhood", 
  "Father unemployed during childhood",
  "Mother work during childhood",
  "Family get financial help in childhood",
  "Rate family financial situation- SES"
)    

new_expos= c(#Infectious
  "B100",
  "B101",
  "B102",
  "ALLERG",
  
  
  #Neurological
  "B111",
  "B113",
  "B116",
  "PSYCHO",
  "NEURO_2.0",
  "B123",
  "B119",
  
  #Respiratory
  "B104",
  "B105",
  "B107",
  
  #behavioral/substance use
  "drugAlcUse",
  
  #Sensory
  "B108",
  "B103",
  
  #Other
  "HEART_2.0",
  "other_c_cond_2",
  "B120",
  "B114",
  "B019",
  "DIGESTIVE",
  "MUSCUL",
  
  
  #child home environment
  "childrural",
  "B099",
  "B025",
  "B023",
  "B088",
  "B022",
  "B020"
)



#-------------------------------PART 16

#create binary CIND & dementia vars for regression 
#cognition at follow-up (dementia or normal), (CIND or normal)
one_hrs <- one_hrs %>%
  mutate(
    demen_vs_norm_1 = ifelse(EVER_DEMENTIA_1==2, NA, EVER_DEMENTIA_1),  #most recent dem status
    cind_vs_norm_1  = ifelse(EVER_DEMENTIA_1==3, NA, EVER_DEMENTIA_1),  #most recent CIND status
    demen_vs_norm_1 = factor(demen_vs_norm_1, levels=c(3,1), labels=c("Dementia","Normal")),
    cind_vs_norm_1  = factor(cind_vs_norm_1,  levels=c(2,1), labels=c("CIND","Normal")),
    demen_vs_norm_1 = relevel(demen_vs_norm_1, ref="Normal"),
    cind_vs_norm_1  = relevel(cind_vs_norm_1,  ref="Normal"),
  )

#-------------------------------PART 17

#create binary cognition (CIND & dementia) vs normal 
#cognition at follow-up (23=dementia and or CIND, 1=normal)
#possible sensitivity analysis 
one_hrs <- one_hrs %>%
  mutate(
    imp_vs_norm = ifelse(demen_vs_norm_1=="Dementia" | cind_vs_norm_1=="CIND", 23, 1),  #most recent dem status
    imp_vs_norm = factor(imp_vs_norm, levels=c(1,23), labels=c("Normal", "Cognitive imparment")),
    imp_vs_norm = relevel(imp_vs_norm, ref="Normal"),
  )



#-------------------------------PART 17#-------------NITA DATASET------------------PART 17#-------------------------------PART 17

#filer data
df<- one_hrs %>%
  select(1:10, "AGE_BASELINE", "wave", "mother_wrk", "fam_finance", starts_with("B"), 225:234)



#load herongs
load(paste0(source_path, "DNAm_age_2016.rda"))

#convert to numeric
w13_clocks$HHID<-as.numeric(w13_clocks$HHID)  
w13_clocks$PN<-as.numeric(w13_clocks$PN) 
w13_clocks$wave <- "2016"

#full merge
ele_clocks<- full_join(w13_clocks, df, by= c("HHID", "PN"))

#make indicator
ele_clocks<- ele_clocks %>%
  mutate(indicator= if_else(complete.cases(.) , "complete", "missing"))

#save
saveRDS(ele_clocks, "G:/.shortcut-targets-by-id/1dGRzB0bSRxxSqHT9WaXQ13_WuVFFZaE7/earlylife/source/ele_clocks_01192024.RData")
#-------------------------------PART 17#-------------------------------PART 17#-------------------------------PART 17
