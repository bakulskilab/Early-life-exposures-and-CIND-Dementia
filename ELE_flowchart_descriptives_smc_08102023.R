#===========================================================================

# File: ELE data prep 
# Author: scarlet cockell
# Update Date: 8/10/2023

#===========================================================================

#===========================================================================
# Purpose -code for the creation of:
#  inclusion descriptive table
#  Bar chart for missingness   
#  Flowchart
#  
#  Inclusion/exclusion descriptive table
#===========================================================================

#path of folders
script_path= "G:/.shortcut-targets-by-id/1dGRzB0bSRxxSqHT9WaXQ13_WuVFFZaE7/earlylife/script/"
source_path= "G:/.shortcut-targets-by-id/1dGRzB0bSRxxSqHT9WaXQ13_WuVFFZaE7/earlylife/source/"
output_path= "G:/My Drive/Bakulski Lab/brads ile updates/code"

#libraries
library(tidyverse)
library(data.table)
library(generics)
library(plyr)
library(ggplot2)
library(visR)
library(gtsummary)
library(haven)
library(dplyr)
library(mde)
library(vcd)
library(gt)


#Load data  
one_hrs<-readRDS(paste0(source_path, "ELE_22081_237_smc.RData")) #for incl vs. exlud table
one_hrs<-readRDS(paste0(source_path, "ELE_16509_241_smc.RData"))

#load functions 
source(paste0(script_path, "functions.R")) 

#------------------------------------------------------------------------------------------------#
#-------------------------------GEOM TILE TEST 
df_dem<-select(reg_adjusted_result_table, 1:3 )
cind<-select(reg_adjusted_result_table, 6:8)


ggplot(df_dem, aes(x = x, y =`OR`, fill =  `P-value`)) +
  geom_tile() + 
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

###########
heaT <- ggplot(df_dem, aes(x = x, y =y)) + 
  geom_tile(aes(fill=`P-value`)) +
  scale_fill_distiller(palette = "YlGn", direction=2) +

  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") +
  ylab("")


####  geom_text(aes(label = round(Similarity, 2))) +
 df_dem$y <- rep(paste0("row", 2:10), 5)
 df_dem$x <- rep(paste0("Col", 1:9), 5)


dem_p<-select(reg_adjusted_result_table, 1,3 )
cind_p<-select(reg_adjusted_result_table, 6,8)

dem_fdr<-select(reg_adjusted_result_table, 1,4 )
cind_fdr<-select(reg_adjusted_result_table, 6,9)

#------------------------------------------------------------------------------------------------#

#-------------------------------PART 1 INCLUDED
#----get proxy status for descriptive table
#load data 
df<-readRDS(paste0(source_path, "proxy_variables_smc_9112023.RData")) 

#convert to numeric 
df$HHID<-as.numeric(df$HHID) 
df$PN<-as.numeric(df$PN) 

#re-code proxy variables to match RAND binary format
df$proxy2008 <- as.factor(df$proxy2008)
df$proxy2010 <- as.factor(df$proxy2010)
df$proxy2012 <- as.factor(df$proxy2012)
df$proxy2014 <- as.factor(df$proxy2014)
df$proxy2016 <- as.factor(df$proxy2016)
df$proxy2018 <- as.factor(df$proxy2018)

#recode based on RAND format 
df$proxy2008=car::recode(df$proxy2008,"c(1,2)='1';5='0'") #0- no proxy; 1- yes proxy 
df$proxy2010=car::recode(df$proxy2010,"c(1,2)= '1'; 5='0'")
df$proxy2012=car::recode(df$proxy2012,"c(1,2)= '1'; 5='0'")
df$proxy2014=car::recode(df$proxy2014,"c(1,2)= '1'; 5='0'")
df$proxy2016=car::recode(df$proxy2016,"c(1,2)= '1'; 5='0'")
df$proxy2018=car::recode(df$proxy2018,"c(1,2)= '1'; 5='0'")

#merge with cleaned dataset 
#returns all matching rows from df
one_hrs= one_hrs %>% left_join(df,by=c("HHID","PN")) #16254, 236; after merge- 16254, 242

##proxy status at baseline -for desc stats        
#no individuals in analytic sample with first cog in 16/18
one_hrs<- one_hrs %>%  
  mutate(proxy_baseline= ifelse2(firstC=="2008" & proxy2008==1, "proxy",
                         ifelse2(firstC=="2010"& proxy2010==1, "proxy",
                         ifelse2(firstC=="2012"& proxy2012==1, "proxy",
                         ifelse2(firstC=="2014"& proxy2014==1, "proxy", 
                                                         "no proxy" )))))



##proxy status at follow up - for desc stats 
one_hrs<- one_hrs %>%  
  mutate(proxy_followup= ifelse2(proxy2010==1 &.end=="2010", "proxy",
                         ifelse2(proxy2012==1 &.end=="2012", "proxy",
                         ifelse2(proxy2014==1 &.end=="2014", "proxy",
                         ifelse2(proxy2016==1 &.end=="2016", "proxy",
                         ifelse2(proxy2018==1 &.end=="2018", "proxy",
                                                                      "no proxy"))))))


#inclusion descriptive table
#select the variables in order of categories for nice table 
m<-one_hrs[new_expos]
m <- select(one_hrs,TIME_EXP_COG, AGE_BASELINE,  EVER_DEMENTIA_1, SCHLYRS, proxy_baseline, proxy_followup, #in years 
            #demos
            SEX, RACE_HISP,MAX_PARENTAL_EDU,
            #infectious
            ALLERG,B100, B101,B102, 
            #neurological
            B111,B113,B116,PSYCHO,NEURO_2.0,B123,B119,B112,
            #respiratory
            B104,B105,B107, 
            #behavioral/substance use
            drugAlcUse,
            #sensory
            B103,B108, 
            #other physical health
            HEART_2.0,other_c_cond_2,
            B120, B114,B019,DIGESTIVE,MUSCUL, CANCER, 
            #childhood home environment
            childrural,B099, fam_finance,B025, B023,mother_wrk, B022)



#MAKE DISCRIPTIVE TABLE
m %>%
  tbl_summary(by = EVER_DEMENTIA_1,
              label = list(TIME_EXP_COG = "Time from baseline to end of follow up (years) ",
                           AGE_BASELINE = "Age at baseline",
                           proxy_baseline= "Proxy status at baseline" , 
                           proxy_followup= "Proxy status at follow-up",
                           EVER_DEMENTIA_1="Cognition Status at Last Follow up",
                           SCHLYRS = "Personal educational attainment (years)", 
                           #demos
                           SEX = "Sex",
                           RACE_HISP = "Race/ethnicity",
                           MAX_PARENTAL_EDU = "Parental highest degree of education",
                           #infectious
                           ALLERG= "Allergies; hay fever; sinusitis; tonsillitis",
                           B100 = "Measles before age 16", 
                           B101 = "Mumps before age 16",
                           B102 = "Chicken pox before age 16", 
                           #neurological
                           B111 = "Ear problems before age 16",
                           B113 = "Headaches or migraines before age 16",
                           B116 = "Depression before age 16",
                           PSYCHO = "Emotional and psychological conditions",
                           NEURO_2.0= "Neurological and sensory conditions2",
                           B123 = "Childhood-learning problems",
                           B119 = "Concussion or severe head injury in childhood",
                           B112 = "epilepsy or seizures before age 16",
                           #respiratory
                           B104 = "Parents/guardians smoke",
                           B122 = "Childhood- smoking",
                           B105 = "Asthma before age 16",
                           B107 = "Respiratory disorder before age 16",
                           #behav/substance use
                           drugAlcUse = "Drugs, alchol, or smoking use before age 16",
                           #sensory
                           B103 = "Difficulty seeing before age 16",
                           B108 = "Speech impairment before age 16", 
                           #other physical health
                           HEART_2.0="Heart, circulatory and blood conditions2",
                           other_c_cond_2="Childhood health conditions",
                           B120 = "Childhood- disability", 
                           B114 = "Stomach problems before age 16",
                           B019 = "Rate health as child",
                           DIGESTIVE = "Digestive system (stomach, liver, gallbladder, kidney, bladder)",
                           MUSCUL = "Musculoskeletal system and connective tissue",
                           #childhood home environment
                           B099 = "Missed school as a child", 
                           childrural = "Lived in rural area as a child",
                           fam_finance = "Rate family financial situation",
                           B025 = "Live with grandparents during childhood", 
                           B023 = "Father unemployed during childhood",
                           mother_wrk = "Mother work during childhood", 
                           B022 = "Family get financial help in childhood"),
              type = all_dichotomous() ~ "categorical",
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)")) %>%
  add_overall() %>%
  add_n() %>%
  add_p(
    test = everything() ~ "fisher.test",
    test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)) %>%
  modify_header(label ~ "**Characteristic**") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Cognitive Status at follow up: Normal, CIND, Dementia**") %>%
  modify_footnote(
    all_stat_cols() ~ "Mean(SD) or Count(%)"
  ) %>%
  modify_caption("**Table 1. Descriptive Statistics HRS Sample**") %>%
  bold_labels() #%>%
#as_gt() %>% 
#gt::gtsave(filename = "included_only.docx") 

#TRYING GT TABLE
#total releases only 
gt_tbl2 <- 
  gt(m) %>%
  tab_header (
    title = "Table 2. Descriptive Statistics HRS Sample"
  ) %>%
  tab_spanner( label = "Cognitive Status at follow up: Normal, CIND, Dementia",
               columns = c('Releases (lb)_Lead', 'Air Releases (lb)_Lead', 'Water Releases (lb)_Lead', 'Land Releases (lb)_Lead', 'Off-Site Releases (lb)_Lead', 'Waste Managed (lb)_Lead')
               
  ) %>%
  tab_spanner( label = "Cadmium Releases (lb)",
               columns = c('Releases (lb)_Cadmium','Air Releases (lb)_Cadmium', 'Water Releases (lb)_Cadmium', 'Land Releases (lb)_Cadmium', 'Off-Site Releases (lb)_Cadmium', 'Waste Managed (lb)_Cadmium')
               
  )%>%
  tab_spanner( label = "Mercury Releases (lb)",
               columns = c('Releases (lb)_Mercury','Air Releases (lb)_Mercury', 'Water Releases (lb)_Mercury', 'Land Releases (lb)_Mercury', 'Off-Site Releases (lb)_Mercury', 'Waste Managed (lb)_Mercury')
  )%>%
  cols_label('Releases (lb)_Lead' = "Total",
             'Releases (lb)_Cadmium' = "Total",
             'Releases (lb)_Mercury' = "Total",
             'Air Releases (lb)_Lead' = "Air",
             'Air Releases (lb)_Cadmium'= "Air",
             'Air Releases (lb)_Mercury'= "Air",
             'Water Releases (lb)_Lead' = "Water",
             'Water Releases (lb)_Cadmium'= "Water",
             'Water Releases (lb)_Mercury'= "Water",
             'Land Releases (lb)_Lead' = "Land",
             'Land Releases (lb)_Cadmium'= "Land",
             'Land Releases (lb)_Mercury'= "Land",
             'Off-Site Releases (lb)_Lead' = "Off-Site",
             'Off-Site Releases (lb)_Cadmium'= "Off-Site",
             'Off-Site Releases (lb)_Mercury'= "Off-Site",
             'Waste Managed (lb)_Lead' = "Waste Managed",
             'Waste Managed (lb)_Cadmium'= "Waste Managed",
             'Waste Managed (lb)_Mercury'= "Waste Managed",
  )%>%
  tab_style(
    style = cell_borders(
      sides= c("left"),
      color= "black",
      weight = px(2.0),
      style= "solid"
    ),
    locations = cells_body(columns= c('Releases (lb)_Cadmium','Releases (lb)_Lead','Releases (lb)_Mercury' ),
                           #'Air Releases (lb)_Lead', 'Water Releases (lb)_Lead', 'Land Releases (lb)_Lead', 'Off-Site Releases (lb)_Lead', 'Waste Managed (lb)_Lead' c('Releases (lb)_Cadmium', 'Air Releases (lb)_Cadmium', 'Water Releases (lb)_Cadmium', 'Land Releases (lb)_Cadmium', 'Off-Site Releases (lb)_Cadmium', 'Waste Managed (lb)_Lead'),
                           # c('Releases (lb)_Mercury', 'Air Releases (lb)_Mercury', 'Water Releases (lb)_Mercury', 'Land Releases (lb)_Mercury', 'Off-Site Releases (lb)_Mercury', 'Waste Managed (lb)_Lead'))
    )) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_style( style =  cell_text( weight = "bold", size = "large"),
             locations = cells_title("title"))%>%
  tab_style( style =  cell_text( weight = "bold", size = "small"),
             locations = cells_column_labels(columns = everything()))%>%
  tab_style( style =  cell_text( weight = "bold"),
             locations = cells_column_spanners()) %>%
  tab_style( style = cell_text(size = "small"),
             locations= cells_body(columns=everything())) %>%
  
  gt_tbl2



#------------------------------------------------------------------------------------------------#
#-------------------------------PART 2 EXCLUDED
#Included versus excluded descriptive statistics tables #included = 16,254, excluded = 5,435

#dummy variable for included and excluded
one_hrs<- one_hrs %>% 
  mutate(decision_time= ifelse2(normal_ele=="1" & visit_numb < "5", "Included", "Excluded"))

#baseline cognition #somethings not right here 
one_hrs<- one_hrs %>% 
  mutate(baseline_cog= ifelse2(normal_ele=="cog_imparment" & AD08 != 1, AD08,
                               ifelse2(normal_ele=="cog_imparment" & AD10 != 1, AD10,
                               ifelse2(normal_ele=="cog_imparment" & AD12 != 1, AD12,
                               ifelse2(normal_ele=="cog_imparment" & AD14 != 1, AD14,
                                                                     normal_ele))))) #NEED TO FIGURE OUT IF THE CODE MISSED MORE THAN ONE PERSON WHO IS COG NORMAL AND MARKED AS IMPARED 


#cognition at last follow up- ever_dementia_ 
#recode levels of baseline_cog 
y_n <- c("baseline_cog")
for (var in y_n) {}
  one_hrs$baseline_cog<-car::recode(one_hrs$baseline_cog,"2='CIND';3='Dementia';c('cog_imparment','yes10','yes12','yes14','yes8')= 'Normal Cognition'")
  one_hrs$baseline_cog<-car::recode(one_hrs$baseline_cog,"2='CIND';3='Dementia';1= 'Normal Cognition'")
  

#recode levels of most recent cognition 
y_n <- c("EVER_DEMENTIA_1")
for (var in y_n) {
  one_hrs$EVER_DEMENTIA_1<-car::recode(one_hrs$EVER_DEMENTIA_1,"1='Normal Cognition'; 2='CIND';3='Dementia'")
}

#add in baseline cog, remove- allergies b16, heart, phsyco
#select the variables in order of categories for nice table 
m <- select(one_hrs,decision_time,proxy_baseline, proxy_followup, baseline_cog, EVER_DEMENTIA_1, TIME_EXP_COG, AGE_BASELINE,SCHLYRS, #in years 
            #demos
            SEX, RACE_HISP,MAX_PARENTAL_EDU,
            #infectious
            ALLERG,B100, B101,B102, 
            #neurological
            B111,B113,B116,PSYCHO,NEURO_2.0,B123,B119,
            #respiratory
            B104,B105,B107,
            #behavioral/substance use
            drugAlcUse,
            #sensory
            B103,B108, 
            #other physical health
            HEART_2.0,other_c_cond_2,
            B120, B114,B019,DIGESTIVE,MUSCUL,
            #childhood home environment
            childrural,B099,fam_finance,B025, B023,mother_wrk, B022)


#MAKE DISCRIPTIVE TABLE
m %>%
  tbl_summary(by = decision_time,
              label = list(baseline_cog="Cognition Status at baseline", 
                           proxy_baseline= "Proxy status at baseline" , 
                           proxy_followup= "Proxy status at follow-up",
                           EVER_DEMENTIA_1="Cognition Status at Last Follow up",
                           TIME_EXP_COG = "Time from baseline to end of follow up (years) ",
                           AGE_BASELINE = "Age at baseline",
                           SCHLYRS = "Personal educational attainment (years)", 
                           #demos
                           SEX = "Sex",
                           RACE_HISP = "Race/ethnicity",
                           MAX_PARENTAL_EDU = "Parental highest degree of education",
                           #infectious
                           ALLRG= "Allergies; hay fever; sinusitis; tonsillitis",
                           B100 = "Measles before age 16", 
                           B101 = "Mumps before age 16",
                           B102 = "Chicken pox before age 16", 
                           #neurological
                           B111 = "Ear problems before age 16",
                           B113 = "Headaches or migraines before age 16",
                           B116 = "Depression before age 16",
                           PSYCHO = "Emotional and psychological conditions",
                           NEURO_2.0= "Neurological and sensory conditions2",
                           B123 = "Childhood-learning problems",
                           B119 = "Concussion or severe head injury in childhood",
                           #respiratory
                           B104 = "Parents/guardians smoke",
                           B122 = "Childhood- smoking",
                           B105 = "Asthma before age 16",
                           B107 = "Respiratory disorder before age 16",
                           #behav/substance use
                           drugAlcUse = "Drugs, alchol, or smoking use before age 16",
                           #sensory
                           B103 = "Difficulty seeing before age 16",
                           B108 = "Speech impairment before age 16", 
                           #other physical health
                           HEART_2.0="Heart, circulatory and blood conditions2",
                           other_c_cond_2="Childhood health conditions",
                           B120 = "Childhood- disability", 
                           B114 = "Stomach problems before age 16",
                           B019 = "Rate health as child",
                           DIGESTIVE = "Digestive system (stomach, liver, gallbladder, kidney, bladder)",
                           MUSCUL = "Musculoskeletal system and connective tissue",
                           #childhood home environment
                           B099 = "Missed school as a child", 
                           childrural = "Lived in rural area as a child",
                           fam_finance = "Rate family financial situation",
                           B025 = "Live with grandparents during childhood", 
                           B023 = "Father unemployed during childhood",
                           mother_wrk = "Mother work during childhood", 
                           B022 = "Family get financial help in childhood"),
              type = all_dichotomous() ~ "categorical",
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"), 
              missing = "always",
              missing_text = "(Missing)") %>%
  add_overall() %>%
  add_n() %>%
  add_p(
    test = everything() ~ "fisher.test",
    test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)) %>%
  modify_header(label ~ "**Characteristic**") %>%
  modify_footnote(
    all_stat_cols() ~ "Mean(SD) or Count(%)"
  ) %>%
  modify_caption("**Table 1.Health and Retirement Study (HRS) Early Life Exposures Included vs Excluded Sample**") %>%
  bold_labels() #%>%
 # as_gt() %>%
#gt::gtsave(filename = "incl_excl.docx")



#make labels for cognition variable
# 1-YES/NO 
y_n <- c("EVER_DEMENTIA_2")

for (var in y_n) {
  
  one_hrs[,var] <- factor(one_hrs[,var], levels=c(1,2,3), labels=c("Normal Cognition","CIND","Dementia"))
}

#-------------------------------PART 3
#fishers test for just exposures by dementia status at follow up 

#select the variables in order of categories for nice table 
m <- select(one_hrs, EVER_DEMENTIA_1, 
            #infectious
            ALLERG,B100, B101,B102, 
            #neurological
            B111,B113,B116,PSYCHO,B123,B119,B112,B117,
            #respiratory
            B104,B122,B105,B107,
            #sensory
            B103,B108, 
            #other physical health
            B115,METABOLIC,B106,
            CANCER,HEART,B124,OTHER,B120, B114,B019,DIGESTIVE,REPROD,MUSCUL,
            #childhood home environment
            B099, childrural, B020,B025, B023,B088, B022)


#MAKE DISCRIPTIVE TABLE 
#demen_vs_norm_1,  cind_vs_norm_1, EVER_DEMENTIA_1
m %>%
  tbl_summary(by = EVER_DEMENTIA_1,
              label = list(EVER_DEMENTIA_1="Cognition Status at Last Follow up",
                           #infectious
                           ALLERG= "Allergies; hay fever; sinusitis; tonsillitis",
                           B100 = "Measles before age 16", 
                           B101 = "Mumps before age 16",
                           B102 = "Chicken pox before age 16", 
                           #neurological
                           B111 = "Ear problems before age 16",
                           B113 = "Headaches or migraines before age 16",
                           B116 = "Depression before age 16",
                           PSYCHO = "Emotional and psychological conditions",
                           B123 = "Childhood-learning problems",
                           B119 = "Concussion or severe head injury in childhood",
                           B112 = "Epilepsy or seizures before age 16",
                           B117 = "Drugs/alcohol problems before age 16",
                           #respiratory
                           B104 = "Parents/guardians smoke",
                           B122 = "Childhood- smoking",
                           B105 = "Asthma before age 16",
                           B107 = "Respiratory disorder before age 16",
                           #sensory
                           B103 = "Difficulty seeing before age 16",
                           B108 = "Speech impairment before age 16", 
                           #other physical health
                           B115 = "High bp before age 16",
                           METABOLIC= "Endocrine, metabolic and nutritional conditions",
                           B106 = "Diabetes before age 16",
                           CANCER = "Cancers and tumors; skin conditions",
                           HEART= "Heart, circulatory and blood conditions",
                           B124 = "Childhood-any other conditions",
                           OTHER = "Other health condition",
                           B120 = "Childhood- disability", 
                           B114 = "Stomach problems before age 16",
                           B019 = "Rate health as child",
                           DIGESTIVE = "Digestive system (stomach, liver, gallbladder, kidney, bladder)",
                           REPROD = "Reproductive system and prostate conditions",
                           MUSCUL = "Musculoskeletal system and connective tissue",
                           #childhood home environment
                           B099 = "Missed school as a child", 
                           childrural = "Lived in rural area as a child",
                           B020 = "Rate family financial situation",
                           B025 = "Live with grandparents during childhood", 
                           B023 = "Father unemployed during childhood",
                           B088 = "Mother work during childhood", 
                           B022 = "Family get financial help in childhood"),
              type = all_dichotomous() ~ "categorical",
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)")) %>%
  add_overall() %>%
  add_n() %>%
  add_p( 
    test= everything() ~ "fisher.test") %>%
  modify_header(label ~ "**Characteristic**") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Cognitive Status: 1-normal, 2-CIND, 3-Dementia**") %>%
  modify_footnote(
    all_stat_cols() ~ "Mean(SD) or Count(%)"
  ) %>%
  modify_caption("**Table 1. Descriptive Statistics HRS Sample**") %>%
  bold_labels() #%>%
#as_gt() %>% 
#gt::gtsave(filename = "included_only.docx")





#----------------------------------- FLOW CHART--------------------------------------------------#

#-------------------------------PART 4  
data_flow <- tibble::tibble(from = c("HRS\nN = 43,233",
                                     "HRS\nN = 43,233",
                                     "Complete data on\n key covariates\nN = 34,248",
                                     "Complete data on\n key covariates\nN = 34,248",
                                     "Has at least one exposure variable\nN = 22,018",
                                     "Has at least one exposure variable\nN = 22,018",
                                     "ELE categories with sufficient cell sizes\nN = 21,880",
                                     "ELE categories with sufficient cell sizes\nN = 21,880",
                                     "Concordant Exposure\nN = 21,689",
                                     "Concordant Exposure\nN = 21,689",
                                     "Complete baseline cognition\nN = 21,689",
                                     "Complete baseline cognition\nN = 21,689", 
                                     "Cognitivly normal at baseline\nN = 17,437",
                                     "Cognitivly normal at baseline\nN = 17,437",
                                     "Baseline & follow-up\n cognition measures\nN = 16,254",
                                     "Baseline & follow-up\n cognition measures\nN = 16,254"),
                            to = c("Complete data on\n key covariates\nN = 34,248",
                                   "Missing sex,N = 23\n Missing RaceEthnicity,N = 1,108\n Missing Education,N = 187\n Missing Parental Education,N = 7,667", 
                                   "Has at least one exposure variable\nN = 22,018",
                                   "Missing All Exposure Information\nN = 12,230",
                                   "ELE categories with sufficient cell sizes\nN = 21,880",
                                   "Exclude ELE categories with small cell sizes:\n'Never lived with mother/Mother Not Alive',N = 107\n'It varied Family Finance',N = 31", 
                                   "Concordant Exposure\nN = 21,689",
                                   "Discordant Exposure\nN = 191",
                                   "Complete baseline cognition\nN = 21,689",
                                   "Missing baseline cognition\nN = 0",
                                   "Cognitivly normal at baseline\nN = 17,437",
                                   "CIND/Dementia at baseline\nN = 4,252",
                                   "Baseline & follow-up\n cognition measures\nN = 16,254",
                                   "Missing follow-up cognition\nN = 1,183",
                                   "Analytic sample\nN = 16,254",
                                   "Analytic sample\nN = 16,254"),
                            
)





######################################################################making changes FIX THE NORMAL AT BASELINE NOT NOM AT BASLINE AND BASELINE AND FOLLOW UP NUMBERS 
data_flow <- tibble::tibble(from = c("HRS\nN = 43,233",
                                     "HRS\nN = 43,233",
                                     "Complete data on\n key covariates\nN = 34,248",
                                     "Complete data on\n key covariates\nN = 34,248",
                                     "Has at least one exposure variable\nN = 22,018",
                                     "Has at least one exposure variable\nN = 22,018",
                                     "ELE categories with sufficient cell sizes\nN = 21,880",
                                     "ELE categories with sufficient cell sizes\nN = 21,880",
                                     "Concordant Exposure\nN = 21,689",
                                     "Concordant Exposure\nN = 21,689",
                                     "Complete baseline cognition\nN = 21,689",
                                     "Complete baseline cognition\nN = 21,689", 
                                     "Cognitivly normal at baseline\nN = 17,437",
                                     "Cognitivly normal at baseline\nN = 17,437",
                                     "Baseline & follow-up\n cognition measures\nN = 16,254",
                                     "Baseline & follow-up\n cognition measures\nN = 16,254"),
                            to = c("Complete data on\n key covariates\nN = 34,248",
                                   "Missing sex, N = 23\n Missing RaceEthnicity, N = 1,108\n Missing Education, N = 187\n Missing Parental Education, N = 7,667", 
                                   "Has at least one exposure variable\nN = 22,018",
                                   "Missing All Exposure Information\nN = 12,230",
                                   "ELE categories with sufficient cell sizes\nN = 21,880",
                                   "Exclude categories with small cell sizes:\n'Never lived with mother/Mother Not Alive', N = 107\n'It varied Family Finance', N = 31", 
                                   "Concordant Exposure\nN = 21,689",
                                   "Discordant Exposure\nN = 191",
                                   "Complete baseline cognition\nN = 21,689",
                                   "Missing baseline cognition\nN = 0",
                                   "Cognitivly normal at baseline\nN = 17,437",
                                   "CIND/Dementia at baseline\nN = 4,252",
                                   "Baseline & follow-up\n cognition measures\nN = 16,254",
                                   "Missing follow-up cognition\nN = 1,183",
                                   "Analytic sample\nN = 16,254",
                                   "Analytic sample\nN = 16,254"),
                            
)

#####################------NEW-------#################################################
data_flow <- tibble::tibble(from = c("HRS\nN = 43,233",
                                     "HRS\nN = 43,233",
                                     "Complete data on\n key covariates\nN = 34,248",
                                     "Complete data on\n key covariates\nN = 34,248",
                                     "Has at least one exposure variable\nN = 22,018",
                                     "Has at least one exposure variable\nN = 22,018",
                                     "Complete baseline cognition\nN = 22,018",
                                     "Complete baseline cognition\nN = 22,018", 
                                     "Cognitivly normal at baseline\nN = 17,698",
                                     "Cognitivly normal at baseline\nN = 17,698",
                                     "Baseline & follow-up\n cognition measures avalible\nN = 16,509",
                                     "Baseline & follow-up\n cognition measures avalible\nN = 16,509"),
                            to = c("Complete data on\n key covariates\nN = 34,248",
                                   "Missing Sex, N = 23\n Missing RaceEthnicity, N = 1,108\n Missing Education, N = 187\n Missing Parental Education, N = 7,667", 
                                   "Has at least one exposure variable\nN = 22,018",
                                   "Missing all exposure information\nN = 12,230",
                                   "Complete baseline cognition\nN = 22,018",
                                   "Missing baseline cognition\nN = 0",
                                   "Cognitivly normal at baseline\nN = 17,698",
                                   "CIND/Dementia at baseline\nN = 4,320",
                                   "Baseline & follow-up\n cognition measures avalible\nN = 16,509",
                                   "Missing follow-up cognition\nN = 1,189",
                                   "Analytic sample\nN = 16,509",
                                   "Analytic sample\nN = 16,509"),
                            
)

data_flow ## looking at tibble
##building flowchart
flowchart <- ggflowchart(data_flow,
                         arrow_size = 0.1,
                         x_nudge = 0.29, ## width of box
                         y_nudge = 0.39, #height
                         horizontal = FALSE,
                         text_size = 2.9) 

flowchart #1300 x 700
####################################################################
#node data
node_data <- tibble::tibble(
  name = c("HRS",
           "Complete sex",
           "Missing sex", 
           "Complete RaceEthnicity",
           "Missing RaceEthnicity",
           "Complete Education",
           "Missing Education",
           "Complete Parental Education",
           "Missing Parental Education",
           "Complete Exposure",
           "Missing Exposure",
           "Complete Mother Work",
           "Never lived with mother/Mother Not Alive",
           "Complete Family Finance",
           "It varied Family Finance",
           "Concordant Exposure",
           "Discordant Exposure",
           "2 ??? cognition measures",
           "1 cognition measure",
           "Normal Cognition", 
           "CIND/Dementia"),
  type = c("Type 1", "Type 1", "Type 2", "Type 1", "Type 2", 
           "Type 1", "Type 2", "Type 1", "Type 2",
           "Type 1", "Type 2","Type 1", "Type 2",
           "Type 1", "Type 2","Type 1", "Type 2",
           "Type 1", "Type 2","Type 2","Type 1")
)
#ggflowchart(data, node_data, fill = type)

##building flowchart
flowchart <- ggflowchart(data_flow, node_data, fill = "type",
                         x_nudge = 0.45, ## width of box
                         y_nudge = 0.40,
                         text_size = 3.0) +
  ggplot2::scale_fill_brewer(palette = 1)
flowchart



#-------------------------------PART 6 
#barchart for missingness for each ELE variable 
table(one_hrs$normal_ele) #Print a table with counts for a categorical variable

ggplot(one_hrs,
       aes(x = normal_ele,
           fill = normal_ele)) +
  geom_bar() +
  labs(y = "Number of Participants",
       x = "cog normal at ELE assessment for each wave")




#-------------------------------PART 7
#missingness stacked barplot for combined waves variable
df<- select(one_hrs,173:211,MAX_PARENTAL_EDU,childrural,CHILDHOODSES) #all b vars 
df<- select(one_hrs,"childrural","CHILDHOODSES","MAX_PARENTAL_EDU")

tbl_summary(df)

#function convert dataframe to binary TRUE/FALSE matrix
toBinaryMatrix <- function(df){
  m<-c()
  for(i in colnames(df)){
    x<-sum(is.na(df[,i]))
    # missing value count
    m<-append(m,x)
    # non-missing value count
    m<-append(m,nrow(df)-x) 
  }
  
  # adding column and row names to matrix
  a<-matrix(m,nrow=2)
  rownames(a)<-c("TRUE","FALSE")
  colnames(a)<-colnames(df)
  
  return(a)
}

#stacked barplot for missing data in all columns
binMat = toBinaryMatrix(df)
#convert to DF
df <- data.frame( binMat)
df$ID <- rownames(df) 
y.melt <- melt(df, id.var = 'ID')

ggplot(y.melt, aes(x = variable, y = value, fill = ID)) +
  ggtitle("Missingness for wave 2014") +
  geom_bar(stat = 'identity') +
  xlab("Variable Name") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.title=element_blank())
#non-missing is False, Missing is TRUE 

#####missingness stacked barplot 2008 wave
df<- subset(h08b2, select = -c(HHID, PN))    
#function convert dataframe to binary TRUE/FALSE matrix
toBinaryMatrix <- function(df){
  m<-c()
  for(i in colnames(df)){
    x<-sum(is.na(df[,i]))
    # missing value count
    m<-append(m,x)
    # non-missing value count
    m<-append(m,nrow(df)-x) 
  }
  
  # adding column and row names to matrix
  a<-matrix(m,nrow=2)
  rownames(a)<-c("TRUE","FALSE")
  colnames(a)<-colnames(df)
  
  return(a)
}

#stacked barplot for missing data in all columns
binMat = toBinaryMatrix(df)
#convert to DF
df <- data.frame( binMat)
df$ID <- rownames(df) 
y.melt <- melt(df, id.var = 'ID')

ggplot(y.melt, aes(x = variable, y = value, fill = ID)) +
  ggtitle("Missingness for 2008 Wave") +
  geom_bar(stat = 'identity') +
  xlab("Variable Name") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.title=element_blank())
#non-missing is False, Missing is TRUE 


#####missingness stacked barplot 2010 wave
df<- subset(h10b2, select = -c(HHID, PN))    
# function convert dataframe to binary TRUE/FALSE matrix
toBinaryMatrix <- function(df){
  m<-c()
  for(i in colnames(df)){
    x<-sum(is.na(df[,i]))
    # missing value count
    m<-append(m,x)
    # non-missing value count
    m<-append(m,nrow(df)-x) 
  }
  
  # adding column and row names to matrix
  a<-matrix(m,nrow=2)
  rownames(a)<-c("TRUE","FALSE")
  colnames(a)<-colnames(df)
  
  return(a)
}

#stacked barplot for missing data in all columns
binMat = toBinaryMatrix(df)
#convert to DF
df <- data.frame( binMat)
df$ID <- rownames(df) 
y.melt <- melt(df, id.var = 'ID')

ggplot(y.melt, aes(x = variable, y = value, fill = ID)) +
  ggtitle("Missingness for 2010 Wave") +
  geom_bar(stat = 'identity') +
  xlab("Variable Name") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.title=element_blank())
#non-missing is False, Missing is TRUE  


#####missingness stacked barplot 2012 wave
df<- subset(h12b2, select = -c(HHID, PN))    
# function convert dataframe to binary TRUE/FALSE matrix
toBinaryMatrix <- function(df){
  m<-c()
  for(i in colnames(df)){
    x<-sum(is.na(df[,i]))
    # missing value count
    m<-append(m,x)
    # non-missing value count
    m<-append(m,nrow(df)-x) 
  }
  
  # adding column and row names to matrix
  a<-matrix(m,nrow=2)
  rownames(a)<-c("TRUE","FALSE")
  colnames(a)<-colnames(df)
  
  return(a)
}

#stacked barplot for missing data in all columns
binMat = toBinaryMatrix(df)
#convert to DF
df <- data.frame( binMat)
df$ID <- rownames(df) 
y.melt <- melt(df, id.var = 'ID')

ggplot(y.melt, aes(x = variable, y = value, fill = ID)) +
  ggtitle("Missingness for 2012 Wave") +
  geom_bar(stat = 'identity') +
  xlab("Variable Name") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.title=element_blank())
#non-missing is False, Missing is TRUE 

#####missingness stacked barplot 2014 wave
df<- subset(h14b2, select = -c(HHID, PN))    
# function convert dataframe to binary TRUE/FALSE matrix
toBinaryMatrix <- function(df){
  m<-c()
  for(i in colnames(df)){
    x<-sum(is.na(df[,i]))
    # missing value count
    m<-append(m,x)
    # non-missing value count
    m<-append(m,nrow(df)-x) 
  }
  
  # adding column and row names to matrix
  a<-matrix(m,nrow=2)
  rownames(a)<-c("TRUE","FALSE")
  colnames(a)<-colnames(df)
  
  return(a)
}


#stacked barplot for missing data in all columns
binMat = toBinaryMatrix(df)
#convert to DF
df <- data.frame( binMat)
df$ID <- rownames(df) 
y.melt <- melt(df, id.var = 'ID')

ggplot(y.melt, aes(x = variable, y = value, fill = ID)) +
  ggtitle("Missingness for 2014 Wave") +
  geom_bar(stat = 'identity') +
  xlab("Variable Name") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.title=element_blank())
#non-missing is False, Missing is TRUE   

