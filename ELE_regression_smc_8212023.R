
#===========================================================================

# File: ELE Regression
# Author: scarlet cockell & Yuan Jin
# Update Date: 8/21/2023

#===========================================================================

#===========================================================================
# Purpose:
#  This script serves as a combination of Yuan's code and mine, with 
#  updated inclusion & exclusion criteria created 7/2023
#  one code to rule all regression 
#===========================================================================
#path of folders
script_path= "G:/.shortcut-targets-by-id/1dGRzB0bSRxxSqHT9WaXQ13_WuVFFZaE7/earlylife/script/"
source_path= "G:/.shortcut-targets-by-id/1dGRzB0bSRxxSqHT9WaXQ13_WuVFFZaE7/earlylife/source/"

#load libraries
library(data.table)
library(plyr)
library(dplyr)
library(tibble)
library(tidyr)
library(tidyverse)
library(rsvg)
library(epitools)
library(plotly)
library(magrittr)
library(knitr)
library(R.utils) #insert function
library(forestplot)
library(kableExtra)
library(compareGroups)
library(manhattanly)
library(metafor)
#library(rowr)
library(xlsx)
library(stats)
library(gtsummary)
library(parameters)
#library(regioneR)

#load data
#load(paste0(source_path, "ELE_6166obs_0122.RData")) #yuans
one_hrs<-readRDS(paste0(source_path, "ELE_16509_241_smc.RData")) #with NEWcombo vars

#source functions
source(paste0(script_path, "functions.R"))

##force decimal digit as
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)


#--------------------------------------------- LOAD CATS/VARS/LABS------------------------------------------------------------#

#load categories & exposure list & labels 
categories=c("Infectious",
             "Neurological",
             "Respiratory",
             "Substance use",
             "Sensory",
             "Other physical health",
             "Child home environment")


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
  "mother_wrk",
  "B022",
  "fam_finance"
)




#create temp elements to use in regression --actually dont need this lol 
temp <- new_expos
temp.lab <- new_expo_lab

#--------------------------------------------Chi-squared test--------------------------------------------------------#
#################################PART 1
#only using fishers test for p-values using tbl_summary (in flowchart script)


#--------------------------------------------Regression -------------------------------------------------------------#

#Set ref group as the most common group (highest # of ppl)


#childrural
#most people did not live in rural area 
one_hrs$childrural <- relevel(one_hrs$childrural, ref = "NO")        # Apply relevel function

#rate fam SES fam_finance (parent var:B020)
#most people were average (poor-1,365; avg-2,990; well off-501)
one_hrs$fam_finance <- relevel(one_hrs$fam_finance, ref = "About Avg")

#Father unemployed B023
#most people said no(he was employed) (never lived w/-323; No-3,640; Yes-841)
one_hrs$B023 <- relevel(one_hrs$B023, ref = "NO")

#Mother work mother_wrk (parent var:B088)
#(not at all-1,782; all the time-1,435; some of the time-2,091)
one_hrs$mother_wrk <- relevel(one_hrs$mother_wrk, ref = "Some of the time")
##change ref group for parents smoked? B104- yes, one of them is maj resp -> or should we just collapse this category into yes/no?

#test for rate health as child 
one_hrs$B019 <- relevel(one_hrs$B019, ref = "Good")

#test
one_hrs$B020 <- relevel(one_hrs$B020, ref = "Pretty well off financially")



#crude model 
#select the variables 
#Dementia
d<-one_hrs[new_expos] 
d2<-select(one_hrs,demen_vs_norm_1 )
d<-cbind(d2,d)

#CIND
c<-one_hrs[new_expos] 
c2<-select(one_hrs,cind_vs_norm_1 )
c<-cbind(c2,c)


##==============================================CRUDE MODEL==========================================================## 

##Dementia 
dn1="demen_vs_norm_1~ +" 

reg_dn1<- lapply(new_expos, function(vars){
  glm(eval(paste(dn1, vars)), 
      data=d,
      family= binomial(link = "logit")
  )}
)


##CIND
cn1="cind_vs_norm_1~ + "

reg_cn1<- lapply(new_expos, function(vars){
  glm(eval(paste(cn1, vars)), 
      data=c,
      family=binomial(link = "logit")
  )}
)


names(reg_dn1)=names(reg_cn1) <- new_expo_lab 

#summary regression result
sumry.dn1 <- lapply(reg_dn1, summary)
sumry.cn1 <- lapply(reg_cn1, summary)


#--------------------------------------------OR AND CI----------------------------------------------------------------#
#Odds Ratio and Confidence interval
or.dn1 <- lapply(reg_dn1, function(x) 
  cbind.data.frame(exp(cbind(OR = coef(x), confint(x))), `P-value`=coef(summary(x))[,4], se=coef(summary(x))[,2]))

or.cn1 <- lapply(reg_cn1, function(x) 
  cbind.data.frame(exp(cbind(OR = coef(x), confint(x))), `P-value`=coef(summary(x))[,4], se=coef(summary(x))[,2]))

#====================================GLM RESULT TABLES================================================================#
#--dementia--#
glm_dn1<- do.call("rbind",or.dn1) #---> ugly but you get the names and can fix later 

#--CIND--#
glm_cn1<- do.call("rbind",or.cn1) #---> ugly but you get the names and can fix later 


#--FDR--#
glm_dn1$FDR <- p.adjust(glm_dn1$`P-value`, method="BH")
glm_cn1$FDR <- p.adjust(glm_cn1$`P-value`, method="BH")

#--Round those bad jacksons--#
glm_dn1 <- glm_dn1 %>%  mutate_if(is.numeric, ~round(.,3)) ##can change options and mutate specific columns 
glm_cn1 <- glm_cn1 %>%  mutate_if(is.numeric, ~round(.,3))
#--------------------------------------------SUMMARY TABLES-----------------------------------------------------------#
glm_dn1 <- glm_dn1 %>%
  mutate(CI= paste0("(", `2.5 %`, ", ", `97.5 %`, ")")) %>%
  select(-`2.5 %`, -`97.5 %`)

glm_cn1 <- glm_cn1 %>%
  mutate(CI= paste0("(", `2.5 %`, ", ", `97.5 %`, ")")) %>%
  select(-`2.5 %`, -`97.5 %`)


reg_result_table2.0 <- cbind(glm_dn1, glm_cn1)


#--------------------------------------------SAVE TABLES--------------------------------------------------------------#

write.xlsx(reg_result_table2.0, "G:/.shortcut-targets-by-id/1dGRzB0bSRxxSqHT9WaXQ13_WuVFFZaE7/earlylife/report/reg_result_table_NEW.xlsx", sheetName = "Sheet1", col.names = TRUE,
           row.names = TRUE, append = FALSE, showNA = TRUE, password = NULL)


##==============================================FULL MODEL==========================================================## 


#Dementia
d<-one_hrs[new_expos] 
d2<-select(one_hrs,demen_vs_norm_1 )
d<-cbind(d2,d)

#CIND
c<-one_hrs[new_expos] 
c2<-select(one_hrs,cind_vs_norm_1 )
c<-cbind(c2,c)

#--------------------------------------------REGRESSION RESULT--------------------------------------------------------#
#Regression result- Generalized linear model- Single Exposure Regression
dn1="demen_vs_norm_1~ SEX+RACE_HISP+SCHLYRS+MAX_PARENTAL_EDU+AGE_BASELINE+TIME_EXP_COG+" #CHANGED TO age at baseline
cn1="cind_vs_norm_1~  SEX+RACE_HISP+SCHLYRS+MAX_PARENTAL_EDU+AGE_BASELINE+TIME_EXP_COG+" #will be the 7th term 


reg_dn1<- lapply(new_expos, function(vars){
  glm(eval(paste(dn1, vars)), 
      data=one_hrs,
      family="binomial"
  )}
)


reg_cn1<- lapply(new_expos, function(vars){
  glm(eval(paste(cn1, vars)), 
      data=one_hrs,
      family="binomial"
  )}
)


names(reg_dn1)=names(reg_cn1) <- new_expo_lab 

#summary regression result
sumry.dn1 <- lapply(reg_dn1, summary)
sumry.cn1 <- lapply(reg_cn1, summary)


#--------------------------------------------OR AND CI--------------------------------------------------------------#
#Odds Ratio and Confidence interval
or.dn1 <- lapply(reg_dn1, function(x) 
  cbind.data.frame(exp(cbind(OR = coef(x), confint(x))), `P-value`=coef(summary(x))[,4], se=coef(summary(x))[,2]))

or.cn1 <- lapply(reg_cn1, function(x) 
  cbind.data.frame(exp(cbind(OR = coef(x), confint(x))), `P-value`=coef(summary(x))[,4], se=coef(summary(x))[,2]))



#====================================GLM RESULT TABLES===============================================================#
#--dementia--#
setattr(or.dn1, 'names', new_expo_lab) #set names
glm_dn1 <-rbindlist(or.dn1, use.names=TRUE, fill=TRUE, idcol="ID") #add names to list 
glm_dn1 <- lapply(or.dn1, function(dat) dat[10:dim(dat)[1],]) %>% rbindlist(idcol = TRUE)   #columns, rows #[1]-> returns number of rows

#--CIND--#
setattr(or.cn1, 'names', new_expo_lab) #set names
glm_cn1 <-rbindlist(or.cn1, use.names=TRUE, fill=TRUE, idcol="ID") #add names to list 
glm_cn1 <- lapply(or.cn1, function(dat) dat[10:dim(dat)[1],]) %>% rbindlist(idcol = TRUE)

#meta analysis
glm_dn1 <- glm_dn1 %>% add_meta %>% mutate_if(is.numeric, ~round(.,3)) 
glm_cn1 <- glm_cn1 %>% add_meta %>% mutate_if(is.numeric, ~round(.,3))  

#--FDR--#
glm_dn1$FDR <- p.adjust(glm_dn1$`P-value`, method="BH")
glm_cn1$FDR <- p.adjust(glm_cn1$`P-value`, method="BH")

#--Round--#
glm_dn1 <- glm_dn1 %>%  mutate_if(is.numeric, ~round(.,3))
glm_cn1 <- glm_cn1 %>%  mutate_if(is.numeric, ~round(.,3)) 


#--------------------------------------------SUMMARY TABLES-----------------------------------------------------------#
glm_dn1 <- glm_dn1 %>%
  mutate(CI= paste0("(", `2.5 %`, ", ", `97.5 %`, ")")) %>%
  select(-`2.5 %`, -`97.5 %`)

glm_cn1 <- glm_cn1 %>%
  mutate(CI= paste0("(", `2.5 %`, ", ", `97.5 %`, ")")) %>%
  select(-`2.5 %`, -`97.5 %`)


reg_adjusted_result_table <- cbind(glm_dn1, glm_cn1)


#--------------------------------------------SAVE TABLES--------------------------------------------------------------#

write.xlsx(reg_adjusted_result_table, "G:/.shortcut-targets-by-id/1dGRzB0bSRxxSqHT9WaXQ13_WuVFFZaE7/earlylife/report/reg_adjusted_result_table_fixed.xlsx", sheetName = "Sheet1", col.names = TRUE,
           row.names = TRUE, append = FALSE, showNA = TRUE, password = NULL)




##==============================================SENSITIVITY MODELS==========================================================## 


#Dementia & CIND vs. normal 
i<-one_hrs[new_expos] 
i2<-select(one_hrs,imp_vs_norm )
i<-cbind(i2,i)


#--------------------------------------------REGRESSION RESULT--------------------------------------------------------#
#Regression result- Generalized linear model- Single Exposure Regression
Imp1="imp_vs_norm~ SEX+RACE_HISP+SCHLYRS+MAX_PARENTAL_EDU+AGE_BASELINE+TIME_EXP_COG+" #CHANGED TO age at baseline


reg_Imp1<- lapply(new_expos, function(vars){
  glm(eval(paste(Imp1, vars)), 
      data=one_hrs,
      family="binomial"
  )}
)



names(reg_Imp1) <- new_expo_lab 

#summary regression result
sumry.Imp1 <- lapply(reg_Imp1, summary)



#--------------------------------------------OR AND CI--------------------------------------------------------------#
#Odds Ratio and Confidence interval
or.Imp1 <- lapply(reg_Imp1, function(x) 
  cbind.data.frame(exp(cbind(OR = coef(x), confint(x))), `P-value`=coef(summary(x))[,4], se=coef(summary(x))[,2]))


#====================================GLM RESULT TABLES===============================================================#
#--dementia--#
setattr(or.Imp1, 'names', new_expo_lab) #set names
glm_Imp1 <-rbindlist(or.Imp1, use.names=TRUE, fill=TRUE, idcol="ID") #add names to list 
glm_Imp1 <- lapply(or.Imp1, function(dat) dat[10:dim(dat)[1],]) %>% rbindlist(idcol = TRUE)   #columns, rows #[1]-> returns number of rows


#meta analysis
glm_Imp1 <- glm_Imp1 %>% add_meta %>% mutate_if(is.numeric, ~round(.,3)) 

#--FDR--#
glm_Imp1$FDR <- p.adjust(glm_Imp1$`P-value`, method="BH")



#--------------------------------------------SUMMARY TABLES-----------------------------------------------------------#
glm_Imp1 <- glm_Imp1 %>%
  mutate(CI= paste0("(", `2.5 %`, ", ", `97.5 %`, ")")) %>%
  select(-`2.5 %`, -`97.5 %`)


imp_vs_normal_regression <- cbind(glm_Imp1)

#--------------------------------------------SAVE TABLES--------------------------------------------------------------#

write.xlsx(imp_vs_normal_regression, "G:/.shortcut-targets-by-id/1dGRzB0bSRxxSqHT9WaXQ13_WuVFFZaE7/earlylife/report/imp_vs_normal_regression.xlsx", sheetName = "Sheet1", col.names = TRUE,
           row.names = TRUE, append = FALSE, showNA = TRUE, password = NULL)

#

####---------------------------~$TEST USING COLLAPSED CATEGORY VARIABLES$~-------------------------------####
#create temp elements to use in regression --actually dont need this lol 

temp= c("METABOLIC",
        "B106",
        "CANCER",
        "REPROD",
        "B124", 
        "OTHER")

temp.lab=c("endoc and metabolic",
           "diabetes",
           "cancer and skin conditions",
           "reproductive and prostate",
           "important or seris hlth cond",
           "other health condition - category")




#Dementia
d<-one_hrs[temp] 
d2<-select(one_hrs,demen_vs_norm_1 )
d<-cbind(d2,d)

#CIND
c<-one_hrs[temp] 
c2<-select(one_hrs,cind_vs_norm_1 )
c<-cbind(c2,c)

##Dementia 
dn1="demen_vs_norm_1~ SEX+RACE_HISP+SCHLYRS+MAX_PARENTAL_EDU+AGE_BASELINE+TIME_EXP_COG+"

reg_dn1<- lapply(temp, function(vars){
  glm(eval(paste(dn1, vars)), 
      data=one_hrs,
      family= binomial(link = "logit")
  )}
)
##CIND
cn1="cind_vs_norm_1~  SEX+RACE_HISP+SCHLYRS+MAX_PARENTAL_EDU+AGE_BASELINE+TIME_EXP_COG+"

reg_cn1<- lapply(temp, function(vars){
  glm(eval(paste(cn1, vars)), 
      data=one_hrs,
      family=binomial(link = "logit")
  )}
)


names(reg_dn1)=names(reg_cn1) <- temp.lab

#summary regression result
sumry.dn1 <- lapply(reg_dn1, summary)
sumry.cn1 <- lapply(reg_cn1, summary)


#--------------------------------------------OR AND CI--------------------------------------------------------------#
#Odds Ratio and Confidence interval
or.dn1 <- lapply(reg_dn1, function(x) 
  cbind.data.frame(exp(cbind(OR = coef(x), confint(x))), `P-value`=coef(summary(x))[,4], se=coef(summary(x))[,2]))

or.cn1 <- lapply(reg_cn1, function(x) 
  cbind.data.frame(exp(cbind(OR = coef(x), confint(x))), `P-value`=coef(summary(x))[,4], se=coef(summary(x))[,2]))

#====================================GLM RESULT TABLES==============================================================#
#--dementia--#
setattr(or.dn1, 'names', temp.lab) #set names
glm_dn1 <-rbindlist(or.dn1, use.names=TRUE, fill=TRUE, idcol="ID") #add names to list 
glm_dn1 <- lapply(or.dn1, function(dat) dat[10:dim(dat)[1],]) %>% rbindlist(idcol = TRUE)   #columns, rows #[1]-> returns number of rows

#--CIND--#
setattr(or.cn1, 'names', temp.lab) #set names
glm_cn1 <-rbindlist(or.cn1, use.names=TRUE, fill=TRUE, idcol="ID") #add names to list 
glm_cn1 <- lapply(or.cn1, function(dat) dat[10:dim(dat)[1],]) %>% rbindlist(idcol = TRUE)

#--dementia--#
glm_dn1<- do.call("rbind",or.dn1) #---> ugly but you get the names and can fix later 

#--CIND--#
glm_cn1<- do.call("rbind",or.cn1) #---> ugly but you get the names and can fix later 


#--FDR--#
glm_dn1$FDR <- p.adjust(glm_dn1$`P-value`, method="BH")
glm_cn1$FDR <- p.adjust(glm_cn1$`P-value`, method="BH")

#--Round those bad jacksons--#
glm_dn1 <- glm_dn1 %>%  mutate_if(is.numeric, ~round(.,3)) ##can change options and mutate specific columns 
glm_cn1 <- glm_cn1 %>%  mutate_if(is.numeric, ~round(.,3))

#--------------------------------------------SUMMARY TABLES-----------------------------------------------------------#
glm_dn1 <- glm_dn1 %>%
  mutate(CI= paste0("(", `2.5 %`, ", ", `97.5 %`, ")")) %>%
  select(-`2.5 %`, -`97.5 %`)

glm_cn1 <- glm_cn1 %>%
  mutate(CI= paste0("(", `2.5 %`, ", ", `97.5 %`, ")")) %>%
  select(-`2.5 %`, -`97.5 %`)


Creg_result_table <- cbind(glm_dn1, glm_cn1)
#--------------------------------------------SAVE TABLES-----------------------------------------------------------#

write.xlsx(Creg_result_table, "G:/.shortcut-targets-by-id/1dGRzB0bSRxxSqHT9WaXQ13_WuVFFZaE7/earlylife/report/test.xlsx", sheetName = "Sheet1", col.names = TRUE,
           row.names = TRUE, append = FALSE, showNA = TRUE, password = NULL)
#--------------------------------------------ADD LABLES FOR MANHATTAN PLOT------------------------------------------#

# #test added in \n
rnm2show <- c(
  "Measles before age 16 (Yes)",
  "Mumps before age 16 (Yes)",
  "Chicken pox before age 16 (Yes)",
  "Allergies, hay fever, sinusitis, tonsilitis (Yes)",
  "Overall odds ratio", #5
  "Ear problems before age 16 (Yes)",
  "Headaches or migraines before age 16 (Yes)",
  "Depression before age 16 (Yes)",
  "Emotional and psychological conditions  (Yes)",
  "Neurological and sensory conditions (Yes)",
  "Childhood- learning problems  (Yes)",
  "Childhood- concussion or severe head injury (Yes)",
  "Overall odds ratio", #8
  "Parents or guardians smoke (ref=No, none of them)",
  "              Yes, one of them",
  "              Yes, both of them",
  "Asthma before age 16 (Yes)",
  "Respiratory disorder before age 16 (Yes)",
  "Overall odds ratio", #6
  "Drugs, alchol, or smoking use before age 16 (Yes)",
  "Overall odds ratio", #2
  "Speech impairment before age 16 (Yes)",
  "Difficulty seeing before age 16 (Yes)",
  "Overall odds ratio", #3
  "Heart, circulatory and blood conditions (Yes)",
  "Childhood health conditions (Yes)",
  "Childhood- disability (Yes)",
  "Stomach problems before age 16 (Yes)",
  "Rate health as child (ref=excellent)",
  "              Very good",
  "              Good",
  "              Fair",
  "              Poor",
  "Digestive system (stomach, liver, gallbladder, kidney, bladder) (Yes)",
  "Musculoskeletal system and connective tissue (Yes)",
  "Overall odds ratio", #12
  "Lived rural area during school (Yes)",
  "Childhood- missed school",
  "Lived with grandparents duing childhood",
  "Father unemployed during childhood (ref=No)",
  "              Yes",
  "              Never lived with father/ father was not alive",
  "Mother work during childhood (ref=Some of the time)",
  "              All of the time",
  "              Not at all",
  "Family get financial help in childhood (Yes)",
  "Rate family financial situation -SES (ref=About average)",
  "              Pretty well off financially",
  "              Poor",
  "Overall odds ratio" #14
)

#--------------------------------------------FOREST PLOTS----------------------------------------------------------#
#-----------------CIND---#
##Forest Plot (Colored)
forest.data <- glm_cn1 %>%  ###5,12,12,16,17,19,29,29,32,34,37,39; 5,12,16,17,19,29,39
  select(OR, `2.5 %`, `97.5 %`) %>%
  lapply(insert,ats=c(14,28,38,40,43),values=NA) %>%  #12 #11  #inserting NA's where the categorical variables start 
  as.data.table  %>% 
  mutate(sub=c(rep(NA,2), "Infectious", #+1
               rep(NA,5), "Neurological",
               rep(NA,7), "Respiratory", #+2
               rep(NA,2), "Substance use",
               rep(NA,2), "Sensory", #+1
               rep(NA,3), "Other physical health", #+1 
               rep(NA,11),"Child home environment", #+1
               rep(NA,11)),
         class=c(rep(2,5),rep(3,8),rep(4,6),rep(5,2),rep(6,3),rep(7,12),rep(8,14))) #add color label

tabletext <- cbind(
  c("Category", "\n",forest.data$sub),  #took out /n 
  c("Exposure", "\n",rnm2show)
)

pop.cols <- c("black","#1B75BB","#1B745B", "darkorchid", "firebrick1","sienna1", "goldenrod1", "darkcyan")
# a self-defined function to color lines and boxes in forestplot
fn<-local({
  i=0
  bclrs=lclrs=c(rep("#1B75BB",4),  #inf
                rep("#1B745B",7),  #neu
                rep("darkorchid",4), #resp
                rep("firebrick1",1),  #su
                rep("sienna1",2),      #sen
                rep("goldenrod1",10),  #oph
                rep("darkcyan",10))     #che
  
  function(...,clr.line, clr.marker){
    i<<-i+1
    fpDrawNormalCI(...,clr.line=lclrs[i],clr.marker=bclrs[i])}
})
fn_sum<-local({
  i=0
  bclrs=lclrs=c("#1B75BB","#1B745B", "darkorchid", "firebrick1","sienna1", "goldenrod1", "darkcyan")
  function(col, size, ...) {
    i<<-i+1
    fpDrawDiamondCI(clr.line=lclrs[i],clr.marker=bclrs[i],size=.5, lwd=10,...)
  }
})

result_path <- "G:/.shortcut-targets-by-id/1dGRzB0bSRxxSqHT9WaXQ13_WuVFFZaE7/earlylife/report/forest plots/"


png(paste0(result_path, "ELE_Forest_CN_1_test.png"),width=5000, height=6000)
forestplot(labeltext=tabletext, 
           graph.pos=2,
           fn.ci_norm=fn,
           fn.ci_sum=fn_sum,
           mean=c(NA,NA,forest.data$OR), 
           lower=c(NA,NA,forest.data$`2.5 %`),
           upper=c(NA,NA,forest.data$`97.5 %`),
           
           xticks=c(0,1,2,3,4,5,6),
           clip=c(0, 8),
           zero=1,
           
           title="CIND vs Normal",
           xlab="Odds ratio of early life exposure on cognition status",
           
           is.summary=c(rep(F,6),TRUE,rep(F,7),TRUE,rep(F,5),TRUE,rep(F,1),TRUE,rep(F,2),TRUE,rep(F,11),TRUE,rep(F,13),TRUE),  #tells where overall OR goes and changes 
           txt_gp=fpTxtGp(label=list(gpar(fontface="bold",cex=5),
                                     gpar(cex=5)),
                          summary=gpar(fontface="bold.italic",cex=5),
                          ticks=gpar(fontface="bold",cex=5),
                          xlab=gpar(fontface="bold",cex=4),
                          title=gpar(cex=7)),
           col=fpColors(text=pop.cols[c(1,1,forest.data$class)], 
                        zero ="black"),
           cex=4, lineheight="auto", colgap=unit(6,"mm"),
           lwd.ci=5, boxsize=0.25,
           ci.vertices=TRUE, ci.vertices.height=0.2)
dev.off()

#-----------------Dementia---#
#Summary Plot
##Forest Plot (Colored)
forest.data <- glm_dn1 %>%  ###5,12,12,16,17,19,29,29,32,34,37,39; 5,12,16,17,19,29,39
  select(OR, `2.5 %`, `97.5 %`) %>%
  lapply(insert,ats=c(14,28,38,40,43),values=NA) %>%  #12 #11  #inserting NA's where the categorical variables start 
  as.data.table  %>% 
  mutate(sub=c(rep(NA,2), "Infectious", #+1
               rep(NA,5), "Neurological",
               rep(NA,7), "Respiratory", #+2
               rep(NA,2), "Substance use",
               rep(NA,2), "Sensory", #+1
               rep(NA,3), "Other physical health", #+1 
               rep(NA,11),"Child home environment", #+1
               rep(NA,11)),
         class=c(rep(2,5),rep(3,8),rep(4,6),rep(5,2),rep(6,3),rep(7,12),rep(8,14))) #add color label

tabletext <- cbind(
  c("Category", "\n",forest.data$sub),  #took out /n 
  c("Exposure", "\n",rnm2show)
)

pop.cols <- c("black","#1B75BB","#1B745B", "darkorchid", "firebrick1","sienna1", "goldenrod1", "darkcyan")
# a self-defined function to color lines and boxes in forestplot
fn<-local({
  i=0
  bclrs=lclrs=c(rep("#1B75BB",4),  #inf
                rep("#1B745B",7),  #neu
                rep("darkorchid",4), #resp
                rep("firebrick1",1),  #su
                rep("sienna1",2),      #sen
                rep("goldenrod1",10),  #oph
                rep("darkcyan",10))     #che
  
  function(...,clr.line, clr.marker){
    i<<-i+1
    fpDrawNormalCI(...,clr.line=lclrs[i],clr.marker=bclrs[i])}
})
fn_sum<-local({
  i=0
  bclrs=lclrs=c("#1B75BB","#1B745B", "darkorchid", "firebrick1","sienna1", "goldenrod1", "darkcyan")
  function(col, size, ...) {
    i<<-i+1
    fpDrawDiamondCI(clr.line=lclrs[i],clr.marker=bclrs[i],size=.5, lwd=10,...)
  }
})

result_path <- "G:/.shortcut-targets-by-id/1dGRzB0bSRxxSqHT9WaXQ13_WuVFFZaE7/earlylife/report/forest plots/"


png(paste0(result_path, "ELE_Forest_DN_1_test.png"),width=5000, height=6000)
forestplot(labeltext=tabletext, 
           graph.pos=2,
           fn.ci_norm=fn,
           fn.ci_sum=fn_sum,
           mean=c(NA,NA,forest.data$OR), 
           lower=c(NA,NA,forest.data$`2.5 %`),
           upper=c(NA,NA,forest.data$`97.5 %`),
           
           xticks=c(0,1,2,3,4,5,6),
           clip=c(0, 8),
           zero=1,
           
           title="Dementia vs Normal",
           xlab="Odds ratio of early life exposure on cognition status",
           
           is.summary=c(rep(F,6),TRUE,rep(F,7),TRUE,rep(F,5),TRUE,rep(F,1),TRUE,rep(F,2),TRUE,rep(F,11),TRUE,rep(F,13),TRUE),  #tells where overall OR goes and changes 
           txt_gp=fpTxtGp(label=list(gpar(fontface="bold",cex=5),
                                     gpar(cex=5)),
                          summary=gpar(fontface="bold.italic",cex=5),
                          ticks=gpar(fontface="bold",cex=5),
                          xlab=gpar(fontface="bold",cex=4),
                          title=gpar(cex=7)),
           col=fpColors(text=pop.cols[c(1,1,forest.data$class)], 
                        zero ="black"),
           cex=4, lineheight="auto", colgap=unit(6,"mm"),
           lwd.ci=5, boxsize=0.25,
           ci.vertices=TRUE, ci.vertices.height=0.2)
dev.off()



#--------------------------------------------MANHATTAN PLOTS---------------------------------------------------------#
#trying ggplot way 
manhplot <- ggplot(data = filter(brassica_df2, AvsDD > sig_cut), aes(x=position, y=AvsDD, color=as.factor(chromosome))) +
  geom_point(alpha = 0.8) +
  scale_x_continuous(label = axisdf$chromosome, breaks= axisdf$center) +
  scale_color_manual(values = rep(c("#276FBF", "#183059"), unique(length(axisdf$chromosome)))) +
  geom_hline(yintercept = sig_cut, lty = 2) +
  ylab("-log10 p value") +
  ylim(c(0,ylim)) +
  theme_classic() +
  theme(legend.position = "n")
print(manhplot)

###format the data
manhattan.data <- glm_dn1 %>%  ###5,12,12,16,17,19,29,29,32,34,37,39; 5,12,16,17,19,29,39
  select('P-value',.id) %>%
  as.data.table  %>% 
  mutate(cat=c(rep("Infectious",4), 
               rep("Neurological",7),
               rep("Respiratory",4), 
               rep("Substance use",1), 
               rep("Sensory",2),  
               rep("Other physical health",10),  
               rep("Child home environment", 10)),
         class=c(rep(2,4),rep(3,7),rep(4,4),rep(5,1),rep(6,2),rep(7,10),rep(8,10))) #add color label
##
manhplot <- ggplot(data = manhattan.data, aes(x=class, y='P-value'))+#, color=as.factor(manhattan.data$class))) +
  geom_point(alpha = 0.8) +
  #scale_x_continuous(label = manhattan.data$`P-value`) +
  #scale_color_manual(values = rep(c("#1B75BB","#1B745B", "darkorchid", "firebrick1","sienna1", "goldenrod1", "darkcyan"), unique(length(manhattan.data$class)))) +
  #geom_hline(yintercept = sig_cut, lty = 2) +
  ylab("-log10 p value") +
  #ylim(c(0,ylim)) +
  theme_classic() +
  theme(legend.position = "n")
print(manhplot)


#Interactive Plot-1 (no tick label)
#"Most recent cognition" - what is the most recent cognition (1,2,3)
###########################this works

man=manhattan.data
#main=rc_dn1
man <- data.frame(
  CHR= c(rep(1,4),rep(2,7),rep(3,4),rep(4,1),rep(5,2),rep(6,10), rep(7,10)),
  BP = c(seq(1:4),seq(1:7),seq(1:4),seq(1:1),seq(1:2),seq(1:10),seq(1:10)),
  P  = main$`P-value`,
  Category= factor(man$cat, levels=unique(man$cat)),
  Exposure= factor(man$.id, levels=unique(man$.id)),
  level= ifelse(is.na(man$class),"Yes",man$class)
)

manhattanly(man, snp="Category", gene="Exposure",annotation1="level", xlab= "Category")
###########################this works^^^
data_cum <- man %>% 
  group_by(CHR) %>% 
  summarise(max_bp = max(BP)) %>% 
  mutate(bp_add = lag(cumsum(max_bp), default = 0)) %>% 
  select(CHR, bp_add)

gwas_data <- gwas_data %>% 
  inner_join(data_cum, by = "CHR") %>% 
  mutate(bp_cum = bp + bp_add)

axis_set <- man %>% 
  group_by(CHR) %>% 
  summarize(center = mean(BP))

ylim <- man %>% 
  mutate(ylim = abs(floor(log10(p))) + 2) %>% 
  pull(ylim)

sig <- 5e-2

manhplot <- ggplot(gwas_data, aes(x = bp_cum, y = -log10(p), 
                                  color = as_factor(chr), size = -log10(p))) +
  geom_hline(yintercept = -log10(sig), color = "grey40", linetype = "dashed") + 
  geom_point(alpha = 0.75) +
  scale_x_continuous(label = axis_set$chr, breaks = axis_set$center) +
  scale_y_continuous(expand = c(0,0), limits = c(0, ylim)) +
  scale_color_manual(values = rep(c("#276FBF", "#183059"), 
                                  unique(length(axis_set$chr)))) +
  scale_size_continuous(range = c(0.5,3)) +
  labs(x = NULL, 
       y = "-log<sub>10</sub>(p)") + 
  theme_minimal() +
  theme( 
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.y = element_markdown(),
    axis.text.x = element_text(angle = 60, size = 8, vjust = 0.5)
  )
###############################test

createDataset <- function(num.snps=38, max.peaks=7) {
  #hg19.genome <- filterChromosomes(getGenome("hg19"))
  snps <-glm_dn1
  #snps <- sort(createRandomRegions(nregions=7, length.mean=4, 
                                   #length.sd=0, genome=filterChromosomes(getGenome("hg19"))))
  names(snps$.id) <- paste0("rs", seq_len(num.snps))
  glm_dn1$`P-value` <- rnorm(n = num.snps, mean = 0.5, sd = 1)
  glm_dn1$`P-value`[glm_dn1$`P-value`<0] <- -1*glm_dn1$`P-value`[glm_dn1$`P-value`<0]
  #define the "significant peaks"
  peaks <- createRandomRegions(runif(1, 1, max.peaks), 8e6, 4e6)
  peaks
  for(npeak in seq_along(peaks)) {
    snps.in.peak <- which(overlapsAny(snps, peaks[npeak]))
    glm_dn1$`P-value`[snps.in.peak] <- runif(n = length(snps.in.peak), 
                                     min=0.1, max=runif(1,6,8))
  }
  glm_dn1$`P-value` <- 10^(-1*glm_dn1$`P-value`)
  return(list(peaks=peaks, snps=snps))
}


ds <- createDataset()
ds$snps


###############################

main=rc_cn1
man <- data.frame(
  CHR= c(rep(1,5),rep(2,7),rep(3,5),rep(4,2),rep(5,10),rep(6,19)),
  BP = c(seq(1:5), seq(1:7), seq(1:5), seq(1:2), seq(1:10), seq(1:19)),
  P  = main$`P-value`,
  Category= factor(main$sub, levels=unique(main$sub)),
  Exposure= factor(main$name, levels=unique(main$name)),
  level= ifelse(is.na(main$cate),"Yes",main$cate)
)

manhattanly(man, snp="Category", gene="Exposure",annotation1="level", xlab= "Category")


#"Ever dementia, most recent cognition" - ever dementia, if not - then the most recent cognition (1,2,3)
main=rc_dn2
man <- data.frame(
  CHR= c(rep(1,5),rep(2,7),rep(3,5),rep(4,2),rep(5,10),rep(6,25)),
  BP = c(seq(1:5), seq(1:7), seq(1:5), seq(1:2), seq(1:10), seq(1:25)),
  P  = main$`P-value`,
  Category= factor(main$sub, levels=unique(main$sub)),
  Exposure= factor(main$name, levels=unique(main$name)),
  level= ifelse(is.na(main$cate),"Yes",main$cate)
)

manhattanly(man, snp="Category", gene="Exposure",annotation1="level", xlab= "Category")


main=rc_cn2
man <- data.frame(
  CHR= c(rep(1,5),rep(2,7),rep(3,5),rep(4,2),rep(5,10),rep(6,19)),
  BP = c(seq(1:5), seq(1:7), seq(1:5), seq(1:2), seq(1:10), seq(1:19)),
  P  = main$`P-value`,
  Category= factor(main$sub, levels=unique(main$sub)),
  Exposure= factor(main$name, levels=unique(main$name)),
  level= ifelse(is.na(main$cate),"Yes",main$cate)
)

manhattanly(man, snp="Category", gene="Exposure",annotation1="level", xlab= "Category")




#Interactive Plot-2 (plot_ly)
xorder <- list(#categoryorder = "array", 
  #categoryarray = unique(main$name),
  dtick =1, tickangle=270,size=2,ticklen=20,showticklabels = FALSE,
  title = "")


plot_ly(data=man, x=~Exposure, y=~-log10(P), 
        text = ~paste("Level: ", level),
        color=~Category, type='scatter', 
        mode='markers', marker=list(size=8)) %>% 
  #add_trace(y = ~trace1, name='Bonferroni', mode='lines') %>%
  #add_trace(y = ~trace2, name='Suggestive', mode='lines') %>%
  layout(xaxis = xorder,
         title = "Manhattan plot of Early Life Exposure" #\n (Bonferroni=3.033, Suggestive=1.301)
  )





#fig.width=8, fig.height=5, fig.align="center"
#Plot-3 no exposure label
pop.cols <- c("#1B75BB", "darkorchid", "firebrick1","sienna1", "goldenrod1", "darkcyan")

par(mai=c(10,1,1,3),mar=c(10,2,0.1,3))
plot(1:nrow(main), -log10(main$P), cex=1.5, pch=20, las=1, xlab="", cex.lab=1.3, ylab=expression(-log[10](italic(p))), col=pop.cols[man$CHR], xaxt="n")
axis(1, cex.axis=1, las=2,at=c(3,9,15,18,25,40), labels= categories)
axis(4, at=c(1.301, 3.033),labels=c(1.301, 3.033), las=2, cex.axis=0.7, tck=-.01)
abline(h=-log10(0.05), col="lightgrey")
abline(h=-log10(0.05/54), col="grey")
#dev.off()



#pdf("~/Desktop/manhattan_plot2.pdf", width=10, height=10)
rnm2show  <- ifelse(is.na(main$cate), main$name, paste("            ",main$cate))
#insert multicategorical exposures and their corresponding reference group
rnm2show  <- insert(rnm2show, ats=c(13,26,33,36,39,45,52), values=refers) 
man2= lapply(man[,1:3], insert, ats=c(13,26,33,36,39,45,52), values=NA) %>% as.data.table 
man2$groups= c(rep(1,5),rep(2,7),rep(3,6),rep(4,2),rep(5,11),rep(6,30))
man2$expo  = rnm2show
P= as.vector(-log10(man2$P))
names(P) = rnm2show

dotchart(P,labels=names(P),cex=.6,groups= man2$groups,
         main="", gcolor=pop.cols,
         xlab="-log10(P)", color=pop.cols[man2$groups]
)
#dev.off()


###test###################################################################################################
new_expos= c(
  "B112",
  "B117",
  "B122",
  "B115",
  "METABOLIC",
  "B106",
  "CANCER",
  "REPROD",
  "B110")

new_expo_lab= c(
  "epilepsy",
  "drugs and alc alone",
  "smoking",
  "high bp",
  "metabolic",
  "diabetes",
  "cancer",
  "reproductive and prostate",
  "heart trouble")

###########################################################################


