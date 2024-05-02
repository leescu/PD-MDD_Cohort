# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 0. Packages and Functions used <<<<< ####
{#* section 0 Packages ####
  library(caret)
  library(car)
  library(cmprsk)
  library(dplyr)
  library(foreign)
  library(ggplot2)
  library(ggsci)
  library(ggrepel)
  library("ggthemes")
  library(lava)
  library(Matching)
  library(mediation)
  library(mice)
  library(pec)
  #install.packages("poLCA", dependencies = TRUE)
  library(poLCA)
  library(plyr)
  library(prodlim)
  library(reshape2)
  library(rms)
  library(riskRegression)
  library(survey)
  library(scales)
  library(survminer)
  library(survival)
  library(splines)
  library(timeROC)
  library(tableone)
  library(rms)
  library(withr)
  library(dplyr)
  library(doParallel)
}
# +++++++++++================================+++++++++++ ####
# +++++++++++============Manuscript==============+++++++++++ ####
# +++++++++++================================+++++++++++ ####  
# +++++++++++============Tables==========+++++++++++ ####  
# >>>>> section 18. Multiple interpolation data (Table 1, Table S3)  ####
load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)

table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
var<-c("MORT_stat","Age_status","Sex","Marital_status","Race_ethnicity","Education_levels","PIR",
       "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status","HEI",
       "HPL_status","HTN_status","CVD_status",
       "BMI_status","T2D_status","Cancer_status","Cohort","PD_diagnosis")
VAR<-c("MORT_stat","CAL_mean","PPD_mean","Age","Marital_status","Age_status","Sex","Race_ethnicity","Education_levels","PIR",
           "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status","HEI", "HTN_status","HPL_status",
           "BMI","BMI_status","T2D_status","CVD_status","Cancer_status","Cohort","PD_diagnosis")

options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)
{ #* section 18.1 Over all ####
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  Over<- ldply(lapply(VAR, model))
}
options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)

{ #* section 18.2 No/Mild periodontitis ####
  rhcSvy_HEI2PD<-subset(rhcSvy,PD_diagnosis=="No/Mild periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_HEI2PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  noPD<- ldply(lapply(VAR, model))
}  
{ #* section 18.3 Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,PD_diagnosis=="Moderate/Severe periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  PD<- ldply(lapply(VAR, model))
}
Table1<-cbind(Over,noPD[,c("counts","Mean","SE")],PD[,c("counts","Mean","SE")])
save(Table1,file = "I:/paper_8_PD&MDD/data/Table1_Rdata")  
{ #* section 18.4 t-test and chi-test ####
  model<- function(x){
    
    if( x %in% var ) {
      formula<-as.formula(paste0("~",x,"+PD_diagnosis"))
      chi_test<-svychisq(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =chi_test[["p.value"]])
      return(model)
    } else {
      formula<-as.formula(paste0(x,"~PD_diagnosis"))
      t_test<-svyttest(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =t_test[["p.value"]])
      return(model)
    }
  }  
  test_data<- ldply(lapply(VAR, model))
  test_data$P.value<-round(test_data$P.value,3)
  test_data$P.value[test_data$P.value==0]<-"<0.001"
  new.function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  test_data$P.value<-lapply(test_data$P.value,new.function)
  test_data$P.value<-as.character(test_data$P.value)
}
load(file = "I:/paper_8_PD&MDD/data/Table1_Rdata")
Table1<-merge(Table1,test_data,by="Covariates",all.x = T)
Table1$Covariates
Table1$Row<-paste0(Table1$Covariates," ",Table1$grade)
rownames(Table1)<-Table1$Row
colnames(Table1)<-c("Covariates","grade",
                    "counts_all","Mean_all","SE_all",
                    "counts_noPD","Mean_noPD","SE_noPD",
                    "counts_PD","Mean_PD","SE_PD",
                    "P.value","Row")
rownames(Table1)
{ #* section 18.5 Combine  Table 1#####
#Events
#all.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")

PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]

PD.counts_ad<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
noPD.counts_ad<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
Counts_ad<-format(round(sum(PD_MDD$weight))+round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)


Table<-c("","","","Periodontal status","","","","")
Table<-rbind(Table,c("","Over all","","No/Mild periodontitis","","Moderate/Severe periodontitis","",""))
Table<-rbind(Table,c("Characteristics","Mean/ %*","SE*","Mean/ %","SE","Mean/ %","SE","P†"))
Table<-rbind(Table,c("No.(Unweighted)",PD.counts[1]+PD.counts[2],"",PD.counts[1],"",PD.counts[2],"",""))
Table<-rbind(Table,c("No.(Weighted)",Counts_ad,"",PD.counts_ad,"",noPD.counts_ad,"",""))
#Age
Table<-rbind(Table,c("Age (years), mean",
                     Table1["Age Mean ± SE","Mean_all"],Table1["Age Mean ± SE","SE_all"],
                     Table1["Age Mean ± SE","Mean_noPD"],Table1["Age Mean ± SE","SE_noPD"],
                     Table1["Age Mean ± SE","Mean_PD"],Table1["Age Mean ± SE","SE_PD"],
                     Table1["Age Mean ± SE","P.value"] ))
Table<-rbind(Table,c("Age status, %","","","","","","",Table1["Age_status <45","P.value"]))
Table<-rbind(Table,c("<45",
                     Table1["Age_status <45","Mean_all"],Table1["Age_status <45","SE_all"],
                     Table1["Age_status <45","Mean_noPD"],Table1["Age_status <45","SE_noPD"],
                     Table1["Age_status <45","Mean_PD"],Table1["Age_status <45","SE_PD"],
                     "" ))
Table<-rbind(Table,c("[45, 65)",
                     Table1["Age_status [45,65)","Mean_all"],Table1["Age_status [45,65)","SE_all"],
                     Table1["Age_status [45,65)","Mean_noPD"],Table1["Age_status [45,65)","SE_noPD"],
                     Table1["Age_status [45,65)","Mean_PD"],Table1["Age_status [45,65)","SE_PD"],
                     "" ))
Table<-rbind(Table,c("≥65",
                     Table1["Age_status >=65","Mean_all"],Table1["Age_status >=65","SE_all"],
                     Table1["Age_status >=65","Mean_noPD"],Table1["Age_status >=65","SE_noPD"],
                     Table1["Age_status >=65","Mean_PD"],Table1["Age_status >=65","SE_PD"],
                     "" ))

#Sex
Table<-rbind(Table,c("Sex, Female",
                     Table1["Sex Female","Mean_all"],Table1["Sex Female","SE_all"],
                     Table1["Sex Female","Mean_noPD"],Table1["Sex Female","SE_noPD"],
                     Table1["Sex Female","Mean_PD"],Table1["Sex Female","SE_PD"],
                     Table1["Sex Female","P.value"]))

#Race/ ethnicity
Table<-rbind(Table,c("Race/ ethnicity, %","","","","","","",Table1["Race_ethnicity Non-Hispanic White","P.value"]))
Table<-rbind(Table,c("Non-Hispanic white",
                     Table1["Race_ethnicity Non-Hispanic White","Mean_all"],Table1["Race_ethnicity Non-Hispanic White","SE_all"],
                     Table1["Race_ethnicity Non-Hispanic White","Mean_noPD"],Table1["Race_ethnicity Non-Hispanic White","SE_noPD"],
                     Table1["Race_ethnicity Non-Hispanic White","Mean_PD"],Table1["Race_ethnicity Non-Hispanic White","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Non-Hispanic black",
                     Table1["Race_ethnicity Non-Hispanic Black","Mean_all"],Table1["Race_ethnicity Non-Hispanic Black","SE_all"],
                     Table1["Race_ethnicity Non-Hispanic Black","Mean_noPD"],Table1["Race_ethnicity Non-Hispanic Black","SE_noPD"],
                     Table1["Race_ethnicity Non-Hispanic Black","Mean_PD"],Table1["Race_ethnicity Non-Hispanic Black","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Hispanic",
                     Table1["Race_ethnicity Hispanic","Mean_all"],Table1["Race_ethnicity Hispanic","SE_all"],
                     Table1["Race_ethnicity Hispanic","Mean_noPD"],Table1["Race_ethnicity Hispanic","SE_noPD"],
                     Table1["Race_ethnicity Hispanic","Mean_PD"],Table1["Race_ethnicity Hispanic","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Other race/ ethnicity",
                     Table1["Race_ethnicity Other_Race","Mean_all"],Table1["Race_ethnicity Other_Race","SE_all"],
                     Table1["Race_ethnicity Other_Race","Mean_noPD"],Table1["Race_ethnicity Other_Race","SE_noPD"],
                     Table1["Race_ethnicity Other_Race","Mean_PD"],Table1["Race_ethnicity Other_Race","SE_PD"],
                     "" ))
#Marital status
Table<-rbind(Table,c("Marital status, %","","","","","","",Table1["Marital_status Married","P.value"]))
Table<-rbind(Table,c("Married",
                     Table1["Marital_status Married","Mean_all"],Table1["Marital_status Married","SE_all"],
                     Table1["Marital_status Married","Mean_noPD"],Table1["Marital_status Married","SE_noPD"],
                     Table1["Marital_status Married","Mean_PD"],Table1["Marital_status Married","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Never married",
                     Table1["Marital_status Never_married","Mean_all"],Table1["Marital_status Never_married","SE_all"],
                     Table1["Marital_status Never_married","Mean_noPD"],Table1["Marital_status Never_married","SE_noPD"],
                     Table1["Marital_status Never_married","Mean_PD"],Table1["Marital_status Never_married","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Widowed/ Divorced/ Separated",
                     Table1["Marital_status Separated","Mean_all"],Table1["Marital_status Separated","SE_all"],
                     Table1["Marital_status Separated","Mean_noPD"],Table1["Marital_status Separated","SE_noPD"],
                     Table1["Marital_status Separated","Mean_PD"],Table1["Marital_status Separated","SE_PD"],
                     "" ))

#Socioeconomic Status
Table<-rbind(Table,c("Socioeconomic Status, %","","","","","","",Table1["SES low","P.value"]))
Table<-rbind(Table,c("Low",
                     Table1["SES low","Mean_all"],Table1["SES low","SE_all"],
                     Table1["SES low","Mean_noPD"],Table1["SES low","SE_noPD"],
                     Table1["SES low","Mean_PD"],Table1["SES low","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Medium",
                     Table1["SES medium","Mean_all"],Table1["SES medium","SE_all"],
                     Table1["SES medium","Mean_noPD"],Table1["SES medium","SE_noPD"],
                     Table1["SES medium","Mean_PD"],Table1["SES medium","SE_PD"],
                     "" ))
Table<-rbind(Table,c("High",
                     Table1["SES high","Mean_all"],Table1["SES high","SE_all"],
                     Table1["SES high","Mean_noPD"],Table1["SES high","SE_noPD"],
                     Table1["SES high","Mean_PD"],Table1["SES high","SE_PD"],
                     "" ))

#Smoking status
Table<-rbind(Table,c("Smoking status, %","","","","","","",Table1["Smoking_status Never_smoker","P.value"]))
Table<-rbind(Table,c("Never smoker",
                     Table1["Smoking_status Never_smoker","Mean_all"],Table1["Smoking_status Never_smoker","SE_all"],
                     Table1["Smoking_status Never_smoker","Mean_noPD"],Table1["Smoking_status Never_smoker","SE_noPD"],
                     Table1["Smoking_status Never_smoker","Mean_PD"],Table1["Smoking_status Never_smoker","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Former smoker",
                     Table1["Smoking_status Former_smoker","Mean_all"],Table1["Smoking_status Former_smoker","SE_all"],
                     Table1["Smoking_status Former_smoker","Mean_noPD"],Table1["Smoking_status Former_smoker","SE_noPD"],
                     Table1["Smoking_status Former_smoker","Mean_PD"],Table1["Smoking_status Former_smoker","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Current smoker",
                     Table1["Smoking_status Current_smoker","Mean_all"],Table1["Smoking_status Current_smoker","SE_all"],
                     Table1["Smoking_status Current_smoker","Mean_noPD"],Table1["Smoking_status Current_smoker","SE_noPD"],
                     Table1["Smoking_status Current_smoker","Mean_PD"],Table1["Smoking_status Current_smoker","SE_PD"],
                     "" ))

#Drinking status
Table<-rbind(Table,c("Drinking status, %","","","","","","",Table1["Drinking_status Nondrinker","P.value"]))
Table<-rbind(Table,c("Nondrinker",
                     Table1["Drinking_status Nondrinker","Mean_all"],Table1["Drinking_status Nondrinker","SE_all"],
                     Table1["Drinking_status Nondrinker","Mean_noPD"],Table1["Drinking_status Nondrinker","SE_noPD"],
                     Table1["Drinking_status Nondrinker","Mean_PD"],Table1["Drinking_status Nondrinker","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Light/ moderate drinker",
                     Table1["Drinking_status Light/moderate_drinker","Mean_all"],Table1["Drinking_status Light/moderate_drinker","SE_all"],
                     Table1["Drinking_status Light/moderate_drinker","Mean_noPD"],Table1["Drinking_status Light/moderate_drinker","SE_noPD"],
                     Table1["Drinking_status Light/moderate_drinker","Mean_PD"],Table1["Drinking_status Light/moderate_drinker","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Heavier drinker",
                     Table1["Drinking_status Heavier_drinker","Mean_all"],Table1["Drinking_status Heavier_drinker","SE_all"],
                     Table1["Drinking_status Heavier_drinker","Mean_noPD"],Table1["Drinking_status Heavier_drinker","SE_noPD"],
                     Table1["Drinking_status Heavier_drinker","Mean_PD"],Table1["Drinking_status Heavier_drinker","SE_PD"],
                     "" ))
#Physical status
Table<-rbind(Table,c("Physical status, %","","","","","","",Table1["Physical_status Inactive","P.value"]))
Table<-rbind(Table,c("Inactive",
                     Table1["Physical_status Inactive","Mean_all"],Table1["Physical_status Inactive","SE_all"],
                     Table1["Physical_status Inactive","Mean_noPD"],Table1["Physical_status Inactive","SE_noPD"],
                     Table1["Physical_status Inactive","Mean_PD"],Table1["Physical_status Inactive","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Insufficient",
                     Table1["Physical_status Insufficient","Mean_all"],Table1["Physical_status Insufficient","SE_all"],
                     Table1["Physical_status Insufficient","Mean_noPD"],Table1["Physical_status Insufficient","SE_noPD"],
                     Table1["Physical_status Insufficient","Mean_PD"],Table1["Physical_status Insufficient","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Recommended",
                     Table1["Physical_status Recommended","Mean_all"],Table1["Physical_status Recommended","SE_all"],
                     Table1["Physical_status Recommended","Mean_noPD"],Table1["Physical_status Recommended","SE_noPD"],
                     Table1["Physical_status Recommended","Mean_PD"],Table1["Physical_status Recommended","SE_PD"],
                     "" ))

#Healthy Eating Index
#Table<-rbind(Table,c("Healthy eating index, %","","","","","","",Table1["HEI Quintile 1","P.value"]))

# Table<-rbind(Table,c("Quintile 1",
#                      Table1["HEI Quintile 1","Mean_all"],Table1["HEI Quintile 1","SE_all"],
#                      Table1["HEI Quintile 1","Mean_noPD"],Table1["HEI Quintile 1","SE_noPD"],
#                      Table1["HEI Quintile 1","Mean_PD"],Table1["HEI Quintile 1","SE_PD"],
#                      "" ))
# Table<-rbind(Table,c("Quintile 2",
#                      Table1["HEI Quintile 2","Mean_all"],Table1["HEI Quintile 2","SE_all"],
#                      Table1["HEI Quintile 2","Mean_noPD"],Table1["HEI Quintile 2","SE_noPD"],
#                      Table1["HEI Quintile 2","Mean_PD"],Table1["HEI Quintile 2","SE_PD"],
#                      "" ))
# 
# Table<-rbind(Table,c("Quintile 3",
#                      Table1["HEI Quintile 3","Mean_all"],Table1["HEI Quintile 3","SE_all"],
#                      Table1["HEI Quintile 3","Mean_noPD"],Table1["HEI Quintile 3","SE_noPD"],
#                      Table1["HEI Quintile 3","Mean_PD"],Table1["HEI Quintile 3","SE_PD"],
#                      "" ))
# Table<-rbind(Table,c("Quintile 4",
#                      Table1["HEI Quintile 4","Mean_all"],Table1["HEI Quintile 4","SE_all"],
#                      Table1["HEI Quintile 4","Mean_noPD"],Table1["HEI Quintile 4","SE_noPD"],
#                      Table1["HEI Quintile 4","Mean_PD"],Table1["HEI Quintile 4","SE_PD"],
#                      "" ))
# Table<-rbind(Table,c("Quintile 5",
#                      Table1["HEI Quintile 5","Mean_all"],Table1["HEI Quintile 5","SE_all"],
#                      Table1["HEI Quintile 5","Mean_noPD"],Table1["HEI Quintile 5","SE_noPD"],
#                      Table1["HEI Quintile 5","Mean_PD"],Table1["HEI Quintile 5","SE_PD"],
#                      "" ))

#BMI
Table<-rbind(Table,c("BMI, Mean",
                     Table1["BMI Mean ± SE","Mean_all"],Table1["BMI Mean ± SE","SE_all"],
                     Table1["BMI Mean ± SE","Mean_noPD"],Table1["BMI Mean ± SE","SE_noPD"],
                     Table1["BMI Mean ± SE","Mean_PD"],Table1["BMI Mean ± SE","SE_PD"],
                     Table1["BMI Mean ± SE","P.value"] ))

Table<-rbind(Table,c("BMI status (kg/m2), %","","","","","","",Table1["BMI_status (0,25)","P.value"]))
Table<-rbind(Table,c("<25",
                     Table1["BMI_status (0,25)","Mean_all"],Table1["BMI_status (0,25)","SE_all"],
                     Table1["BMI_status (0,25)","Mean_noPD"],Table1["BMI_status (0,25)","SE_noPD"],
                     Table1["BMI_status (0,25)","Mean_PD"],Table1["BMI_status (0,25)","SE_PD"],
                     "" ))
Table<-rbind(Table,c("[25.0 -30)",
                     Table1["BMI_status [25.0-30)","Mean_all"],Table1["BMI_status [25.0-30)","SE_all"],
                     Table1["BMI_status [25.0-30)","Mean_noPD"],Table1["BMI_status [25.0-30)","SE_noPD"],
                     Table1["BMI_status [25.0-30)","Mean_PD"],Table1["BMI_status [25.0-30)","SE_PD"],
                     "" ))
Table<-rbind(Table,c("≥30",
                     Table1["BMI_status [30,inf)","Mean_all"],Table1["BMI_status [30,inf)","SE_all"],
                     Table1["BMI_status [30,inf)","Mean_noPD"],Table1["BMI_status [30,inf)","SE_noPD"],
                     Table1["BMI_status [30,inf)","Mean_PD"],Table1["BMI_status [30,inf)","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Comorbidities, %","","","","","","",""))
#Hypertension
Table<-rbind(Table,c("Hypertension",
                     Table1["HTN_status YES","Mean_all"],Table1["HTN_status YES","SE_all"],
                     Table1["HTN_status YES","Mean_noPD"],Table1["HTN_status YES","SE_noPD"],
                     Table1["HTN_status YES","Mean_PD"],Table1["HTN_status YES","SE_PD"],
                     Table1["HTN_status YES","P.value"]))
#Hyperlipidemia
Table<-rbind(Table,c("Hyperlipidemia",
                     Table1["HPL_status YES","Mean_all"],Table1["HPL_status YES","SE_all"],
                     Table1["HPL_status YES","Mean_noPD"],Table1["HPL_status YES","SE_noPD"],
                     Table1["HPL_status YES","Mean_PD"],Table1["HPL_status YES","SE_PD"],
                     Table1["HPL_status YES","P.value"]))
#Diabetes mellitus
Table<-rbind(Table,c("Diabetes mellitus",
                     Table1["T2D_status YES","Mean_all"],Table1["T2D_status YES","SE_all"],
                     Table1["T2D_status YES","Mean_noPD"],Table1["T2D_status YES","SE_noPD"],
                     Table1["T2D_status YES","Mean_PD"],Table1["T2D_status YES","SE_PD"],
                     Table1["T2D_status YES","P.value"]))
#Cohort
Table<-rbind(Table,c("Cohort period, %","","","","","","",Table1["Cohort NHANES_CON1","P.value"]))

Table<-rbind(Table,c("NHANES III",
                     Table1["Cohort NHANES_III","Mean_all"],Table1["Cohort NHANES_III","SE_all"],
                     Table1["Cohort NHANES_III","Mean_noPD"],Table1["Cohort NHANES_III","SE_noPD"],
                     Table1["Cohort NHANES_III","Mean_PD"],Table1["Cohort NHANES_III","SE_PD"],
                     "" ))
Table<-rbind(Table,c("NHANES 1999-2004",
                     Table1["Cohort NHANES_CON1","Mean_all"],Table1["Cohort NHANES_CON1","SE_all"],
                     Table1["Cohort NHANES_CON1","Mean_noPD"],Table1["Cohort NHANES_CON1","SE_noPD"],
                     Table1["Cohort NHANES_CON1","Mean_PD"],Table1["Cohort NHANES_CON1","SE_PD"],
                     "" ))
Table<-rbind(Table,c("NHANES 2009-2014",
                     Table1["Cohort NHANES_CON2","Mean_all"],Table1["Cohort NHANES_CON2","SE_all"],
                     Table1["Cohort NHANES_CON2","Mean_noPD"],Table1["Cohort NHANES_CON2","SE_noPD"],
                     Table1["Cohort NHANES_CON2","Mean_PD"],Table1["Cohort NHANES_CON2","SE_PD"],
                     "" ))

Table_1<-Table
write.table(Table_1,sep = ",",file ="I:/paper_8_PD&MDD/result/Table 1.csv" ,row.names =F,col.names =F )
}

{ #* section 18.6 Combine  Table S3#####
Table<-c("","","","Periodontal status","","","","")
Table<-rbind(Table,c("","Over all","","No/Mild periodontitis","","Moderate/Severe periodontitis","",""))
Table<-rbind(Table,c("Characteristics","Mean/ %*","SE*","Mean/ %","SE","Mean/ %","SE","P†"))
Table<-rbind(Table,c("No.(Unweighted)",PD.counts[1]+PD.counts[2],"",PD.counts[1],"",PD.counts[2],"",""))
Table<-rbind(Table,c("No.(Weighted)",Counts_ad,"",PD.counts_ad,"",noPD.counts_ad,"",""))

#Socioeconomic index
Table<-rbind(Table,c("Socioeconomic index, %","","","","","","",Table1["SEI Unemployment","P.value"]))
Table<-rbind(Table,c("Unemployment",
                     Table1["SEI Unemployment","Mean_all"],Table1["SEI Unemployment","SE_all"],
                     Table1["SEI Unemployment","Mean_noPD"],Table1["SEI Unemployment","SE_noPD"],
                     Table1["SEI Unemployment","Mean_PD"],Table1["SEI Unemployment","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Lower",
                     Table1["SEI Lower","Mean_all"],Table1["SEI Lower","SE_all"],
                     Table1["SEI Lower","Mean_noPD"],Table1["SEI Lower","SE_noPD"],
                     Table1["SEI Lower","Mean_PD"],Table1["SEI Lower","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Upper",
                     Table1["SEI Upper","Mean_all"],Table1["SEI Upper","SE_all"],
                     Table1["SEI Upper","Mean_noPD"],Table1["SEI Upper","SE_noPD"],
                     Table1["SEI Upper","Mean_PD"],Table1["SEI Upper","SE_PD"],
                     "" ))

#Poverty income ratio
Table<-rbind(Table,c("Poverty income ratio, %","","","","","","",Table1["PIR (0, 1]","P.value"]))
Table<-rbind(Table,c("<1.00",
                     Table1["PIR (0, 1]","Mean_all"],Table1["PIR (0, 1]","SE_all"],
                     Table1["PIR (0, 1]","Mean_noPD"],Table1["PIR (0, 1]","SE_noPD"],
                     Table1["PIR (0, 1]","Mean_PD"],Table1["PIR (0, 1]","SE_PD"],
                     "" ))
Table<-rbind(Table,c("1.00-3.99",
                     Table1["PIR (1,4)","Mean_all"],Table1["PIR (1,4)","SE_all"],
                     Table1["PIR (1,4)","Mean_noPD"],Table1["PIR (1,4)","SE_noPD"],
                     Table1["PIR (1,4)","Mean_PD"],Table1["PIR (1,4)","SE_PD"],
                     "" ))
Table<-rbind(Table,c("≥4.00",
                     Table1["PIR [4,inf)","Mean_all"],Table1["PIR [4,inf)","SE_all"],
                     Table1["PIR [4,inf)","Mean_noPD"],Table1["PIR [4,inf)","SE_noPD"],
                     Table1["PIR [4,inf)","Mean_PD"],Table1["PIR [4,inf)","SE_PD"],
                     "" ))
#Health insurance
Table<-rbind(Table,c("Health insurance, %","","","","","","",Table1["Health_insurance No_insurance","P.value"]))
Table<-rbind(Table,c("No insurance",
                     Table1["Health_insurance No_insurance","Mean_all"],Table1["Health_insurance No_insurance","SE_all"],
                     Table1["Health_insurance No_insurance","Mean_noPD"],Table1["Health_insurance No_insurance","SE_noPD"],
                     Table1["Health_insurance No_insurance","Mean_PD"],Table1["Health_insurance No_insurance","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Public insurance only",
                     Table1["Health_insurance Public_insurance","Mean_all"],Table1["Health_insurance Public_insurance","SE_all"],
                     Table1["Health_insurance Public_insurance","Mean_noPD"],Table1["Health_insurance Public_insurance","SE_noPD"],
                     Table1["Health_insurance Public_insurance","Mean_PD"],Table1["Health_insurance Public_insurance","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Private insurance",
                     Table1["Health_insurance Private_insurance","Mean_all"],Table1["Health_insurance Private_insurance","SE_all"],
                     Table1["Health_insurance Private_insurance","Mean_noPD"],Table1["Health_insurance Private_insurance","SE_noPD"],
                     Table1["Health_insurance Private_insurance","Mean_PD"],Table1["Health_insurance Private_insurance","SE_PD"],
                     "" ))
#Education levels
Table<-rbind(Table,c("Education levels, %","","","","","","",Table1["Education_levels Less_than_high_school","P.value"]))
Table<-rbind(Table,c("Less than high school",
                     Table1["Education_levels Less_than_high_school","Mean_all"],Table1["Education_levels Less_than_high_school","SE_all"],
                     Table1["Education_levels Less_than_high_school","Mean_noPD"],Table1["Education_levels Less_than_high_school","SE_noPD"],
                     Table1["Education_levels Less_than_high_school","Mean_PD"],Table1["Education_levels Less_than_high_school","SE_PD"],
                     "" ))
Table<-rbind(Table,c("High school or equivalent",
                     Table1["Education_levels High_school_or_Equivalent","Mean_all"],Table1["Education_levels High_school_or_Equivalent","SE_all"],
                     Table1["Education_levels High_school_or_Equivalent","Mean_noPD"],Table1["Education_levels High_school_or_Equivalent","SE_noPD"],
                     Table1["Education_levels High_school_or_Equivalent","Mean_PD"],Table1["Education_levels High_school_or_Equivalent","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Hispanic",
                     Table1["Race_ethnicity Hispanic","Mean_all"],Table1["Race_ethnicity Hispanic","SE_all"],
                     Table1["Race_ethnicity Hispanic","Mean_noPD"],Table1["Race_ethnicity Hispanic","SE_noPD"],
                     Table1["Race_ethnicity Hispanic","Mean_PD"],Table1["Race_ethnicity Hispanic","SE_PD"],
                     "" ))
#Socioeconomic Status
Table<-rbind(Table,c("Socioeconomic Status, %","","","","","","",Table1["SES low","P.value"]))
Table<-rbind(Table,c("Low",
                     Table1["SES low","Mean_all"],Table1["SES low","SE_all"],
                     Table1["SES low","Mean_noPD"],Table1["SES low","SE_noPD"],
                     Table1["SES low","Mean_PD"],Table1["SES low","SE_PD"],
                     "" ))
Table<-rbind(Table,c("Medium",
                     Table1["SES medium","Mean_all"],Table1["SES medium","SE_all"],
                     Table1["SES medium","Mean_noPD"],Table1["SES medium","SE_noPD"],
                     Table1["SES medium","Mean_PD"],Table1["SES medium","SE_PD"],
                     "" ))
Table<-rbind(Table,c("High",
                     Table1["SES high","Mean_all"],Table1["SES high","SE_all"],
                     Table1["SES high","Mean_noPD"],Table1["SES high","SE_noPD"],
                     Table1["SES high","Mean_PD"],Table1["SES high","SE_PD"],
                     "" ))
Table_S3<-Table
write.table(Table_S3,sep = ",",file ="I:/paper_8_PD&MDD/result/Supplementary Table 3.csv" ,row.names =F,col.names =F )
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 19 All-cause and cause-specific mortality (Table 2) ####  
load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all model1
  model1<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result<-rbind(result,result2)
  #all model3
  model3<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result<-rbind(result,result3)
  result
  #all model4
  model4<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort
                       , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result_All<-rbind(result,result4)
  result_All
}

{ #* CVD model #####
  
  #CVD Crude
  model1<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result<-rbind(result,result2)
  result
  #CVD model2
  model3<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result<-rbind(result,result3)
  result
  #CVD model3
  model4<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort
                       , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result_CVD<-rbind(result,result4)
  result_CVD
  
}

{ #* Cancer model #####
  
  #Cancer model1
  model1<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model2
  model2<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result<-rbind(result,result2)
  result
  #Cancer model3
  model3<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status,design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result<-rbind(result,result3)
  result
  #Cancer model4
  model4<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result_Cancer<-rbind(result,result4)
  result_Cancer
}



{ #* Combine #####
  #Results
  result.cause<-rbind(result_All,result_CVD,result_Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="10000"]<-"1.000"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause
  #Events
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts
  
  #PD_MDD
  PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
  PD_MDD$year<-PD_MDD$peryear
  PD_MDD$Pyear<-PD_MDD$year*PD_MDD$weight
  PD_MDD_death_All<-PD_MDD[which(PD_MDD$MORT_stat==1),]
  PD_MDD_death_CVD<-PD_MDD[which(PD_MDD$CVD_MORT_stat==1),]
  PD_MDD_death_Cancer<-PD_MDD[which(PD_MDD$Cancer_MORT_stat==1),]
  
  #noPD_MDD
  noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
  noPD_MDD$year<-noPD_MDD$peryear
  noPD_MDD$Pyear<-noPD_MDD$year*noPD_MDD$weight
  noPD_MDD_death_All<-noPD_MDD[which(noPD_MDD$MORT_stat==1),]
  noPD_MDD_death_CVD<-noPD_MDD[which(noPD_MDD$CVD_MORT_stat==1),]
  noPD_MDD_death_Cancer<-noPD_MDD[which(noPD_MDD$Cancer_MORT_stat==1),]
  
  #all.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)

  all.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #CVD.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  CVD.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #Cancer.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  Cancer.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  total.counts_ad<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts_ad
  
  #Cbind
  Table<-c("","Periodontal status","","")
  Table<-rbind(Table,c("", "No/Mild periodontitis","Moderate/Severe periodontitis","P"))
  Table<-rbind(Table,c("All cause*","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[1,1],total.counts[1,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[1,1],total.counts_ad[1,2],""))
  Table<-rbind(Table,c("Model 1†","1.00 [Reference]",paste0(result.cause[1,1]," (",result.cause[1,2],", ",result.cause[1,3],")"),result.cause[1,4]))
  Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",paste0(result.cause[2,1]," (",result.cause[2,2],", ",result.cause[2,3],")"),result.cause[2,4]))
  Table<-rbind(Table,c("Model 3§","1.00 [Reference]",paste0(result.cause[3,1]," (",result.cause[3,2],", ",result.cause[3,3],")"),result.cause[3,4]))
  Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",paste0(result.cause[4,1]," (",result.cause[4,2],", ",result.cause[4,3],")"),result.cause[4,4]))
  Table<-rbind(Table,c("CVD cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[2,1],total.counts[2,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[2,1],total.counts_ad[2,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[5,1]," (",result.cause[5,2],", ",result.cause[5,3],")"),result.cause[5,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[6,1]," (",result.cause[6,2],", ",result.cause[6,3],")"),result.cause[6,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[7,1]," (",result.cause[7,2],", ",result.cause[7,3],")"),result.cause[7,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[8,1]," (",result.cause[8,2],", ",result.cause[8,3],")"),result.cause[8,4]))
  Table<-rbind(Table,c("Cancer cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[3,1],total.counts[3,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[3,1],total.counts_ad[3,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[9,1]," (",result.cause[9,2],", ",result.cause[9,3],")"),result.cause[9,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[10,1]," (",result.cause[10,2],", ",result.cause[10,3],")"),result.cause[10,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[11,1]," (",result.cause[11,2],", ",result.cause[11,3],")"),result.cause[11,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[12,1]," (",result.cause[12,2],", ",result.cause[12,3],")"),result.cause[12,4]))
  Table<-as.data.frame(Table)
  Table_2 = data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  Table_2
  write.table(Table_2,sep = ",",file ="I:/paper_8_PD&MDD/result/Table 2.csv",row.names =F,col.names =F )
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 20 Weiboll model (Table 3) ####  
{ #* all model #####
  #all Crude
  model<-svysurvreg(Surv(peryear, MORT_stat==1) ~
                      PD_diagnosis, design =rhcSvy,model=T, x=T,
                    y=T, dist="weibull")
  model_result<-summary(model)
  model_result
  HR<-exp(model_result[["coefficients"]][2])
  lower<-exp(model_result[["coefficients"]][2]-1.96*model_result[["table"]][,2][2])
  upper<-exp(model_result[["coefficients"]][2]+1.96*model_result[["table"]][,2][2])
  P<-model_result[["table"]][,4][2]
  result <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model 2
  model<-svysurvreg(Surv(peryear, MORT_stat==1) ~
                      PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy,dist="weibull")
  model_result<-summary(model)
  model_result
  HR<-exp(model_result[["coefficients"]][2])
  lower<-exp(model_result[["coefficients"]][2]-1.96*model_result[["table"]][,2][2])
  upper<-exp(model_result[["coefficients"]][2]+1.96*model_result[["table"]][,2][2])
  P<-model_result[["table"]][,4][2]
  result2 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model2",'status'="All cause")
  result.all<-rbind(result,result2)
  result.all
  #all model3
  model<-svysurvreg(Surv(peryear, MORT_stat==1) ~
                      PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                      Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy,dist="weibull")
  model_result<-summary(model)
  HR<-exp(model_result[["coefficients"]][2])
  lower<-exp(model_result[["coefficients"]][2]-1.96*model_result[["table"]][,2][2])
  upper<-exp(model_result[["coefficients"]][2]+1.96*model_result[["table"]][,2][2])
  P<-model_result[["table"]][,4][2]
  result3 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
  #all model4
  model<-svysurvreg(Surv(peryear, MORT_stat==1) ~
                      PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                      Smoking_status+Drinking_status+BMI_status+Physical_status+
                      HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy,dist="weibull")
  model_result<-summary(model)
  HR<-exp(model_result[["coefficients"]][2])
  lower<-exp(model_result[["coefficients"]][2]-1.96*model_result[["table"]][,2][2])
  upper<-exp(model_result[["coefficients"]][2]+1.96*model_result[["table"]][,2][2])
  P<-model_result[["table"]][,4][2]
  result4 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model4",'status'="All cause")
  result.All<-rbind(result.all,result4)
  result.All
}

{ #* CVD model #####
  
  #CVD Crude
  model<-svysurvreg(Surv(peryear, CVD_MORT_stat==1) ~
                      PD_diagnosis, design =rhcSvy, dist="weibull")
  model_result<-summary(model)
  model_result
  HR<-exp(model_result[["coefficients"]][2])
  lower<-exp(model_result[["coefficients"]][2]-1.96*model_result[["table"]][,2][2])
  upper<-exp(model_result[["coefficients"]][2]+1.96*model_result[["table"]][,2][2])
  P<-model_result[["table"]][,4][2]
  result <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model<-svysurvreg(Surv(peryear, CVD_MORT_stat==1) ~
                      PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy,dist="weibull")
  model_result<-summary(model)
  model_result
  HR<-exp(model_result[["coefficients"]][2])
  lower<-exp(model_result[["coefficients"]][2]-1.96*model_result[["table"]][,2][2])
  upper<-exp(model_result[["coefficients"]][2]+1.96*model_result[["table"]][,2][2])
  P<-model_result[["table"]][,4][2]
  result2 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result,result2)
  result.CVD
  #CVD model2
  model<-svysurvreg(Surv(peryear, CVD_MORT_stat==1) ~
                      PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                      Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy,dist="weibull")
  model_result<-summary(model)
  HR<-exp(model_result[["coefficients"]][2])
  lower<-exp(model_result[["coefficients"]][2]-1.96*model_result[["table"]][,2][2])
  upper<-exp(model_result[["coefficients"]][2]+1.96*model_result[["table"]][,2][2])
  P<-model_result[["table"]][,4][2]
  result3 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3)
  result.CVD
  #CVD model3
  model<-svysurvreg(Surv(peryear, CVD_MORT_stat==1) ~
                      PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                      Smoking_status+Drinking_status+BMI_status+Physical_status+
                      HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy,dist="weibull")
  model_result<-summary(model)
  HR<-exp(model_result[["coefficients"]][2])
  lower<-exp(model_result[["coefficients"]][2]-1.96*model_result[["table"]][,2][2])
  upper<-exp(model_result[["coefficients"]][2]+1.96*model_result[["table"]][,2][2])
  P<-model_result[["table"]][,4][2]
  result4 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
}

{ #* Cancer model #####
  
  #Cancer Crude
  model<-svysurvreg(Surv(peryear, Cancer_MORT_stat==1) ~
                      PD_diagnosis+Age_status, design =rhcSvy, dist="weibull")
  model_result<-summary(model)
  model_result
  HR<-exp(model_result[["coefficients"]][2])
  lower<-exp(model_result[["coefficients"]][2]-1.96*model_result[["table"]][,2][2])
  upper<-exp(model_result[["coefficients"]][2]+1.96*model_result[["table"]][,2][2])
  P<-model_result[["table"]][,4][2]
  result <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model1
  model<-svysurvreg(Surv(peryear, Cancer_MORT_stat==1) ~
                      PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy,dist="weibull")
  model_result<-summary(model)
  model_result
  HR<-exp(model_result[["coefficients"]][2])
  lower<-exp(model_result[["coefficients"]][2]-1.96*model_result[["table"]][,2][2])
  upper<-exp(model_result[["coefficients"]][2]+1.96*model_result[["table"]][,2][2])
  P<-model_result[["table"]][,4][2]
  result2 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result,result2)
  result.Cancer
  #Cancer model2
  model<-svysurvreg(Surv(peryear, Cancer_MORT_stat==1) ~
                      PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                      Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy,dist="weibull")
  model_result<-summary(model)
  HR<-exp(model_result[["coefficients"]][2])
  lower<-exp(model_result[["coefficients"]][2]-1.96*model_result[["table"]][,2][2])
  upper<-exp(model_result[["coefficients"]][2]+1.96*model_result[["table"]][,2][2])
  P<-model_result[["table"]][,4][2]
  result3 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3)
  result.Cancer
  #Cancer model3
  model<-svysurvreg(Surv(peryear, Cancer_MORT_stat==1) ~
                      PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                      Smoking_status+Drinking_status+BMI_status+Physical_status+
                      HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy,dist="weibull")
  model_result<-summary(model)
  HR<-exp(model_result[["coefficients"]][2])
  lower<-exp(model_result[["coefficients"]][2]-1.96*model_result[["table"]][,2][2])
  upper<-exp(model_result[["coefficients"]][2]+1.96*model_result[["table"]][,2][2])
  P<-model_result[["table"]][,4][2]
  result4 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}


{ #* Combine #####
  #Results
  result.cause<-rbind(result.All,result.CVD,result.Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="10000"]<-"1.000"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause
  #Events
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts
  
  #PD_MDD
  PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
  PD_MDD$year<-PD_MDD$peryear
  PD_MDD$Pyear<-PD_MDD$year*PD_MDD$weight
  PD_MDD_death_All<-PD_MDD[which(PD_MDD$MORT_stat==1),]
  PD_MDD_death_CVD<-PD_MDD[which(PD_MDD$CVD_MORT_stat==1),]
  PD_MDD_death_Cancer<-PD_MDD[which(PD_MDD$Cancer_MORT_stat==1),]
  
  #noPD_MDD
  noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
  noPD_MDD$year<-noPD_MDD$peryear
  noPD_MDD$Pyear<-noPD_MDD$year*noPD_MDD$weight
  noPD_MDD_death_All<-noPD_MDD[which(noPD_MDD$MORT_stat==1),]
  noPD_MDD_death_CVD<-noPD_MDD[which(noPD_MDD$CVD_MORT_stat==1),]
  noPD_MDD_death_Cancer<-noPD_MDD[which(noPD_MDD$Cancer_MORT_stat==1),]
  
  #all.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  
  all.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #CVD.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  CVD.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #Cancer.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  Cancer.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  total.counts_ad<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts_ad
  
  #Cbind
  Table_01<-c("","Periodontal status","","")
  Table_02<-c("", "No/Mild periodontitis","Moderate/Severe periodontitis","P")
  Table_03<-c("All cause*","","","")
  Table_04<-c("Deaths/total (Unweighted)",total.counts[1,1],total.counts[1,2],"")
  Table_05<-c("Deaths/total (Weighted)",total.counts_ad[1,1],total.counts_ad[1,2],"")
  Table_06<-c("Model 1†","1.00 [Reference]",paste0(result.cause[1,1]," (",result.cause[1,2],", ",result.cause[1,3],")"),result.cause[1,4])
  Table_07<-c("Model 2‡","1.00 [Reference]",paste0(result.cause[2,1]," (",result.cause[2,2],", ",result.cause[2,3],")"),result.cause[2,4])
  Table_08<-c("Model 3§","1.00 [Reference]",paste0(result.cause[3,1]," (",result.cause[3,2],", ",result.cause[3,3],")"),result.cause[3,4])
  Table_09<-c("Model 4¶","1.00 [Reference]",paste0(result.cause[4,1]," (",result.cause[4,2],", ",result.cause[4,3],")"),result.cause[4,4])
  Table_10<-c("CVD cause","","","")
  Table_11<-c("Deaths/total (Unweighted)",total.counts[2,1],total.counts[2,2],"")
  Table_12<-c("Deaths/total (Weighted)",total.counts_ad[2,1],total.counts_ad[2,2],"")
  Table_13<-c("Model 1","1.00 [Reference]",paste0(result.cause[5,1]," (",result.cause[5,2],", ",result.cause[5,3],")"),result.cause[5,4])
  Table_14<-c("Model 2","1.00 [Reference]",paste0(result.cause[6,1]," (",result.cause[6,2],", ",result.cause[6,3],")"),result.cause[6,4])
  Table_15<-c("Model 3","1.00 [Reference]",paste0(result.cause[7,1]," (",result.cause[7,2],", ",result.cause[7,3],")"),result.cause[7,4])
  Table_16<-c("Model 4","1.00 [Reference]",paste0(result.cause[8,1]," (",result.cause[8,2],", ",result.cause[8,3],")"),result.cause[8,4])
  Table_17<-c("Cancer cause","","","")
  Table_18<-c("Deaths/total (Unweighted)",total.counts[3,1],total.counts[3,2],"")
  Table_19<-c("Deaths/total (Weighted)",total.counts_ad[3,1],total.counts_ad[3,2],"")
  Table_20<-c("Model 1","1.00 [Reference]",paste0(result.cause[9,1]," (",result.cause[9,2],", ",result.cause[9,3],")"),result.cause[9,4])
  Table_21<-c("Model 2","1.00 [Reference]",paste0(result.cause[10,1]," (",result.cause[10,2],", ",result.cause[10,3],")"),result.cause[10,4])
  Table_22<-c("Model 3","1.00 [Reference]",paste0(result.cause[11,1]," (",result.cause[11,2],", ",result.cause[11,3],")"),result.cause[11,4])
  Table_23<-c("Model 4","1.00 [Reference]",paste0(result.cause[12,1]," (",result.cause[12,2],", ",result.cause[12,3],")"),result.cause[12,4])
  Table_3<-as.data.frame(rbind(Table_01,Table_02,Table_03,Table_04,Table_05,
                               Table_06,Table_07,Table_08,Table_09,Table_10,
                               Table_11,Table_12,Table_13,Table_14,Table_15,
                               Table_16,Table_17,Table_18,Table_19,Table_20,
                               Table_21,Table_22,Table_23))
  Table_3
  Table_3 = data.frame(lapply(Table_3, as.character), stringsAsFactors=FALSE)
  write.table(Table_3,sep = ",",file ="I:/paper_8_PD&MDD/result/Table 3.csv",row.names =F,col.names =F )
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 21 dose -response effect (Table 4) #### 
load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
table(Interpolation_weighted$Periodontitis_diagnosis)
Interpolation_weighted$Periodontitis_diagnosis<-as.character(Interpolation_weighted$Periodontitis_diagnosis)
Interpolation_weighted$Periodontitis[
  Interpolation_weighted$Periodontitis_diagnosis=="normal"|Interpolation_weighted$Periodontitis_diagnosis=="mild"
]<-"normal/mild"
table(Interpolation_weighted$Periodontitis)
Interpolation_weighted$Periodontitis<-factor(Interpolation_weighted$Periodontitis,
                                             levels = c("normal/mild","moderate","severe"))
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         Periodontitis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1:2,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1:2,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-result3
}

{ #* CVD model #####
  
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         Periodontitis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1:2,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1:2,c("exp(coef)","lower .95","upper .95")]
  result3.CVD <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-result3.CVD
  
}

{ #* Cancer model #####
  
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            Periodontitis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status+
                            HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1:2,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1:2,c("exp(coef)","lower .95","upper .95")]
  result3.Cancer <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-result3.Cancer
}


{ #* Combine #####
  result.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  Table4_1<-result.cause
  Table4_1$HR_CI<-paste0(Table4_1$HR," (",Table4_1$lower..95,", ",Table4_1$upper..95,")")
  Table4_1
}
Interpolation_weighted$Periodontitis<-as.character(Interpolation_weighted$Periodontitis)
table(Interpolation_weighted$Periodontitis)
Interpolation_weighted$Periodontitis[Interpolation_weighted$Periodontitis=="normal/mild"]<-0
Interpolation_weighted$Periodontitis[Interpolation_weighted$Periodontitis=="moderate"]<-1
Interpolation_weighted$Periodontitis[Interpolation_weighted$Periodontitis=="severe"]<-2
Interpolation_weighted$Periodontitis<-as.numeric(Interpolation_weighted$Periodontitis)

{#* P for trend ####
  rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
  { #* all model #####
    model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                           Periodontitis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                           Smoking_status+Drinking_status+BMI_status+Physical_status+
                           HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
    model3_all_result<-summary(model3_all)
    
    P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model3",'status'="All cause")
    result.all<-result3
    result.all
  }
  
  { #* CVD model #####
    
    model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                           Periodontitis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                           Smoking_status+Drinking_status+BMI_status+Physical_status+
                           HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
    model3_CVD_result<-summary(model3_CVD)
    P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model3",'status'="CVD cause")
    result.CVD<-result3.CVD
    result.CVD
    
  }
  
  { #* Cancer model #####
    
    #Cancer model2
    model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                              Periodontitis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                              Smoking_status+Drinking_status+BMI_status+Physical_status+
                              HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
    model3_Cancer_result<-summary(model3_Cancer)
    P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                                 'P value' =P,'model'="model3",'status'="Cancer cause")
    result.Cancer<-result3.Cancer
    result.Cancer
  }
  
  
  
  { #* Combine #####
    result.cause<-rbind(result.all,result.CVD,result.Cancer)
    result.cause$HR<-round(result.cause$HR,2)
    result.cause$lower..95<-round(result.cause$lower..95,2)
    result.cause$upper..95<-round(result.cause$upper..95,2)
    result.cause$P.value<-round(result.cause$P.value,3)
    round_3_function <- function(x){
      while(nchar(x)<5){
        temp <- paste(x,0)
        x <- temp
        x <- gsub(" ","",x)
      }
      return(x)
    }
    round_2_function <- function(x){
      while(nchar(x)<4){
        temp <- paste(x,0)
        x <- temp
        x <- gsub(" ","",x)
      }
      return(x)
    }
    
    result.cause$HR<-lapply(result.cause$HR,round_2_function)
    result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
    result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
    result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
    Table4_2<-result.cause
    Table4_2$HR_CI<-paste0(Table4_2$HR," (",Table4_2$lower..95,", ",Table4_2$upper..95,")")
    Table4_2
  }
}
{ #* section 21.3 Combine  Table 4#####
  #Events
  #cause
  PD.counts<-table(Interpolation_weighted$Periodontitis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$Periodontitis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),
               paste0(PD.M.counts[2,2],"/",PD.counts[2]),
               paste0(PD.M.counts[3,2],"/",PD.counts[3]))
  #CVD.cause
  PD.M.counts<-table(Interpolation_weighted$Periodontitis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),
               paste0(PD.M.counts[2,2],"/",PD.counts[2]),
               paste0(PD.M.counts[3,2],"/",PD.counts[3]))
  #Cancer.cause
  PD.M.counts<-table(Interpolation_weighted$Periodontitis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),
                  paste0(PD.M.counts[2,2],"/",PD.counts[2]),
                  paste0(PD.M.counts[3,2],"/",PD.counts[3]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts
  #"normal/mild","moderate","severe"
  #PD_MDD
  PDS_MDD<-Interpolation_weighted[which(Interpolation_weighted$Periodontitis==2),]
  table(Interpolation_weighted$Periodontitis)
  PDS_MDD_death_All<-PDS_MDD[which(PDS_MDD$MORT_stat==1),]
  PDS_MDD_death_CVD<-PDS_MDD[which(PDS_MDD$CVD_MORT_stat==1),]
  PDS_MDD_death_Cancer<-PDS_MDD[which(PDS_MDD$Cancer_MORT_stat==1),]
  #PD_MDD
  PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$Periodontitis==1),]
  PD_MDD_death_All<-PD_MDD[which(PD_MDD$MORT_stat==1),]
  PD_MDD_death_CVD<-PD_MDD[which(PD_MDD$CVD_MORT_stat==1),]
  PD_MDD_death_Cancer<-PD_MDD[which(PD_MDD$Cancer_MORT_stat==1),]
  
  #noPD_MDD
  noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$Periodontitis==0),]
  noPD_MDD_death_All<-noPD_MDD[which(noPD_MDD$MORT_stat==1),]
  noPD_MDD_death_CVD<-noPD_MDD[which(noPD_MDD$CVD_MORT_stat==1),]
  noPD_MDD_death_Cancer<-noPD_MDD[which(noPD_MDD$Cancer_MORT_stat==1),]
  
  #all.cause
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  PDS.counts<-format(round(sum(PDS_MDD$weight)), big.mark = ",", scientific = FALSE)
  PDS.cause.counts<-format(round(sum(PDS_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  all.cause<-c(paste0(noPD.cause.counts,"/",noPD.counts),
               paste0(PD.cause.counts,"/",PD.counts),
               paste0(PDS.cause.counts,"/",PDS.counts))
  all.cause
  #CVD.cause
  
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  PDS.counts<-format(round(sum(PDS_MDD$weight)), big.mark = ",", scientific = FALSE)
  PDS.cause.counts<-format(round(sum(PDS_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  CVD.cause<-c(paste0(noPD.cause.counts,"/",noPD.counts),
               paste0(PD.cause.counts,"/",PD.counts),
               paste0(PDS.cause.counts,"/",PDS.counts))
  #Cancer.cause
  
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  PDS.counts<-format(round(sum(PDS_MDD$weight)), big.mark = ",", scientific = FALSE)
  PDS.cause.counts<-format(round(sum(PDS_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  Cancer.cause<-c(paste0(noPD.cause.counts,"/",noPD.counts),
                  paste0(PD.cause.counts,"/",PD.counts),
                  paste0(PDS.cause.counts,"/",PDS.counts))
  
  total.counts_ad<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts_ad
  
  #Cbind
  Table<-c("Outcomes","Events (Weighted), n/N","HR (95% CI)","P")
  Table<-rbind(Table,c("All cause","","",""))
  Table<-rbind(Table,c("No/Mild periodontitis",total.counts_ad[1,1],"1.00 [Reference]",""))
  Table<-rbind(Table,c("Moderate periodontitis*",total.counts_ad[1,2],Table4_1[1,"HR_CI"],Table4_1[1,"P.value"]))
  Table<-rbind(Table,c("Severe periodontitis",total.counts_ad[1,3],Table4_1[2,"HR_CI"],Table4_1[2,"P.value"]))
  Table<-rbind(Table,c("P for trend","",Table4_2[1,"HR_CI"],Table4_2[1,"P.value"]))
  Table<-rbind(Table,c("CVD cause*","","",""))
  Table<-rbind(Table,c("No/Mild periodontitis",total.counts_ad[2,1],"1.00 [Reference]",""))
  Table<-rbind(Table,c("Moderate periodontitis",total.counts_ad[2,2],Table4_1[3,"HR_CI"],Table4_1[3,"P.value"]))
  Table<-rbind(Table,c("Severe periodontitis",total.counts_ad[2,3],Table4_1[4,"HR_CI"],Table4_1[4,"P.value"]))
  Table<-rbind(Table,c("P for trend","",Table4_2[2,"HR_CI"],Table4_2[2,"P.value"]))
  Table<-rbind(Table,c("Cancer cause*","","",""))
  Table<-rbind(Table,c("No/Mild periodontitis",total.counts_ad[3,1],"1.00 [Reference]",""))
  Table<-rbind(Table,c("Moderate periodontitis",total.counts_ad[3,2],Table4_1[5,"HR_CI"],Table4_1[5,"P.value"]))
  Table<-rbind(Table,c("Severe periodontitis",total.counts_ad[3,3],Table4_1[6,"HR_CI"],Table4_1[6,"P.value"]))
  Table<-rbind(Table,c("P for trend","",Table4_2[3,"HR_CI"],Table4_2[3,"P.value"]))
  Table
  Table_4<-as.data.frame(Table)
  Table_4<-data.frame(lapply(Table_4, as.character), stringsAsFactors=FALSE)
  write.table(Table_4,sep = ",",file ="I:/paper_8_PD&MDD/result/Table 4.csv" ,row.names =F,col.names =F )
  
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# +++++++++++============Figures==========+++++++++++ ####  
# >>>>> section 22 Figure 1 ####  
{ #* ALL Mortality Figure 1A ####
  #install.packages("ggthemes")
  Interpolation_weighted$PD<-Interpolation_weighted$PD_diagnosis
  FIT_ALL_PD<-survfit(Surv(peryear, MORT_stat==1) ~ PD, Interpolation_weighted)
  Figure_1A<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), palette=c("#0073C2E5", "#EFC000E5"), 
                        ncensor.plot=F,risk.table = T, 
                        break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
                        xlab ="Time in Year",  pval = F, legend.title = "",ylim = c(0,0.4),ggtheme =  theme_calc())
  print(Figure_1A)
  topptx(filename = "I:/paper_8_PD&MDD/result/Figure_1A1.pptx", width = 8, height = 6)
  Figure_1A<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, tables.theme = theme_cleantable(), palette=c("#0073C2E5", "#EFC000E5"), 
                        ncensor.plot=F,risk.table = T,
                        break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
                        xlab ="Time in Year", pval = F, legend.title = "",ggtheme = theme(panel.background = element_rect(fill="white"),
                                                                                         #panel.grid.major.y = element_line(color="black",size = 0.5),
                                                                                         panel.border = element_rect(fill=NA,color="black", size=0.7)))
  print(Figure_1A)
  topptx(filename = "I:/paper_8_PD&MDD/result/Figure_1A2.pptx", width = 8, height = 6)
  
} 
{ #* ALL Mortality Figure 1B ####

  FIT_ALL_PD<-survfit(Surv(peryear, CVD_MORT_stat==1) ~ PD, Interpolation_weighted)
  Figure_1B<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), palette=c("#0073C2E5", "#EFC000E5"), 
                        ncensor.plot=F,risk.table = T, 
                        break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
                        xlab ="Time in Year",  pval = F, legend.title = "",ylim = c(0,0.2),ggtheme =  theme_calc())
  print(Figure_1B)
  topptx(filename = "I:/paper_8_PD&MDD/result/Figure_1B.pptx", width = 8, height = 6)


} 
{ #* ALL Mortality Figure 1C ####

  FIT_ALL_PD<-survfit(Surv(peryear, Cancer_MORT_stat==1) ~ PD, Interpolation_weighted)
  Figure_1C<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), palette=c("#0073C2E5", "#EFC000E5"), 
                        ncensor.plot=F,risk.table = T, 
                        break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
                        xlab ="Time in Year",  pval = F, legend.title = "",ylim = c(0,0.2),ggtheme =  theme_calc())
  print(Figure_1C)
  topptx(filename = "I:/paper_8_PD&MDD/result/Figure_1C.pptx", width = 8, height = 6)
} 
{ #* CIF Figure 2D ##### 
  
  load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
  Interpolation_weighted$ALL_MORT_stat[Interpolation_weighted$MORT_stat==0]<-"0"
  Interpolation_weighted$ALL_MORT_stat[Interpolation_weighted$MORT_stat==1]<-"MORT"
  Interpolation_weighted$ALL_MORT_stat[Interpolation_weighted$CVD_MORT_stat==1]<-"CVD"
  Interpolation_weighted$ALL_MORT_stat[Interpolation_weighted$Cancer_MORT_stat==1]<-"Cancer"
  CIF<-cuminc(Interpolation_weighted$peryear,Interpolation_weighted$ALL_MORT_stat,Interpolation_weighted$PD_diagnosis,0,strata=Interpolation_weighted$sdmvstra,rho =Interpolation_weighted$weight) 
  #CIF<-cuminc(ftime=time,fstatus=cause,group=stage,cencode=0)
  
  CIF #print(CIF)
  NM_periodontitis_CVD<- as.data.frame(CIF[["No/Mild periodontitis 1"]])
  NM_periodontitis_CVD$type<-"No/Mild periodontitis 1"
  NM_periodontitis_CVD$group<-"CVD"
  NM_periodontitis_CVD$line<-"No/Mild periodontitis"
  MS_periodontitis_CVD<-as.data.frame(CIF[["Moderate/Severe periodontitis 1"]])
  MS_periodontitis_CVD$type<-"Moderate/Severe periodontitis 1"
  MS_periodontitis_CVD$group<-"CVD"
  MS_periodontitis_CVD$line<-"Moderate/Severe periodontitis"
  NM_periodontitis_Cancer<-as.data.frame(CIF[["No/Mild periodontitis 2"]])
  NM_periodontitis_Cancer$type<-"No/Mild periodontitis 2"
  NM_periodontitis_Cancer$group<-"Cancer"
  NM_periodontitis_Cancer$line<-"No/Mild periodontitis"
  MS_periodontitis_Cancer<-as.data.frame(CIF[["Moderate/Severe periodontitis 2"]])
  MS_periodontitis_Cancer$type<-"Moderate/Severe periodontitis 2"
  MS_periodontitis_Cancer$group<-"Cancer"
  MS_periodontitis_Cancer$line<-"Moderate/Severe periodontitis"
  NM_periodontitis_Other<-as.data.frame(CIF[["No/Mild periodontitis 3"]])
  NM_periodontitis_Other$type<-"No/Mild periodontitis 3"
  NM_periodontitis_Other$group<-"Others"
  NM_periodontitis_Other$line<-"No/Mild periodontitis"
  MS_periodontitis_Other<-as.data.frame(CIF[["Moderate/Severe periodontitis 3"]])
  MS_periodontitis_Other$type<-"Moderate/Severe periodontitis 3"
  MS_periodontitis_Other$line<-"Moderate/Severe periodontitis"
  MS_periodontitis_Other$group<-"Others"
  data<-rbind(NM_periodontitis_CVD,MS_periodontitis_CVD,NM_periodontitis_Cancer,MS_periodontitis_Cancer,
              NM_periodontitis_Other,MS_periodontitis_Other)
  #rename
  Figure_1D<- ggplot(data, aes(x=time, y=est, group=type,colour=group)) + geom_line(aes(linetype=line), size=1)+ theme_bw(base_size = 12)+
    
    theme(panel.background = element_rect(fill="white"),
          panel.grid.major.y = element_line(color="black",size = 0.5),
          panel.border = element_rect(fill=NA,color="black", size=0.7),
          axis.text = element_text(size=15),
          axis.title=element_text(size=15),
          legend.text=element_text(size=15),
          panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
    scale_color_manual(values=c("#0073C2FF", "#EFC000FF", "#CD534CFF","#8686864C"))+ 
    ylab("Cumulative incidence function") + xlab("Time in year") 
  Figure_1D
  ggsave("I:/paper_8_PD&MDD/result/Figure_1D.pdf",Figure_2D,device = "pdf", width = 12, height = 6, units ="in",
         dpi = 600, limitsize = TRUE)
}
# +++++++++++================================+++++++++++ ####
# +++++++++++========Supplementary===========+++++++++++ ####
# +++++++++++================================+++++++++++ #### 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# +++++++++++++++++++Tables++++++++++++++++++++++++++ ####
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 22 Relative mortality rates (Table S4) ####  
load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
#PD_MDD
PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
PD_MDD$year<-PD_MDD$peryear
PD_MDD$Pyear<-PD_MDD$year*PD_MDD$weight
PD_MDD_death_All<-PD_MDD[which(PD_MDD$MORT_stat==1),]
PD_MDD_death_CVD<-PD_MDD[which(PD_MDD$CVD_MORT_stat==1),]
PD_MDD_death_Cancer<-PD_MDD[which(PD_MDD$Cancer_MORT_stat==1),]
PD_MDD_Perseon_year_ad<-sum(PD_MDD$Pyear)
PD_MDD_Perseon<-sum(PD_MDD$weight)
PD_MDD_Perseon_ad_All<-as.numeric(round(sum(PD_MDD_death_All$weight)))
PD_MDD_Perseon_ad_CVD<-as.numeric(round(sum(PD_MDD_death_CVD$weight)))
PD_MDD_Perseon_ad_Cancer<-as.numeric(round(sum(PD_MDD_death_Cancer$weight)))

#noPD_MDD
noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
noPD_MDD$year<-noPD_MDD$peryear
noPD_MDD$Pyear<-noPD_MDD$year*noPD_MDD$weight
noPD_MDD_death_All<-noPD_MDD[which(noPD_MDD$MORT_stat==1),]
noPD_MDD_death_CVD<-noPD_MDD[which(noPD_MDD$CVD_MORT_stat==1),]
noPD_MDD_death_Cancer<-noPD_MDD[which(noPD_MDD$Cancer_MORT_stat==1),]
noPD_MDD_Perseon_year_ad<-sum(noPD_MDD$Pyear)
noPD_MDD_Perseon<-sum(noPD_MDD$weight)
noPD_MDD_Perseon_ad_All<-as.numeric(round(sum(noPD_MDD_death_All$weight)))
noPD_MDD_Perseon_ad_CVD<-as.numeric(round(sum(noPD_MDD_death_CVD$weight)))
noPD_MDD_Perseon_ad_Cancer<-as.numeric(round(sum(noPD_MDD_death_Cancer$weight)))

#PD_All
PD_All<-PD_MDD_Perseon_ad_All*(1000/PD_MDD_Perseon_year_ad)
PD_All_UCL<-(PD_MDD_Perseon_ad_All+(1.96*sqrt(PD_MDD_Perseon_ad_All)))*(1000/PD_MDD_Perseon_year_ad)
PD_All_LCL<-(PD_MDD_Perseon_ad_All-(1.96*sqrt(PD_MDD_Perseon_ad_All)))*(1000/PD_MDD_Perseon_year_ad)
PD_All_Incidence<-paste0(round(PD_All,2)," (",round(PD_All_LCL,2),"-",round(PD_All_UCL,2),")")
PD_All_Incidence
#PD_CVD
PD_CVD<-PD_MDD_Perseon_ad_CVD*(1000/PD_MDD_Perseon_year_ad)
PD_CVD_UCL<-(PD_MDD_Perseon_ad_CVD+(1.96*sqrt(PD_MDD_Perseon_ad_CVD)))*(1000/PD_MDD_Perseon_year_ad)
PD_CVD_LCL<-(PD_MDD_Perseon_ad_CVD-(1.96*sqrt(PD_MDD_Perseon_ad_CVD)))*(1000/PD_MDD_Perseon_year_ad)
PD_CVD_Incidence<-paste0(round(PD_CVD,2)," (",round(PD_CVD_LCL,2),"-",round(PD_CVD_UCL,2),")")
PD_CVD_Incidence
#PD_Cancer
PD_Cancer<-PD_MDD_Perseon_ad_Cancer*(1000/PD_MDD_Perseon_year_ad)
PD_Cancer_UCL<-(PD_MDD_Perseon_ad_Cancer+(1.96*sqrt(PD_MDD_Perseon_ad_Cancer)))*(1000/PD_MDD_Perseon_year_ad)
PD_Cancer_LCL<-(PD_MDD_Perseon_ad_Cancer-(1.96*sqrt(PD_MDD_Perseon_ad_Cancer)))*(1000/PD_MDD_Perseon_year_ad)
PD_Cancer_Incidence<-paste0(round(PD_Cancer,2)," (",round(PD_Cancer_LCL,2),"-",round(PD_Cancer_UCL,2),")")
PD_Cancer_Incidence

#noPD_All
noPD_All<-noPD_MDD_Perseon_ad_All*(1000/noPD_MDD_Perseon_year_ad)
noPD_All_UCL<-(noPD_MDD_Perseon_ad_All+(1.96*sqrt(noPD_MDD_Perseon_ad_All)))*(1000/noPD_MDD_Perseon_year_ad)
noPD_All_LCL<-(noPD_MDD_Perseon_ad_All-(1.96*sqrt(noPD_MDD_Perseon_ad_All)))*(1000/noPD_MDD_Perseon_year_ad)
noPD_All_Incidence<-paste0(round(noPD_All,2)," (",round(noPD_All_LCL,2),"-",round(noPD_All_UCL,2),")")

#noPD_CVD
noPD_CVD<-noPD_MDD_Perseon_ad_CVD*(1000/noPD_MDD_Perseon_year_ad)
noPD_CVD_UCL<-(noPD_MDD_Perseon_ad_CVD+(1.96*sqrt(noPD_MDD_Perseon_ad_CVD)))*(1000/noPD_MDD_Perseon_year_ad)
noPD_CVD_LCL<-(noPD_MDD_Perseon_ad_CVD-(1.96*sqrt(noPD_MDD_Perseon_ad_CVD)))*(1000/noPD_MDD_Perseon_year_ad)
noPD_CVD_Incidence<-paste0(round(noPD_CVD,2)," (",round(noPD_CVD_LCL,2),"-",round(noPD_CVD_UCL,2),")")
#noPD_Cancer
noPD_Cancer<-noPD_MDD_Perseon_ad_Cancer*(1000/noPD_MDD_Perseon_year_ad)
noPD_Cancer_UCL<-(noPD_MDD_Perseon_ad_Cancer+(1.96*sqrt(noPD_MDD_Perseon_ad_Cancer)))*(1000/noPD_MDD_Perseon_year_ad)
noPD_Cancer_LCL<-(noPD_MDD_Perseon_ad_Cancer-(1.96*sqrt(noPD_MDD_Perseon_ad_Cancer)))*(1000/noPD_MDD_Perseon_year_ad)
noPD_Cancer_Incidence<-paste0(round(noPD_Cancer,2)," (",round(noPD_Cancer_LCL,2),"-",round(noPD_Cancer_UCL,2),")")

#EVENTS
#all.cause
PD.counts<-round(sum(PD_MDD$weight))
PD.cause.counts<-round(sum(PD_MDD_death_All$weight))
noPD.counts<-round(sum(noPD_MDD$weight))
noPD.cause.counts<-round(sum(noPD_MDD_death_All$weight))
all.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
#CVD.cause
PD.counts<-round(sum(PD_MDD$weight))
PD.cause.counts<-round(sum(PD_MDD_death_CVD$weight))
noPD.counts<-round(sum(noPD_MDD$weight))
noPD.cause.counts<-round(sum(noPD_MDD_death_CVD$weight))
CVD.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
#Cancer.cause
PD.counts<-round(sum(PD_MDD$weight))
PD.cause.counts<-round(sum(PD_MDD_death_Cancer$weight))
noPD.counts<-round(sum(noPD_MDD$weight))
noPD.cause.counts<-round(sum(noPD_MDD_death_Cancer$weight))
Cancer.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))

total.counts_ad<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))

TableS1_1<-c("Outcome",NA,"No/Mild periodontitis",NA,"Moderate/Severe periodontitis")
TableS1_2<-c(NA,"Events, n/N","Incidence Rate (95% CI)","Events, n/N (Weighted)", "Incidence Rate (95% CI)")
TableS1_3<-c("All-cause mortality",total.counts_ad[1,1],noPD_All_Incidence,total.counts_ad[1,2],PD_All_Incidence)
TableS1_4<-c("CVD mortality",total.counts_ad[2,1],noPD_CVD_Incidence,total.counts_ad[2,2],PD_CVD_Incidence)
TableS1_5<-c("Cancer mortality",total.counts_ad[3,1],noPD_Cancer_Incidence,total.counts_ad[3,2],PD_Cancer_Incidence)
Table_S4<-as.data.frame(rbind(TableS1_1,TableS1_2,TableS1_3,TableS1_4,TableS1_5))
Table_S4
write.table(Table_S4,sep = ",",file ="I:/paper_8_PD&MDD/result/Supplementary Table 4.csv",row.names =F,col.names =F )
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 23 Absolute risk differences (Table S5) ####  

load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
## import packages

time <-c(5,10)
cl <- makeCluster(3)
registerDoParallel(cl)
Interpolation_weighted$MORT_stat<-as.numeric(Interpolation_weighted$MORT_stat)
Interpolation_weighted$PD<-NULL
Interpolation_weighted$PD_diagnosis<-as.character(Interpolation_weighted$PD_diagnosis)
table(Interpolation_weighted$PD_diagnosis)
Interpolation_weighted$PD[Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"]<-0
Interpolation_weighted$PD[Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"]<-1

table(Interpolation_weighted$PD)
colnames(Interpolation_weighted)
data<-Interpolation_weighted[,c('peryear','MORT_stat','Cancer_MORT_stat','PD','Age_status','Sex','Marital_status','Race_ethnicity','SES',
                                'Smoking_status','Drinking_status','HEI','Physical_status','BMI_status','HTN_status','T2D_status','HPL_status','Cohort',
                                'sdmvpsu','sdmvstra','weight')]
data<-subset(data,Cancer_MORT_stat!=2)

data.treated <- data
data.treated[ ,'PD'] <- 1
data.untreated <- data
data.untreated[ , 'PD'] <- 0
#MODEL0
cox.res <- cph(Surv(peryear,Cancer_MORT_stat)~PD+
                 sdmvpsu+sdmvstra,data=data, weights=weight,surv = T, x = T, y = T)
treated.sur <- survest(cox.res, newdata = data.treated, times = time) 
untreated.sur <- survest(cox.res, newdata = data.untreated, times = time)
c <- colMeans(1 - treated.sur$surv) 
treated.Mean <- colMeans(1 - treated.sur$surv)
treated.SE<-colMeans(treated.sur$std.err) 
treated.SD<-treated.SE*sqrt(length(treated.sur$surv)/2)
untreated.Mean <- colMeans(1 - untreated.sur$surv)
untreated.SE<-colMeans(untreated.sur$std.err) 
untreated.SD<-untreated.SE*sqrt(length(untreated.sur$surv)/2)
RD.Mean <- treated.Mean - untreated.Mean
RD.SD=sqrt((untreated.SD)^2+(untreated.SD)^2)
RD.SE=RD.SD/(sqrt(length(treated.sur$surv)/2))
untreated.LCI=untreated.Mean-1.96*untreated.SE
untreated.UCI=untreated.Mean+1.96*untreated.SE
treated.LCI=treated.Mean-1.96*treated.SE
treated.UCI=treated.Mean+1.96*treated.SE
RD.LCI=RD.Mean-1.96*RD.SE
RD.UCI=RD.Mean+1.96*RD.SE
RD_ALL_MODEL0<-round(as.data.frame(cbind(untreated.Mean,untreated.LCI,untreated.UCI,
                                         treated.Mean,treated.LCI,treated.UCI,
                                         RD.Mean,RD.LCI,RD.UCI)),5)
RD_ALL_MODEL0$Period<-c("5 years","10 years")
RD_ALL_MODEL0$Model<-"Model 0"


#MODEL1
cox.res <- cph(Surv(peryear,Cancer_MORT_stat)~PD+
                 sdmvpsu+sdmvstra+Age_status+Sex+Race_ethnicity+SES
               ,data=data, weights=weight,surv = T, x = T, y = T)
treated.sur <- survest(cox.res, newdata = data.treated, times = time) 
untreated.sur <- survest(cox.res, newdata = data.untreated, times = time)
treated.Mean <- colMeans(1 - treated.sur$surv) 
treated.SE<-colMeans(treated.sur$std.err) 
treated.SD<-treated.SE*sqrt(length(treated.sur$surv)/2)
untreated.Mean <- colMeans(1 - untreated.sur$surv)
untreated.SE<-colMeans(untreated.sur$std.err) 
untreated.SD<-untreated.SE*sqrt(length(untreated.sur$surv)/2)
RD.Mean <- treated.Mean - untreated.Mean
RD.SD=sqrt((untreated.SD)^2+(untreated.SD)^2)
RD.SE=RD.SD/(sqrt(length(treated.sur$surv)/2))
untreated.LCI=untreated.Mean-1.96*untreated.SE
untreated.UCI=untreated.Mean+1.96*untreated.SE
treated.LCI=treated.Mean-1.96*treated.SE
treated.UCI=treated.Mean+1.96*treated.SE
RD.LCI=RD.Mean-1.96*RD.SE
RD.UCI=RD.Mean+1.96*RD.SE
RD_ALL_MODEL1<-round(as.data.frame(cbind(untreated.Mean,untreated.LCI,untreated.UCI,
                                         treated.Mean,treated.LCI,treated.UCI,
                                         RD.Mean,RD.LCI,RD.UCI)),5)
RD_ALL_MODEL1$Period<-c("5 years","10 years")
RD_ALL_MODEL1$Model<-"Model 1"

#MODEL2
cox.res <- cph(Surv(peryear,Cancer_MORT_stat)~PD+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                 Smoking_status+Drinking_status+BMI_status+Physical_status+
                sdmvpsu+sdmvstra,data=data, weights=weight,surv = T, x = T, y = T)
treated.sur <- survest(cox.res, newdata = data.treated, times = time) 
untreated.sur <- survest(cox.res, newdata = data.untreated, times = time)
treated.Mean <- colMeans(1 - treated.sur$surv) 
treated.SE<-colMeans(treated.sur$std.err) 
treated.SD<-treated.SE*sqrt(length(treated.sur$surv)/2)
untreated.Mean <- colMeans(1 - untreated.sur$surv)
untreated.SE<-colMeans(untreated.sur$std.err) 
untreated.SD<-untreated.SE*sqrt(length(untreated.sur$surv)/2)
RD.Mean <- treated.Mean - untreated.Mean
RD.SD=sqrt((untreated.SD)^2+(untreated.SD)^2)
RD.SE=RD.SD/(sqrt(length(treated.sur$surv)/2))
untreated.LCI=untreated.Mean-1.96*untreated.SE
untreated.UCI=untreated.Mean+1.96*untreated.SE
treated.LCI=treated.Mean-1.96*treated.SE
treated.UCI=treated.Mean+1.96*treated.SE

RD.LCI=RD.Mean-1.96*RD.SE
RD.UCI=RD.Mean+1.96*RD.SE
RD_ALL_MODEL2<-round(as.data.frame(cbind(untreated.Mean,untreated.LCI,untreated.UCI,
                                         treated.Mean,treated.LCI,treated.UCI,
                                         RD.Mean,RD.LCI,RD.UCI)),5)
RD_ALL_MODEL2$Period<-c("5 years","10 years")
RD_ALL_MODEL2$Model<-"Model 2"


#MODEL3
cox.res <- cph(Surv(peryear,MORT_stat)~PD+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                 Smoking_status+Drinking_status+BMI_status+Physical_status+
                 HTN_status+T2D_status+HPL_status+Cohort+
                 sdmvpsu+sdmvstra,data=data, weights=weight,surv = T, x = T, y = T)
treated.sur <- survest(cox.res, newdata = data.treated, times = time) 
untreated.sur <- survest(cox.res, newdata = data.untreated, times = time)
treated.Mean <- colMeans(1 - treated.sur$surv) 
treated.SE<-colMeans(treated.sur$std.err) 
treated.SD<-treated.SE*sqrt(length(treated.sur$surv)/2)
untreated.Mean <- colMeans(1 - untreated.sur$surv)
untreated.SE<-colMeans(untreated.sur$std.err) 
untreated.SD<-untreated.SE*sqrt(length(untreated.sur$surv)/2)
RD.Mean <- treated.Mean - untreated.Mean
RD.SD=sqrt((untreated.SD)^2+(untreated.SD)^2)
RD.SE=RD.SD/(sqrt(length(treated.sur$surv)/2))
RD.LCI=RD.Mean-1.96*RD.SE
RD.UCI=RD.Mean+1.96*RD.SE
RD_ALL_MODEL3<-round(as.data.frame(cbind(untreated.Mean,untreated.LCI,untreated.UCI,
                                         treated.Mean,treated.LCI,treated.UCI,
                                         RD.Mean,RD.LCI,RD.UCI)),5)
RD_ALL_MODEL3$Period<-c("5 years","10 years")
RD_ALL_MODEL3$Model<-"Model 3"

RD_ALL<-rbind(RD_ALL_MODEL0,RD_ALL_MODEL1,RD_ALL_MODEL2,RD_ALL_MODEL3)
RD_ALL
RD_ALL$untreated<-paste(RD_ALL$untreated.Mean," (",RD_ALL$untreated.LCI,", ",RD_ALL$untreated.UCI,")",sep = "")
RD_ALL$treated<-paste(RD_ALL$treated.Mean," (",RD_ALL$treated.LCI,", ",RD_ALL$treated.UCI,")",sep = "")
RD_ALL$RD<-paste(RD_ALL$RD.Mean," (",RD_ALL$RD.LCI,", ",RD_ALL$RD.UCI,")",sep = "")
RD_LAST<-RD_ALL[,c("Period","Model","untreated","treated","RD")]
RD_LA<-RD_LAST[order(RD_LAST$Period),]
RD_LA

FIT_ALL_PD<-survfit(Surv(peryear,MORT_stat)~PD,data=data, weights =data$weight)
survplot<-ggsurvplot(FIT_ALL_PD, data=data,break.x.by = 5,
               risk.table = TRUE,cumevents = T,cumcensor = T)
events<- format(survplot[["table"]][["data"]][["n.risk"]], big.mark = ",", scientific = FALSE)
cumevents<- format(round(survplot[["cumevents"]][["data"]][["cum.n.event"]]), big.mark = ",", scientific = FALSE)
cumevent<- round(survplot[["cumevents"]][["data"]][["cum.n.event"]])

#rbind
Table<-c("","5-year","10-year")
Table<-rbind(Table,c("Weighted events*","",""))
Table<-rbind(Table,c("No/ Mild periodontitis",paste0(cumevents[2],"/ ",events[1]),paste0(cumevents[3],"/ ",events[1])))
Table<-rbind(Table,c("Moderate/ Severe periodontitis",paste0(cumevents[9],"/ ",events[8]),paste0(cumevents[10],"/ ",events[8])))
Table<-rbind(Table,c("Cumulative incidence&","",""))
Table<-rbind(Table,c("No/ Mild periodontitis",RD_LA[8,3],RD_LA[4,3]))
Table<-rbind(Table,c("Moderate/ Severe periodontitis",RD_LA[8,4],RD_LA[4,4]))
Table<-rbind(Table,c("Absolute risk difference",RD_LA[8,5],RD_LA[4,5]))
Table<-rbind(Table,c("risk difference events#",
                     format(round(RD_ALL[7,7]*cumevent[9]), big.mark = ",", scientific = FALSE),
                     format(round(RD_ALL[8,7]*cumevent[10]), big.mark = ",", scientific = FALSE)))
Table_S5<-as.data.frame(Table)
Table_S5 = data.frame(lapply(Table_S5, as.character), stringsAsFactors=FALSE)
write.table(Table_S5,sep = ",",file ="I:/paper_8_PD&MDD/result/Supplementary Table 5.csv",row.names =F,col.names =F )

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 24 Stratified analyses (Table S6) ####  
#* Data Collation #####
load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
table(Interpolation_weighted$Age_status)
table(Interpolation_weighted$Age_status,useNA = "ifany")
Interpolation_weighted$Age_sub[Interpolation_weighted$Age_status=="<45"]<-"<45"
Interpolation_weighted$Age_sub[Interpolation_weighted$Age_status=="[45,65)"|Interpolation_weighted$Age_status==">=65"]<-">=45"

Interpolation_weighted$Age_sub<-factor(Interpolation_weighted$Age_sub,
                                       levels = c("<45",">=45")) 
table(Interpolation_weighted$Age_sub)
options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)
{ #* Age #####
  
  #### >=45 ####
  rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
  rhcSvy_Age<-subset(rhcSvy,Age_sub==">=45")
  #all
  model<-svycoxph(Surv(peryear, MORT_stat==1) ~
                    PD_diagnosis+Sex+Race_ethnicity+SES+Marital_status+
                    Smoking_status+Drinking_status+BMI+Physical_status+
                    HTN_status+T2D_status+HPL_status
                  , design =rhcSvy_Age)
  model_result<-summary(model)
  
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'Group'=">=45",'status'="All cause")
  result_all<-result
  
  model<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                    PD_diagnosis+Sex+Race_ethnicity+SES+Marital_status+
                    Smoking_status+Drinking_status+BMI+Physical_status+
                    HTN_status+T2D_status+HPL_status
                  , design =rhcSvy_Age)
  model_result<-summary(model)
  
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'Group'=">=45",'status'="CVD cause")
  result_all<-rbind(result_all,result)
  
  model<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                    PD_diagnosis+Sex+Race_ethnicity+SES+Marital_status+
                    Smoking_status+Drinking_status+BMI+Physical_status+
                    HTN_status+T2D_status+HPL_status
                  , design =rhcSvy_Age)
  model_result<-summary(model)
  
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'Group'=">=45",'status'="Cancer cause")
  result_all<-rbind(result_all,result)
  result_all
  
  #### <45 ####
  rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
  rhcSvy_Age<-subset(rhcSvy,Age_sub=="<45")
  #all
  model<-svycoxph(Surv(peryear, MORT_stat==1) ~
                    PD_diagnosis+Sex+Race_ethnicity+SES+Marital_status+
                    Smoking_status+Drinking_status+BMI+Physical_status+
                    HTN_status+T2D_status+HPL_status
                  , design =rhcSvy_Age)
  model_result<-summary(model)
  
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'Group'="<45",'status'="All cause")
  result_all<-rbind(result_all,result)
  
  model<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                    PD_diagnosis+Sex+Race_ethnicity+SES+Marital_status+
                    Smoking_status+Drinking_status+BMI+Physical_status+
                    HTN_status+T2D_status+HPL_status
                  , design =rhcSvy_Age)
  model_result<-summary(model)
  
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'Group'="<45",'status'="CVD cause")
  result_all<-rbind(result_all,result)
  
  model<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                    PD_diagnosis+Sex+Race_ethnicity+SES+Marital_status+
                    Smoking_status+Drinking_status+BMI+Physical_status+
                    HTN_status+T2D_status+HPL_status
                  , design =rhcSvy_Age)
  model_result<-summary(model)
  
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'Group'="<45",'status'="Cancer cause")
  result.cause<-rbind(result_all,result)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Age<-result.cause
  
}
{ #* Sex #####
  table(Interpolation_weighted$Sex)
  #### Male ####
  rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
  rhcSvy_Sex<-subset(rhcSvy,Sex=="Male")
  #all
  model<-svycoxph(Surv(peryear, MORT_stat==1) ~
                    PD_diagnosis+Age_status+Race_ethnicity+SES+Marital_status+
                    Smoking_status+Drinking_status+BMI+Physical_status+
                    HTN_status+T2D_status+HPL_status
                  , design =rhcSvy_Sex)
  model_result<-summary(model)
  
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'Group'="Male",'status'="All cause")
  result_all<-result
  
  model<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                    PD_diagnosis+Age_status+Race_ethnicity+SES+Marital_status+
                    Smoking_status+Drinking_status+BMI+Physical_status+
                    HTN_status+T2D_status+HPL_status
                  , design =rhcSvy_Sex)
  model_result<-summary(model)
  
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'Group'="Male",'status'="CVD cause")
  result_all<-rbind(result_all,result)
  
  model<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                    PD_diagnosis+Age_status+Race_ethnicity+SES+Marital_status+
                    Smoking_status+Drinking_status+BMI+Physical_status+
                    HTN_status+T2D_status+HPL_status
                  , design =rhcSvy_Sex)
  model_result<-summary(model)
  
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'Group'="Male",'status'="Cancer cause")
  result_all<-rbind(result_all,result)
  result_all
  
  #### Female ####
  rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
  rhcSvy_Sex<-subset(rhcSvy,Sex=="Female")
  #all
  model<-svycoxph(Surv(peryear, MORT_stat==1) ~
                    PD_diagnosis+Age_status+Race_ethnicity+SES+Marital_status+
                    Smoking_status+Drinking_status+BMI+Physical_status+
                    HTN_status+T2D_status+HPL_status
                  , design =rhcSvy_Sex)
  model_result<-summary(model)
  
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'Group'="Female",'status'="All cause")
  result_all<-rbind(result_all,result)
  
  model<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                    PD_diagnosis+Age_status+Race_ethnicity+SES+Marital_status+
                    Smoking_status+Drinking_status+BMI+Physical_status+
                    HTN_status+T2D_status+HPL_status
                  , design =rhcSvy_Sex)
  model_result<-summary(model)
  
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'Group'="Female",'status'="CVD cause")
  result_all<-rbind(result_all,result)
  
  model<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                    PD_diagnosis+Age_status+Race_ethnicity+SES+Marital_status+
                    Smoking_status+Drinking_status+BMI+Physical_status+
                    HTN_status+T2D_status+HPL_status
                  , design =rhcSvy_Sex)
  model_result<-summary(model)
  
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'Group'="Female",'status'="Cancer cause")
  result.cause<-rbind(result_all,result)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause[result.cause=="2000"]<-"2.00"
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  
  Sex<-result.cause
}
Sex
Age

Table<-c("","Periodontal status","","")
Table<-rbind(Table,c("", "No/Mild periodontitis","Moderate/Severe periodontitis","P"))
Table<-rbind(Table,c("All cause*","","",""))
Table<-rbind(Table,c("Age <45","1.00 [Reference]",Age[1,"HR_CI"],Age[1,"P.value"]))
Table<-rbind(Table,c("Age >=45","1.00 [Reference]",Age[4,"HR_CI"],Age[4,"P.value"]))
Table<-rbind(Table,c("Male","1.00 [Reference]",Sex[1,"HR_CI"],Sex[1,"P.value"]))
Table<-rbind(Table,c("Female","1.00 [Reference]",Sex[4,"HR_CI"],Sex[4,"P.value"]))
Table<-rbind(Table,c("CVD cause*","","",""))
Table<-rbind(Table,c("Age <45","1.00 [Reference]",Age[2,"HR_CI"],Age[2,"P.value"]))
Table<-rbind(Table,c("Age >=45","1.00 [Reference]",Age[5,"HR_CI"],Age[5,"P.value"]))
Table<-rbind(Table,c("Male","1.00 [Reference]",Sex[2,"HR_CI"],Sex[2,"P.value"]))
Table<-rbind(Table,c("Female","1.00 [Reference]",Sex[5,"HR_CI"],Sex[5,"P.value"]))
Table<-rbind(Table,c("Cancer cause*","","",""))
Table<-rbind(Table,c("Age <45","1.00 [Reference]",Age[3,"HR_CI"],Age[3,"P.value"]))
Table<-rbind(Table,c("Age >=45","1.00 [Reference]",Age[6,"HR_CI"],Age[6,"P.value"]))
Table<-rbind(Table,c("Male","1.00 [Reference]",Sex[3,"HR_CI"],Sex[3,"P.value"]))
Table<-rbind(Table,c("Female","1.00 [Reference]",Sex[6,"HR_CI"],Sex[6,"P.value"]))
Table_S6<-as.data.frame(Table)
Table_S6
Table_S6= data.frame(lapply(Table_S6, as.character), stringsAsFactors=FALSE)
write.table(Table_S6,sep = ",",file ="I:/paper_8_PD&MDD/result/Supplementary Table 6.csv",row.names =F,col.names =F )

# +++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 25 Mean CAL and PPD  (Table S7) ####  
{ #* Mean CAL #####
load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #** all model #####
  #all model1
  model1<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     CAL_mean, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     CAL_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result<-rbind(result,result2)
  #all model3
  model3<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     CAL_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result<-rbind(result,result3)
  result
  #all model4
  model4<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     CAL_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result_All<-rbind(result,result4)
  result_All
}

{ #** CVD model #####
  
  #CVD Crude
  model1<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     CAL_mean, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     CAL_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result<-rbind(result,result2)
  result
  #CVD model2
  model3<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     CAL_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result<-rbind(result,result3)
  result
  #CVD model3
  model4<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     CAL_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result_CVD<-rbind(result,result4)
  result_CVD
  
}

{ #** Cancer model #####
  
  #Cancer model1
  model1<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     CAL_mean, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model2
  model2<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     CAL_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result<-rbind(result,result2)
  result
  #Cancer model3
  model3<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     CAL_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status,design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result<-rbind(result,result3)
  result
  #Cancer model4
  model4<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     CAL_mean+Age+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status++BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result_Cancer<-rbind(result,result4)
  result_Cancer
}
{ #** Combine #####
  #Results
  result.cause<-rbind(result_All,result_CVD,result_Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="1000"]<-"1.00"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause.CAL<-result.cause
}
}
result.cause.CAL
  { #* Mean PPD #####

load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #** all model #####
  #all model1
  model1<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PPD_mean, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PPD_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result<-rbind(result,result2)
  #all model3
  model3<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PPD_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result<-rbind(result,result3)
  result
  #all model4
  model4<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PPD_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result_All<-rbind(result,result4)
  result_All
}

{ #** CVD model #####
  
  #CVD Crude
  model1<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PPD_mean, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PPD_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result<-rbind(result,result2)
  result
  #CVD model2
  model3<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PPD_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result<-rbind(result,result3)
  result
  #CVD model3
  model4<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PPD_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result_CVD<-rbind(result,result4)
  result_CVD
  
}

{ #** Cancer model #####
  
  #Cancer model1
  model1<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PPD_mean, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model2
  model2<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PPD_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result<-rbind(result,result2)
  result
  #Cancer model3
  model3<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PPD_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status,design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result<-rbind(result,result3)
  result
  #Cancer model4
  model4<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PPD_mean+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result_Cancer<-rbind(result,result4)
  result_Cancer
}
{ #** Combine #####
  #Results
  result.cause<-rbind(result_All,result_CVD,result_Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="10000"]<-"1.000"
  result.cause[result.cause=="2000"]<-"2.00"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause.PPD<-result.cause
}
  }
result.cause.CAL$HR_CI<-paste0(result.cause.CAL$HR," (",
                               result.cause.CAL$lower..95,", ",
                               result.cause.CAL$upper..95,")")
CAL<-result.cause.CAL
result.cause.PPD$HR_CI<-paste0(result.cause.PPD$HR," (",
                               result.cause.PPD$lower..95,", ",
                               result.cause.PPD$upper..95,")")
PPD<-result.cause.PPD


#Cbind
  Table<-c("","Periodontal index","","","")
  Table<-rbind(Table,c("", "Mean CAL","","Mean PPD",""))
  Table<-rbind(Table,c("", "HR (95% CI)","P","HR (95% CI)","P"))
  Table<-rbind(Table,c("All cause*","","","",""))
  Table<-rbind(Table,c("Model 1†",CAL[1,"HR_CI"],CAL[1,"P.value"],
              PPD[1,"HR_CI"],PPD[1,"P.value"]))
  Table<-rbind(Table,c("Model 2‡",CAL[2,"HR_CI"],CAL[2,"P.value"],
                       PPD[2,"HR_CI"],PPD[2,"P.value"]))
  Table<-rbind(Table,c("Model 3§",CAL[3,"HR_CI"],CAL[3,"P.value"],
                       PPD[3,"HR_CI"],PPD[3,"P.value"]))
  Table<-rbind(Table,c("Model 4¶",CAL[4,"HR_CI"],CAL[4,"P.value"],
                       PPD[4,"HR_CI"],PPD[4,"P.value"]))

  Table<-rbind(Table,c("CVD cause","","","",""))
  Table<-rbind(Table,c("Model 1",CAL[5,"HR_CI"],CAL[5,"P.value"],
                       PPD[5,"HR_CI"],PPD[5,"P.value"]))
  Table<-rbind(Table,c("Model 2",CAL[6,"HR_CI"],CAL[6,"P.value"],
                       PPD[6,"HR_CI"],PPD[6,"P.value"]))
  Table<-rbind(Table,c("Model 3",CAL[7,"HR_CI"],CAL[7,"P.value"],
                       PPD[7,"HR_CI"],PPD[7,"P.value"]))
  Table<-rbind(Table,c("Model 4",CAL[8,"HR_CI"],CAL[8,"P.value"],
                       PPD[8,"HR_CI"],PPD[8,"P.value"]))
  
  Table<-rbind(Table,c("Cancer cause","","","",""))
  Table<-rbind(Table,c("Model 1",CAL[5,"HR_CI"],CAL[5,"P.value"],
                       PPD[5,"HR_CI"],PPD[5,"P.value"]))
  Table<-rbind(Table,c("Model 2",CAL[6,"HR_CI"],CAL[6,"P.value"],
                       PPD[6,"HR_CI"],PPD[6,"P.value"]))
  Table<-rbind(Table,c("Model 3",CAL[7,"HR_CI"],CAL[7,"P.value"],
                       PPD[7,"HR_CI"],PPD[7,"P.value"]))
  Table<-rbind(Table,c("Model 4",CAL[8,"HR_CI"],CAL[8,"P.value"],
                       PPD[8,"HR_CI"],PPD[8,"P.value"]))
  
  Table_S7<-as.data.frame(Table)
  Table_S7= data.frame(lapply(Table_S67, as.character), stringsAsFactors=FALSE)
  write.table(Table_S7,sep = ",",file ="I:/paper_8_PD&MDD/result/Supplementary Table 7.csv",row.names =F,col.names =F )
 
  
# +++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 quantile of CAL and PPD (Table S8 & S9) ####    
#### CAL ####
  load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
  colnames(Interpolation_weighted)
table(Interpolation_weighted$CAL_quantile)
Interpolation_weighted$CAL_quantile<-as.character(Interpolation_weighted$CAL_quantile)
Interpolation_weighted$CAL_qua[Interpolation_weighted$CAL_quantile=="Quantile 1"]<-0
Interpolation_weighted$CAL_qua[Interpolation_weighted$CAL_quantile=="Quantile 2"]<-1
Interpolation_weighted$CAL_qua[Interpolation_weighted$CAL_quantile=="Quantile 3"]<-2
Interpolation_weighted$CAL_qua[Interpolation_weighted$CAL_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$CAL_qua)
Interpolation_weighted$PPD_quantile<-as.character(Interpolation_weighted$PPD_quantile)
Interpolation_weighted$PPD_qua[Interpolation_weighted$PPD_quantile=="Quantile 1"]<-0
Interpolation_weighted$PPD_qua[Interpolation_weighted$PPD_quantile=="Quantile 2"]<-1
Interpolation_weighted$PPD_qua[Interpolation_weighted$PPD_quantile=="Quantile 3"]<-2
Interpolation_weighted$PPD_qua[Interpolation_weighted$PPD_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$PPD_qua)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all model1
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         CAL_quantile, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  model1_all_result
  P<-model1_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         CAL_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model3
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         CAL_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
  #all model4
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         CAL_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         CAL_quantile, design =rhcSvy)
  model1_CVD_result<-summary(model1_CVD)
  model1_CVD_result
  P<-model1_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         CAL_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result2
  result.CVD<-rbind(result,result2)
  #CVD model3
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         CAL_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_CVD_result<-summary(model3_CVD)
  
  P<-model3_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3)
  result.CVD
  #CVD model4
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         CAL_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
}
{ #* Cancer model #####
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            CAL_quantile, design =rhcSvy)
  model1_Cancer_result<-summary(model1_Cancer)
  model1_Cancer_result
  P<-model1_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            CAL_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result2
  result.Cancer<-rbind(result,result2)
  #Cancer model3
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            CAL_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_Cancer_result<-summary(model3_Cancer)
  model3_Cancer_result
  P<-model3_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3)
  result.Cancer
  
  #Cancer model4
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            CAL_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status+
                            HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_Cancer_result<-summary(model4_Cancer)
  model4_Cancer_result
  P<-model4_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}


{ #* Combine #####
  result.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  CAL<-result.cause
  CAL
}
 { #* all model #####
    #all model1
    model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                           CAL_qua, design =rhcSvy)
    model1_all_result<-summary(model1_all)
    model1_all_result
    P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
    result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'model'="model1",'status'="All cause")
    result
    #all model2
    model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                           CAL_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
    model2_all_result<-summary(model2_all)
    P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
    result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model2",'status'="All cause")
    result2
    result.all<-rbind(result,result2)
    #all model3
    model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                           CAL_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                           Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
    model3_all_result<-summary(model3_all)
    
    P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
    result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model3",'status'="All cause")
    result.all<-rbind(result.all,result3)
    result.all
    #all model4
    model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                           CAL_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                           Smoking_status+Drinking_status+BMI_status+Physical_status+
                           HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
    model4_all_result<-summary(model4_all)
    
    P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
    result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model4",'status'="All cause")
    result.all<-rbind(result.all,result4)
    result.all
  }
  
  { #* CVD model #####
    #CVD Crude
    model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                           CAL_qua, design =rhcSvy)
    model1_CVD_result<-summary(model1_CVD)
    model1_CVD_result
    P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
    result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'model'="model1",'status'="CVD cause")
    result
    #CVD model1
    model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                           CAL_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
    model2_CVD_result<-summary(model2_CVD)
    P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
    result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model2",'status'="CVD cause")
    result2
    result.CVD<-rbind(result,result2)
    #CVD model3
    model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                           CAL_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                           Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
    model3_CVD_result<-summary(model3_CVD)
    
    P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
    result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model3",'status'="CVD cause")
    result.CVD<-rbind(result.CVD,result3)
    result.CVD
    #CVD model4
    model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                           CAL_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                           Smoking_status+Drinking_status+BMI_status+Physical_status+
                           HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
    model4_CVD_result<-summary(model4_CVD)
    
    P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
    result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model4",'status'="CVD cause")
    result.CVD<-rbind(result.CVD,result4)
    result.CVD
  }
  { #* Cancer model #####
    #Cancer Crude
    model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                              CAL_qua, design =rhcSvy)
    model1_Cancer_result<-summary(model1_Cancer)
    model1_Cancer_result
    P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
    result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'model'="model1",'status'="Cancer cause")
    result
    #Cancer model1
    model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                              CAL_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
    model2_Cancer_result<-summary(model2_Cancer)
    P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
    result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model2",'status'="Cancer cause")
    result2
    result.Cancer<-rbind(result,result2)
    #Cancer model3
    model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                              CAL_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                              Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
    model3_Cancer_result<-summary(model3_Cancer)
    model3_Cancer_result
    P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
    result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model3",'status'="Cancer cause")
    result.Cancer<-rbind(result.Cancer,result3)
    result.Cancer
    
    #Cancer model4
    model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                              CAL_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                              Smoking_status+Drinking_status+BMI_status+Physical_status+
                              HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
    model4_Cancer_result<-summary(model4_Cancer)
    model4_Cancer_result
    P<-model4_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-model4_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
    result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model4",'status'="Cancer cause")
    result.Cancer<-rbind(result.Cancer,result4)
    result.Cancer
   }


  { #* Combine #####
    result.cause<-rbind(result.all,result.CVD,result.Cancer)
    result.cause$HR<-round(result.cause$HR,2)
    result.cause$lower..95<-round(result.cause$lower..95,2)
    result.cause$upper..95<-round(result.cause$upper..95,2)
    result.cause$P.value<-round(result.cause$P.value,3)
    round_2_function <- function(x){
      while(nchar(x)<4){
        temp <- paste(x,0)
        x <- temp
        x <- gsub(" ","",x)
      }
      return(x)
    }
    
    result.cause$HR<-lapply(result.cause$HR,round_2_function)
    result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
    result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
    result.cause$HR_CI<-paste0(result.cause$HR," (",
                               result.cause$lower..95,", ",
                               result.cause$upper..95,")")
    CAL_qua<-result.cause
    CAL_qua
  }
#Cbind
Table<-c("","Periodontal index status","","","","")
Table<-rbind(Table,c("Mean CAL","Quantile 1*", "Quantile 2","Quantile 3","Quantile 4","P for trend"))

Table<-rbind(Table,c("All cause","", "","","",""))

Table<-rbind(Table,c("Model 1†","1.00 [Reference]",CAL[1,"HR_CI"],CAL[2,"HR_CI"],CAL[3,"HR_CI"],CAL_qua[1,"P.value"]))
Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",CAL[4,"HR_CI"],CAL[5,"HR_CI"],CAL[6,"HR_CI"],CAL_qua[2,"P.value"]))
Table<-rbind(Table,c("Model 3§","1.00 [Reference]",CAL[7,"HR_CI"],CAL[8,"HR_CI"],CAL[9,"HR_CI"],CAL_qua[3,"P.value"]))
Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",CAL[10,"HR_CI"],CAL[11,"HR_CI"],CAL[12,"HR_CI"],CAL_qua[4,"P.value"]))

Table<-rbind(Table,c("CVD cause","", "","","",""))
Table<-rbind(Table,c("Model 1","1.00 [Reference]",CAL[13,"HR_CI"],CAL[14,"HR_CI"],CAL[15,"HR_CI"],CAL_qua[5,"P.value"]))
Table<-rbind(Table,c("Model 2","1.00 [Reference]",CAL[16,"HR_CI"],CAL[17,"HR_CI"],CAL[18,"HR_CI"],CAL_qua[6,"P.value"]))
Table<-rbind(Table,c("Model 3","1.00 [Reference]",CAL[19,"HR_CI"],CAL[20,"HR_CI"],CAL[21,"HR_CI"],CAL_qua[7,"P.value"]))
Table<-rbind(Table,c("Model 4","1.00 [Reference]",CAL[22,"HR_CI"],CAL[23,"HR_CI"],CAL[24,"HR_CI"],CAL_qua[8,"P.value"]))

Table<-rbind(Table,c("Cancer cause","","","","",""))
Table<-rbind(Table,c("Model 1","1.00 [Reference]",CAL[25,"HR_CI"],CAL[26,"HR_CI"],CAL[27,"HR_CI"],CAL_qua[9,"P.value"]))
Table<-rbind(Table,c("Model 2","1.00 [Reference]",CAL[28,"HR_CI"],CAL[29,"HR_CI"],CAL[30,"HR_CI"],CAL_qua[10,"P.value"]))
Table<-rbind(Table,c("Model 3","1.00 [Reference]",CAL[31,"HR_CI"],CAL[32,"HR_CI"],CAL[33,"HR_CI"],CAL_qua[11,"P.value"]))
Table<-rbind(Table,c("Model 4","1.00 [Reference]",CAL[34,"HR_CI"],CAL[35,"HR_CI"],CAL[36,"HR_CI"],CAL_qua[12,"P.value"]))
Table



Table_S8<-as.data.frame(Table)
Table_S8= data.frame(lapply(Table_S8, as.character), stringsAsFactors=FALSE)
write.table(Table_S8,sep = ",",file ="I:/paper_8_PD&MDD/result/Supplementary Table 8.csv",row.names =F,col.names =F )

#### PPD ####

{ #* all model #####
  #all model1
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_quantile, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  model1_all_result
  P<-model1_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model3
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
  #all model4
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_quantile, design =rhcSvy)
  model1_CVD_result<-summary(model1_CVD)
  model1_CVD_result
  P<-model1_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result2
  result.CVD<-rbind(result,result2)
  #CVD model3
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_CVD_result<-summary(model3_CVD)
  
  P<-model3_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3)
  result.CVD
  #CVD model4
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
}
{ #* Cancer model #####
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_quantile, design =rhcSvy)
  model1_Cancer_result<-summary(model1_Cancer)
  model1_Cancer_result
  P<-model1_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result2
  result.Cancer<-rbind(result,result2)
  #Cancer model3
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_Cancer_result<-summary(model3_Cancer)
  model3_Cancer_result
  P<-model3_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3)
  result.Cancer
  
  #Cancer model4
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status+
                            HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_Cancer_result<-summary(model4_Cancer)
  model4_Cancer_result
  P<-model4_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
  
  
}
{ #* Combine #####
  result.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  PPD<-result.cause
  PPD
}

{ #* all model #####
  #all model1
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_qua, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  model1_all_result
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model3
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
  #all model4
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_qua, design =rhcSvy)
  model1_CVD_result<-summary(model1_CVD)
  model1_CVD_result
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result2
  result.CVD<-rbind(result,result2)
  #CVD model3
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_CVD_result<-summary(model3_CVD)
  
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3)
  result.CVD
  #CVD model4
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
}
{ #* Cancer model #####
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_qua, design =rhcSvy)
  model1_Cancer_result<-summary(model1_Cancer)
  model1_Cancer_result
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result2
  result.Cancer<-rbind(result,result2)
  #Cancer model3
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_Cancer_result<-summary(model3_Cancer)
  model3_Cancer_result
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3)
  result.Cancer
  
  #Cancer model4
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status+
                            HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_Cancer_result<-summary(model4_Cancer)
  model4_Cancer_result
  P<-model4_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}


{ #* Combine #####
  result.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  PPD_qua<-result.cause
  PPD_qua
}
Table<-c("","Periodontal index status","","","","")
Table<-rbind(Table,c("Mean CAL","Quantile 1*", "Quantile 2","Quantile 3","Quantile 4","P for trend"))

Table<-rbind(Table,c("All cause","", "","","",""))

Table<-rbind(Table,c("Model 1†","1.00 [Reference]",PPD[1,"HR_CI"],PPD[2,"HR_CI"],PPD[3,"HR_CI"],PPD_qua[1,"P.value"]))
Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",PPD[4,"HR_CI"],PPD[5,"HR_CI"],PPD[6,"HR_CI"],PPD_qua[2,"P.value"]))
Table<-rbind(Table,c("Model 3§","1.00 [Reference]",PPD[7,"HR_CI"],PPD[8,"HR_CI"],PPD[9,"HR_CI"],PPD_qua[3,"P.value"]))
Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",PPD[10,"HR_CI"],PPD[11,"HR_CI"],PPD[12,"HR_CI"],PPD_qua[4,"P.value"]))

Table<-rbind(Table,c("CVD cause","", "","","",""))
Table<-rbind(Table,c("Model 1","1.00 [Reference]",PPD[13,"HR_CI"],PPD[14,"HR_CI"],PPD[15,"HR_CI"],PPD_qua[5,"P.value"]))
Table<-rbind(Table,c("Model 2","1.00 [Reference]",PPD[16,"HR_CI"],PPD[17,"HR_CI"],PPD[18,"HR_CI"],PPD_qua[6,"P.value"]))
Table<-rbind(Table,c("Model 3","1.00 [Reference]",PPD[19,"HR_CI"],PPD[20,"HR_CI"],PPD[21,"HR_CI"],PPD_qua[7,"P.value"]))
Table<-rbind(Table,c("Model 4","1.00 [Reference]",PPD[22,"HR_CI"],PPD[23,"HR_CI"],PPD[24,"HR_CI"],PPD_qua[8,"P.value"]))

Table<-rbind(Table,c("Cancer cause","","","","",""))
Table<-rbind(Table,c("Model 1","1.00 [Reference]",PPD[25,"HR_CI"],PPD[26,"HR_CI"],PPD[27,"HR_CI"],PPD_qua[9,"P.value"]))
Table<-rbind(Table,c("Model 2","1.00 [Reference]",PPD[28,"HR_CI"],PPD[29,"HR_CI"],PPD[30,"HR_CI"],PPD_qua[10,"P.value"]))
Table<-rbind(Table,c("Model 3","1.00 [Reference]",PPD[31,"HR_CI"],PPD[32,"HR_CI"],PPD[33,"HR_CI"],PPD_qua[11,"P.value"]))
Table<-rbind(Table,c("Model 4","1.00 [Reference]",PPD[34,"HR_CI"],PPD[35,"HR_CI"],PPD[36,"HR_CI"],PPD_qua[12,"P.value"]))
Table



Table_S9<-as.data.frame(Table)
Table_S9= data.frame(lapply(Table_S9, as.character), stringsAsFactors=FALSE)
write.table(Table_S9,sep = ",",file ="I:/paper_8_PD&MDD/result/Supplementary Table 9.csv",row.names =F,col.names =F )

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 27 Peryear>=2 (Table S10) #### 
load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
Interpolation_weighted<-subset(Interpolation_weighted,peryear>=2)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

{ #* all model #####
  #all model1
  model1<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result<-rbind(result,result2)
  #all model3
  model3<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result<-rbind(result,result3)
  result
  #all model4
  model4<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result_All<-rbind(result,result4)
  result_All
}

{ #* CVD model #####
  
  #CVD Crude
  model1<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result<-rbind(result,result2)
  result
  #CVD model2
  model3<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result<-rbind(result,result3)
  result
  #CVD model3
  model4<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result_CVD<-rbind(result,result4)
  result_CVD
  
}

{ #* Cancer model #####
  
  #Cancer model1
  model1<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model2
  model2<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result<-rbind(result,result2)
  result
  #Cancer model3
  model3<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status,design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result<-rbind(result,result3)
  result
  #Cancer model4
  model4<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result_Cancer<-rbind(result,result4)
  result_Cancer
}



{ #* Combine #####
  #Results
  result.cause<-rbind(result_All,result_CVD,result_Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="10000"]<-"1.000"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause
  #Events
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts
  
  #PD_MDD
  PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
  PD_MDD$year<-PD_MDD$peryear
  PD_MDD$Pyear<-PD_MDD$year*PD_MDD$weight
  PD_MDD_death_All<-PD_MDD[which(PD_MDD$MORT_stat==1),]
  PD_MDD_death_CVD<-PD_MDD[which(PD_MDD$CVD_MORT_stat==1),]
  PD_MDD_death_Cancer<-PD_MDD[which(PD_MDD$Cancer_MORT_stat==1),]
  
  #noPD_MDD
  noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
  noPD_MDD$year<-noPD_MDD$peryear
  noPD_MDD$Pyear<-noPD_MDD$year*noPD_MDD$weight
  noPD_MDD_death_All<-noPD_MDD[which(noPD_MDD$MORT_stat==1),]
  noPD_MDD_death_CVD<-noPD_MDD[which(noPD_MDD$CVD_MORT_stat==1),]
  noPD_MDD_death_Cancer<-noPD_MDD[which(noPD_MDD$Cancer_MORT_stat==1),]
  
  #all.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  
  all.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #CVD.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  CVD.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #Cancer.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  Cancer.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  total.counts_ad<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts_ad
  
  #Cbind
  Table<-c("","Periodontal status","","")
  Table<-rbind(Table,c("", "No/Mild periodontitis","Moderate/Severe periodontitis","P"))
  Table<-rbind(Table,c("All cause*","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[1,1],total.counts[1,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[1,1],total.counts_ad[1,2],""))
  Table<-rbind(Table,c("Model 1†","1.00 [Reference]",paste0(result.cause[1,1]," (",result.cause[1,2],", ",result.cause[1,3],")"),result.cause[1,4]))
  Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",paste0(result.cause[2,1]," (",result.cause[2,2],", ",result.cause[2,3],")"),result.cause[2,4]))
  Table<-rbind(Table,c("Model 3§","1.00 [Reference]",paste0(result.cause[3,1]," (",result.cause[3,2],", ",result.cause[3,3],")"),result.cause[3,4]))
  Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",paste0(result.cause[4,1]," (",result.cause[4,2],", ",result.cause[4,3],")"),result.cause[4,4]))
  Table<-rbind(Table,c("CVD cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[2,1],total.counts[2,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[2,1],total.counts_ad[2,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[5,1]," (",result.cause[5,2],", ",result.cause[5,3],")"),result.cause[5,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[6,1]," (",result.cause[6,2],", ",result.cause[6,3],")"),result.cause[6,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[7,1]," (",result.cause[7,2],", ",result.cause[7,3],")"),result.cause[7,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[8,1]," (",result.cause[8,2],", ",result.cause[8,3],")"),result.cause[8,4]))
  Table<-rbind(Table,c("Cancer cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[3,1],total.counts[3,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[3,1],total.counts_ad[3,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[9,1]," (",result.cause[9,2],", ",result.cause[9,3],")"),result.cause[9,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[10,1]," (",result.cause[10,2],", ",result.cause[10,3],")"),result.cause[10,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[11,1]," (",result.cause[11,2],", ",result.cause[11,3],")"),result.cause[11,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[12,1]," (",result.cause[12,2],", ",result.cause[12,3],")"),result.cause[12,4]))
  Table<-as.data.frame(Table)
  Table_S10 = data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  Table_S10
  write.table(Table_S10,sep = ",",file ="I:/paper_8_PD&MDD/result/Supplementary Table 10.csv",row.names =F,col.names =F )
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 28 omitted cancer and CVD patients (Table S11) ####  
load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
Interpolation_weighted<-subset(Interpolation_weighted,Cancer_status!="YES"&CVD_status!="YES")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

{ #* all model #####
  #all model1
  model1<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result<-rbind(result,result2)
  #all model3
  model3<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result<-rbind(result,result3)
  result
  #all model4
  model4<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result_All<-rbind(result,result4)
  result_All
}

{ #* CVD model #####
  
  #CVD Crude
  model1<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result<-rbind(result,result2)
  result
  #CVD model2
  model3<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result<-rbind(result,result3)
  result
  #CVD model3
  model4<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result_CVD<-rbind(result,result4)
  result_CVD
  
}

{ #* Cancer model #####
  
  #Cancer model1
  model1<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model2
  model2<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result<-rbind(result,result2)
  result
  #Cancer model3
  model3<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status,design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result<-rbind(result,result3)
  result
  #Cancer model4
  model4<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result_Cancer<-rbind(result,result4)
  result_Cancer
}



{ #* Combine #####
  #Results
  result.cause<-rbind(result_All,result_CVD,result_Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="10000"]<-"1.000"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause
  #Events
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts
  
  #PD_MDD
  PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
  PD_MDD$year<-PD_MDD$peryear
  PD_MDD$Pyear<-PD_MDD$year*PD_MDD$weight
  PD_MDD_death_All<-PD_MDD[which(PD_MDD$MORT_stat==1),]
  PD_MDD_death_CVD<-PD_MDD[which(PD_MDD$CVD_MORT_stat==1),]
  PD_MDD_death_Cancer<-PD_MDD[which(PD_MDD$Cancer_MORT_stat==1),]
  
  #noPD_MDD
  noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
  noPD_MDD$year<-noPD_MDD$peryear
  noPD_MDD$Pyear<-noPD_MDD$year*noPD_MDD$weight
  noPD_MDD_death_All<-noPD_MDD[which(noPD_MDD$MORT_stat==1),]
  noPD_MDD_death_CVD<-noPD_MDD[which(noPD_MDD$CVD_MORT_stat==1),]
  noPD_MDD_death_Cancer<-noPD_MDD[which(noPD_MDD$Cancer_MORT_stat==1),]
  
  #all.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  
  all.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #CVD.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  CVD.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #Cancer.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  Cancer.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  total.counts_ad<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts_ad
  
  #Cbind
  Table<-c("","Periodontal status","","")
  Table<-rbind(Table,c("", "No/Mild periodontitis","Moderate/Severe periodontitis","P"))
  Table<-rbind(Table,c("All cause*","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[1,1],total.counts[1,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[1,1],total.counts_ad[1,2],""))
  Table<-rbind(Table,c("Model 1†","1.00 [Reference]",paste0(result.cause[1,1]," (",result.cause[1,2],", ",result.cause[1,3],")"),result.cause[1,4]))
  Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",paste0(result.cause[2,1]," (",result.cause[2,2],", ",result.cause[2,3],")"),result.cause[2,4]))
  Table<-rbind(Table,c("Model 3§","1.00 [Reference]",paste0(result.cause[3,1]," (",result.cause[3,2],", ",result.cause[3,3],")"),result.cause[3,4]))
  Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",paste0(result.cause[4,1]," (",result.cause[4,2],", ",result.cause[4,3],")"),result.cause[4,4]))
  Table<-rbind(Table,c("CVD cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[2,1],total.counts[2,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[2,1],total.counts_ad[2,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[5,1]," (",result.cause[5,2],", ",result.cause[5,3],")"),result.cause[5,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[6,1]," (",result.cause[6,2],", ",result.cause[6,3],")"),result.cause[6,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[7,1]," (",result.cause[7,2],", ",result.cause[7,3],")"),result.cause[7,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[8,1]," (",result.cause[8,2],", ",result.cause[8,3],")"),result.cause[8,4]))
  Table<-rbind(Table,c("Cancer cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[3,1],total.counts[3,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[3,1],total.counts_ad[3,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[9,1]," (",result.cause[9,2],", ",result.cause[9,3],")"),result.cause[9,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[10,1]," (",result.cause[10,2],", ",result.cause[10,3],")"),result.cause[10,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[11,1]," (",result.cause[11,2],", ",result.cause[11,3],")"),result.cause[11,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[12,1]," (",result.cause[12,2],", ",result.cause[12,3],")"),result.cause[12,4]))
  Table<-as.data.frame(Table)
  Table_S11 = data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  Table_S11
  write.table(Table_S11,sep = ",",file ="I:/paper_8_PD&MDD/result/Supplementary Table 11.csv",row.names =F,col.names =F )
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 29 Original data (Table S12) ####  
load(file="I:/paper_8_PD&MDD/data/Original_weighted.Rdata")
Interpolation_weighted<-Original_weighted
colnames(Interpolation_weighted)

table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
var<-c("MORT_stat","Age_status","Sex","Marital_status","Race_ethnicity","Education_levels","PIR",
       "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status","HEI",
       "HPL_status","HTN_status","CVD_status",
       "BMI_status","T2D_status","Cancer_status","Cohort")
VAR<-c("MORT_stat","CAL_mean","PPD_mean","Age","Marital_status","Age_status","Sex","Race_ethnicity","Education_levels","PIR",
       "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status","HEI", "HTN_status","HPL_status",
       "BMI","BMI_status","T2D_status","CVD_status","Cancer_status","Cohort")

options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)
{ #* section 18.1 Over all ####
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  Over<- ldply(lapply(VAR, model))
}
options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)

{ #* section 18.2 No/Mild periodontitis ####
  rhcSvy_HEI2PD<-subset(rhcSvy,PD_diagnosis=="No/Mild periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_HEI2PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  noPD<- ldply(lapply(VAR, model))
}  
{ #* section 18.3 Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,PD_diagnosis=="Moderate/Severe periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  PD<- ldply(lapply(VAR, model))
}
Table1<-cbind(Over,noPD[,c("counts","Mean","SE")],PD[,c("counts","Mean","SE")])
save(Table1,file = "I:/paper_8_PD&MDD/data/Table1_Rdata")  
{ #* section 18.4 t-test and chi-test ####
  model<- function(x){
    
    if( x %in% var ) {
      formula<-as.formula(paste0("~",x,"+PD_diagnosis"))
      chi_test<-svychisq(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =chi_test[["p.value"]])
      return(model)
    } else {
      formula<-as.formula(paste0(x,"~PD_diagnosis"))
      t_test<-svyttest(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =t_test[["p.value"]])
      return(model)
    }
  }  
  test_data<- ldply(lapply(VAR, model))
  test_data$P.value<-round(test_data$P.value,3)
  test_data$P.value[test_data$P.value==0]<-"<0.001"
  new.function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  test_data$P.value<-lapply(test_data$P.value,new.function)
  test_data$P.value<-as.character(test_data$P.value)
}
load(file = "I:/paper_8_PD&MDD/data/Table1_Rdata")
Table1<-merge(Table1,test_data,by="Covariates",all.x = T)
Table1$Covariates
Table1$Row<-paste0(Table1$Covariates," ",Table1$grade)
rownames(Table1)<-Table1$Row
colnames(Table1)<-c("Covariates","grade",
                    "counts_all","Mean_all","SE_all",
                    "counts_noPD","Mean_noPD","SE_noPD",
                    "counts_PD","Mean_PD","SE_PD",
                    "P.value","Row")
rownames(Table1)
{ #* section 18.5 Combine  Table S12 #####
  #Events
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  
  PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
  noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
  
  PD.counts_ad<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts_ad<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  Counts_ad<-format(round(sum(PD_MDD$weight))+round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  
  
  Table<-c("","","","Periodontal status","","","","")
  Table<-rbind(Table,c("","Over all","","No/Mild periodontitis","","Moderate/Severe periodontitis","",""))
  Table<-rbind(Table,c("Characteristics","Mean/ %*","SE*","Mean/ %","SE","Mean/ %","SE","P†"))
  Table<-rbind(Table,c("No.(Unweighted)",PD.counts[1]+PD.counts[2],"",PD.counts[1],"",PD.counts[2],"",""))
  Table<-rbind(Table,c("No.(Weighted)",Counts_ad,"",PD.counts_ad,"",noPD.counts_ad,"",""))
  #Age
  Table<-rbind(Table,c("Age (years), mean",
                       Table1["Age Mean ± SE","Mean_all"],Table1["Age Mean ± SE","SE_all"],
                       Table1["Age Mean ± SE","Mean_noPD"],Table1["Age Mean ± SE","SE_noPD"],
                       Table1["Age Mean ± SE","Mean_PD"],Table1["Age Mean ± SE","SE_PD"],
                       Table1["Age Mean ± SE","P.value"] ))
  Table<-rbind(Table,c("Age status, %","","","","","","",Table1["Age_status <45","P.value"]))
  Table<-rbind(Table,c("<45",
                       Table1["Age_status <45","Mean_all"],Table1["Age_status <45","SE_all"],
                       Table1["Age_status <45","Mean_noPD"],Table1["Age_status <45","SE_noPD"],
                       Table1["Age_status <45","Mean_PD"],Table1["Age_status <45","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("[45, 65)",
                       Table1["Age_status [45,65)","Mean_all"],Table1["Age_status [45,65)","SE_all"],
                       Table1["Age_status [45,65)","Mean_noPD"],Table1["Age_status [45,65)","SE_noPD"],
                       Table1["Age_status [45,65)","Mean_PD"],Table1["Age_status [45,65)","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("≥65",
                       Table1["Age_status >=65","Mean_all"],Table1["Age_status >=65","SE_all"],
                       Table1["Age_status >=65","Mean_noPD"],Table1["Age_status >=65","SE_noPD"],
                       Table1["Age_status >=65","Mean_PD"],Table1["Age_status >=65","SE_PD"],
                       "" ))
  
  #Sex
  Table<-rbind(Table,c("Sex, Female",
                       Table1["Sex Female","Mean_all"],Table1["Sex Female","SE_all"],
                       Table1["Sex Female","Mean_noPD"],Table1["Sex Female","SE_noPD"],
                       Table1["Sex Female","Mean_PD"],Table1["Sex Female","SE_PD"],
                       Table1["Sex Female","P.value"]))
  
  #Race/ ethnicity
  Table<-rbind(Table,c("Race/ ethnicity, %","","","","","","",Table1["Race_ethnicity Non-Hispanic White","P.value"]))
  Table<-rbind(Table,c("Non-Hispanic white",
                       Table1["Race_ethnicity Non-Hispanic White","Mean_all"],Table1["Race_ethnicity Non-Hispanic White","SE_all"],
                       Table1["Race_ethnicity Non-Hispanic White","Mean_noPD"],Table1["Race_ethnicity Non-Hispanic White","SE_noPD"],
                       Table1["Race_ethnicity Non-Hispanic White","Mean_PD"],Table1["Race_ethnicity Non-Hispanic White","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Non-Hispanic black",
                       Table1["Race_ethnicity Non-Hispanic Black","Mean_all"],Table1["Race_ethnicity Non-Hispanic Black","SE_all"],
                       Table1["Race_ethnicity Non-Hispanic Black","Mean_noPD"],Table1["Race_ethnicity Non-Hispanic Black","SE_noPD"],
                       Table1["Race_ethnicity Non-Hispanic Black","Mean_PD"],Table1["Race_ethnicity Non-Hispanic Black","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Hispanic",
                       Table1["Race_ethnicity Hispanic","Mean_all"],Table1["Race_ethnicity Hispanic","SE_all"],
                       Table1["Race_ethnicity Hispanic","Mean_noPD"],Table1["Race_ethnicity Hispanic","SE_noPD"],
                       Table1["Race_ethnicity Hispanic","Mean_PD"],Table1["Race_ethnicity Hispanic","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Other race/ ethnicity",
                       Table1["Race_ethnicity Other_Race","Mean_all"],Table1["Race_ethnicity Other_Race","SE_all"],
                       Table1["Race_ethnicity Other_Race","Mean_noPD"],Table1["Race_ethnicity Other_Race","SE_noPD"],
                       Table1["Race_ethnicity Other_Race","Mean_PD"],Table1["Race_ethnicity Other_Race","SE_PD"],
                       "" ))
  #Marital status
  Table<-rbind(Table,c("Marital status, %","","","","","","",Table1["Marital_status Married","P.value"]))
  Table<-rbind(Table,c("Married",
                       Table1["Marital_status Married","Mean_all"],Table1["Marital_status Married","SE_all"],
                       Table1["Marital_status Married","Mean_noPD"],Table1["Marital_status Married","SE_noPD"],
                       Table1["Marital_status Married","Mean_PD"],Table1["Marital_status Married","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Never married",
                       Table1["Marital_status Never_married","Mean_all"],Table1["Marital_status Never_married","SE_all"],
                       Table1["Marital_status Never_married","Mean_noPD"],Table1["Marital_status Never_married","SE_noPD"],
                       Table1["Marital_status Never_married","Mean_PD"],Table1["Marital_status Never_married","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Widowed/ Divorced/ Separated",
                       Table1["Marital_status Separated","Mean_all"],Table1["Marital_status Separated","SE_all"],
                       Table1["Marital_status Separated","Mean_noPD"],Table1["Marital_status Separated","SE_noPD"],
                       Table1["Marital_status Separated","Mean_PD"],Table1["Marital_status Separated","SE_PD"],
                       "" ))
  
  #Socioeconomic Status
  Table<-rbind(Table,c("Socioeconomic Status, %","","","","","","",Table1["SES low","P.value"]))
  Table<-rbind(Table,c("Low",
                       Table1["SES low","Mean_all"],Table1["SES low","SE_all"],
                       Table1["SES low","Mean_noPD"],Table1["SES low","SE_noPD"],
                       Table1["SES low","Mean_PD"],Table1["SES low","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Medium",
                       Table1["SES medium","Mean_all"],Table1["SES medium","SE_all"],
                       Table1["SES medium","Mean_noPD"],Table1["SES medium","SE_noPD"],
                       Table1["SES medium","Mean_PD"],Table1["SES medium","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("High",
                       Table1["SES high","Mean_all"],Table1["SES high","SE_all"],
                       Table1["SES high","Mean_noPD"],Table1["SES high","SE_noPD"],
                       Table1["SES high","Mean_PD"],Table1["SES high","SE_PD"],
                       "" ))
  
  #Smoking status
  Table<-rbind(Table,c("Smoking status, %","","","","","","",Table1["Smoking_status Never_smoker","P.value"]))
  Table<-rbind(Table,c("Never smoker",
                       Table1["Smoking_status Never_smoker","Mean_all"],Table1["Smoking_status Never_smoker","SE_all"],
                       Table1["Smoking_status Never_smoker","Mean_noPD"],Table1["Smoking_status Never_smoker","SE_noPD"],
                       Table1["Smoking_status Never_smoker","Mean_PD"],Table1["Smoking_status Never_smoker","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Former smoker",
                       Table1["Smoking_status Former_smoker","Mean_all"],Table1["Smoking_status Former_smoker","SE_all"],
                       Table1["Smoking_status Former_smoker","Mean_noPD"],Table1["Smoking_status Former_smoker","SE_noPD"],
                       Table1["Smoking_status Former_smoker","Mean_PD"],Table1["Smoking_status Former_smoker","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Current smoker",
                       Table1["Smoking_status Current_smoker","Mean_all"],Table1["Smoking_status Current_smoker","SE_all"],
                       Table1["Smoking_status Current_smoker","Mean_noPD"],Table1["Smoking_status Current_smoker","SE_noPD"],
                       Table1["Smoking_status Current_smoker","Mean_PD"],Table1["Smoking_status Current_smoker","SE_PD"],
                       "" ))
  
  #Drinking status
  Table<-rbind(Table,c("Drinking status, %","","","","","","",Table1["Drinking_status Nondrinker","P.value"]))
  Table<-rbind(Table,c("Nondrinker",
                       Table1["Drinking_status Nondrinker","Mean_all"],Table1["Drinking_status Nondrinker","SE_all"],
                       Table1["Drinking_status Nondrinker","Mean_noPD"],Table1["Drinking_status Nondrinker","SE_noPD"],
                       Table1["Drinking_status Nondrinker","Mean_PD"],Table1["Drinking_status Nondrinker","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Light/ moderate drinker",
                       Table1["Drinking_status Light/moderate_drinker","Mean_all"],Table1["Drinking_status Light/moderate_drinker","SE_all"],
                       Table1["Drinking_status Light/moderate_drinker","Mean_noPD"],Table1["Drinking_status Light/moderate_drinker","SE_noPD"],
                       Table1["Drinking_status Light/moderate_drinker","Mean_PD"],Table1["Drinking_status Light/moderate_drinker","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Heavier drinker",
                       Table1["Drinking_status Heavier_drinker","Mean_all"],Table1["Drinking_status Heavier_drinker","SE_all"],
                       Table1["Drinking_status Heavier_drinker","Mean_noPD"],Table1["Drinking_status Heavier_drinker","SE_noPD"],
                       Table1["Drinking_status Heavier_drinker","Mean_PD"],Table1["Drinking_status Heavier_drinker","SE_PD"],
                       "" ))
  #Physical status
  Table<-rbind(Table,c("Physical status, %","","","","","","",Table1["Physical_status Inactive","P.value"]))
  Table<-rbind(Table,c("Inactive",
                       Table1["Physical_status Inactive","Mean_all"],Table1["Physical_status Inactive","SE_all"],
                       Table1["Physical_status Inactive","Mean_noPD"],Table1["Physical_status Inactive","SE_noPD"],
                       Table1["Physical_status Inactive","Mean_PD"],Table1["Physical_status Inactive","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Insufficient",
                       Table1["Physical_status Insufficient","Mean_all"],Table1["Physical_status Insufficient","SE_all"],
                       Table1["Physical_status Insufficient","Mean_noPD"],Table1["Physical_status Insufficient","SE_noPD"],
                       Table1["Physical_status Insufficient","Mean_PD"],Table1["Physical_status Insufficient","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Recommended",
                       Table1["Physical_status Recommended","Mean_all"],Table1["Physical_status Recommended","SE_all"],
                       Table1["Physical_status Recommended","Mean_noPD"],Table1["Physical_status Recommended","SE_noPD"],
                       Table1["Physical_status Recommended","Mean_PD"],Table1["Physical_status Recommended","SE_PD"],
                       "" ))
  #BMI
  Table<-rbind(Table,c("BMI, Mean",
                       Table1["BMI Mean ± SE","Mean_all"],Table1["BMI Mean ± SE","SE_all"],
                       Table1["BMI Mean ± SE","Mean_noPD"],Table1["BMI Mean ± SE","SE_noPD"],
                       Table1["BMI Mean ± SE","Mean_PD"],Table1["BMI Mean ± SE","SE_PD"],
                       Table1["BMI Mean ± SE","P.value"] ))
  
  Table<-rbind(Table,c("BMI status (kg/m2), %","","","","","","",Table1["BMI_status (0,25)","P.value"]))
  Table<-rbind(Table,c("<25",
                       Table1["BMI_status (0,25)","Mean_all"],Table1["BMI_status (0,25)","SE_all"],
                       Table1["BMI_status (0,25)","Mean_noPD"],Table1["BMI_status (0,25)","SE_noPD"],
                       Table1["BMI_status (0,25)","Mean_PD"],Table1["BMI_status (0,25)","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("[25.0 -30)",
                       Table1["BMI_status [25.0-30)","Mean_all"],Table1["BMI_status [25.0-30)","SE_all"],
                       Table1["BMI_status [25.0-30)","Mean_noPD"],Table1["BMI_status [25.0-30)","SE_noPD"],
                       Table1["BMI_status [25.0-30)","Mean_PD"],Table1["BMI_status [25.0-30)","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("≥30",
                       Table1["BMI_status [30,inf)","Mean_all"],Table1["BMI_status [30,inf)","SE_all"],
                       Table1["BMI_status [30,inf)","Mean_noPD"],Table1["BMI_status [30,inf)","SE_noPD"],
                       Table1["BMI_status [30,inf)","Mean_PD"],Table1["BMI_status [30,inf)","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Comorbidities, %","","","","","","",""))
  #Hypertension
  Table<-rbind(Table,c("Hypertension",
                       Table1["HTN_status YES","Mean_all"],Table1["HTN_status YES","SE_all"],
                       Table1["HTN_status YES","Mean_noPD"],Table1["HTN_status YES","SE_noPD"],
                       Table1["HTN_status YES","Mean_PD"],Table1["HTN_status YES","SE_PD"],
                       Table1["HTN_status YES","P.value"]))
  #Hyperlipidemia
  Table<-rbind(Table,c("Hyperlipidemia",
                       Table1["HPL_status YES","Mean_all"],Table1["HPL_status YES","SE_all"],
                       Table1["HPL_status YES","Mean_noPD"],Table1["HPL_status YES","SE_noPD"],
                       Table1["HPL_status YES","Mean_PD"],Table1["HPL_status YES","SE_PD"],
                       Table1["HPL_status YES","P.value"]))
  #Diabetes mellitus
  Table<-rbind(Table,c("Diabetes mellitus",
                       Table1["T2D_status YES","Mean_all"],Table1["T2D_status YES","SE_all"],
                       Table1["T2D_status YES","Mean_noPD"],Table1["T2D_status YES","SE_noPD"],
                       Table1["T2D_status YES","Mean_PD"],Table1["T2D_status YES","SE_PD"],
                       Table1["T2D_status YES","P.value"]))
  #Cohort
  Table<-rbind(Table,c("Cohort period, %","","","","","","",Table1["Cohort NHANES_CON1","P.value"]))
  
  Table<-rbind(Table,c("NHANES III",
                       Table1["Cohort NHANES_III","Mean_all"],Table1["Cohort NHANES_III","SE_all"],
                       Table1["Cohort NHANES_III","Mean_noPD"],Table1["Cohort NHANES_III","SE_noPD"],
                       Table1["Cohort NHANES_III","Mean_PD"],Table1["Cohort NHANES_III","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("NHANES 1999-2004",
                       Table1["Cohort NHANES_CON1","Mean_all"],Table1["Cohort NHANES_CON1","SE_all"],
                       Table1["Cohort NHANES_CON1","Mean_noPD"],Table1["Cohort NHANES_CON1","SE_noPD"],
                       Table1["Cohort NHANES_CON1","Mean_PD"],Table1["Cohort NHANES_CON1","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("NHANES 2009-2014",
                       Table1["Cohort NHANES_CON2","Mean_all"],Table1["Cohort NHANES_CON2","SE_all"],
                       Table1["Cohort NHANES_CON2","Mean_noPD"],Table1["Cohort NHANES_CON2","SE_noPD"],
                       Table1["Cohort NHANES_CON2","Mean_PD"],Table1["Cohort NHANES_CON2","SE_PD"],
                       "" ))
  
  Table_S12<-Table
  write.table(Table_S12,sep = ",",file ="I:/paper_8_PD&MDD/result/Supplementary Table 12.csv" ,row.names =F,col.names =F )
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 30 Original data COX (Table S13) ####  
load(file="I:/paper_8_PD&MDD/data/Original_weighted.Rdata")
Interpolation_weighted<-Original_weighted
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all model1
  model1<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result<-rbind(result,result2)
  #all model3
  model3<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result<-rbind(result,result3)
  result
  #all model4
  model4<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result_All<-rbind(result,result4)
  result_All
}

{ #* CVD model #####
  
  #CVD Crude
  model1<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result<-rbind(result,result2)
  result
  #CVD model2
  model3<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result<-rbind(result,result3)
  result
  #CVD model3
  model4<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result_CVD<-rbind(result,result4)
  result_CVD
  
}

{ #* Cancer model #####
  
  #Cancer model1
  model1<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model2
  model2<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result<-rbind(result,result2)
  result
  #Cancer model3
  model3<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status,design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result<-rbind(result,result3)
  result
  #Cancer model4
  model4<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result_Cancer<-rbind(result,result4)
  result_Cancer
}



{ #* Combine #####
  #Results
  result.cause<-rbind(result_All,result_CVD,result_Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="10000"]<-"1.000"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause
  #Events
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts
  
  #PD_MDD
  PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
  PD_MDD$year<-PD_MDD$peryear
  PD_MDD$Pyear<-PD_MDD$year*PD_MDD$weight
  PD_MDD_death_All<-PD_MDD[which(PD_MDD$MORT_stat==1),]
  PD_MDD_death_CVD<-PD_MDD[which(PD_MDD$CVD_MORT_stat==1),]
  PD_MDD_death_Cancer<-PD_MDD[which(PD_MDD$Cancer_MORT_stat==1),]
  
  #noPD_MDD
  noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
  noPD_MDD$year<-noPD_MDD$peryear
  noPD_MDD$Pyear<-noPD_MDD$year*noPD_MDD$weight
  noPD_MDD_death_All<-noPD_MDD[which(noPD_MDD$MORT_stat==1),]
  noPD_MDD_death_CVD<-noPD_MDD[which(noPD_MDD$CVD_MORT_stat==1),]
  noPD_MDD_death_Cancer<-noPD_MDD[which(noPD_MDD$Cancer_MORT_stat==1),]
  
  #all.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  
  all.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #CVD.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  CVD.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #Cancer.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  Cancer.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  total.counts_ad<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts_ad
  
  #Cbind
  Table<-c("","Periodontal status","","")
  Table<-rbind(Table,c("", "No/Mild periodontitis","Moderate/Severe periodontitis","P"))
  Table<-rbind(Table,c("All cause*","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[1,1],total.counts[1,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[1,1],total.counts_ad[1,2],""))
  Table<-rbind(Table,c("Model 1†","1.00 [Reference]",paste0(result.cause[1,1]," (",result.cause[1,2],", ",result.cause[1,3],")"),result.cause[1,4]))
  Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",paste0(result.cause[2,1]," (",result.cause[2,2],", ",result.cause[2,3],")"),result.cause[2,4]))
  Table<-rbind(Table,c("Model 3§","1.00 [Reference]",paste0(result.cause[3,1]," (",result.cause[3,2],", ",result.cause[3,3],")"),result.cause[3,4]))
  Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",paste0(result.cause[4,1]," (",result.cause[4,2],", ",result.cause[4,3],")"),result.cause[4,4]))
  Table<-rbind(Table,c("CVD cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[2,1],total.counts[2,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[2,1],total.counts_ad[2,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[5,1]," (",result.cause[5,2],", ",result.cause[5,3],")"),result.cause[5,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[6,1]," (",result.cause[6,2],", ",result.cause[6,3],")"),result.cause[6,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[7,1]," (",result.cause[7,2],", ",result.cause[7,3],")"),result.cause[7,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[8,1]," (",result.cause[8,2],", ",result.cause[8,3],")"),result.cause[8,4]))
  Table<-rbind(Table,c("Cancer cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[3,1],total.counts[3,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[3,1],total.counts_ad[3,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[9,1]," (",result.cause[9,2],", ",result.cause[9,3],")"),result.cause[9,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[10,1]," (",result.cause[10,2],", ",result.cause[10,3],")"),result.cause[10,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[11,1]," (",result.cause[11,2],", ",result.cause[11,3],")"),result.cause[11,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[12,1]," (",result.cause[12,2],", ",result.cause[12,3],")"),result.cause[12,4]))
  Table<-as.data.frame(Table)
  Table_S13 = data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  Table_S13
  write.table(Table_S13,sep = ",",file ="I:/paper_8_PD&MDD/result/Supplementary Table 13.csv",row.names =F,col.names =F )
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 29 Complete data (Table S14) ####  
load(file="I:/paper_8_PD&MDD/data/Complete_weighted.Rdata")
Interpolation_weighted<-Complete_weighted
colnames(Interpolation_weighted)

table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
var<-c("MORT_stat","Age_status","Sex","Marital_status","Race_ethnicity","Education_levels","PIR",
       "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status","HEI",
       "HPL_status","HTN_status","CVD_status",
       "BMI_status","T2D_status","Cancer_status","Cohort")
VAR<-c("MORT_stat","CAL_mean","PPD_mean","Age","Marital_status","Age_status","Sex","Race_ethnicity","Education_levels","PIR",
       "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status","HEI", "HTN_status","HPL_status",
       "BMI","BMI_status","T2D_status","CVD_status","Cancer_status","Cohort")

options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)
{ #* section 18.1 Over all ####
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  Over<- ldply(lapply(VAR, model))
}
options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)

{ #* section 18.2 No/Mild periodontitis ####
  rhcSvy_HEI2PD<-subset(rhcSvy,PD_diagnosis=="No/Mild periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_HEI2PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  noPD<- ldply(lapply(VAR, model))
}  
{ #* section 18.3 Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,PD_diagnosis=="Moderate/Severe periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  PD<- ldply(lapply(VAR, model))
}
Table1<-cbind(Over,noPD[,c("counts","Mean","SE")],PD[,c("counts","Mean","SE")])
save(Table1,file = "I:/paper_8_PD&MDD/data/Table1_Rdata")  
{ #* section 18.4 t-test and chi-test ####
  model<- function(x){
    
    if( x %in% var ) {
      formula<-as.formula(paste0("~",x,"+PD_diagnosis"))
      chi_test<-svychisq(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =chi_test[["p.value"]])
      return(model)
    } else {
      formula<-as.formula(paste0(x,"~PD_diagnosis"))
      t_test<-svyttest(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =t_test[["p.value"]])
      return(model)
    }
  }  
  test_data<- ldply(lapply(VAR, model))
  test_data$P.value<-round(test_data$P.value,3)
  test_data$P.value[test_data$P.value==0]<-"<0.001"
  new.function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  test_data$P.value<-lapply(test_data$P.value,new.function)
  test_data$P.value<-as.character(test_data$P.value)
}
load(file = "I:/paper_8_PD&MDD/data/Table1_Rdata")
Table1<-merge(Table1,test_data,by="Covariates",all.x = T)
Table1$Covariates
Table1$Row<-paste0(Table1$Covariates," ",Table1$grade)
rownames(Table1)<-Table1$Row
colnames(Table1)<-c("Covariates","grade",
                    "counts_all","Mean_all","SE_all",
                    "counts_noPD","Mean_noPD","SE_noPD",
                    "counts_PD","Mean_PD","SE_PD",
                    "P.value","Row")
rownames(Table1)
{ #* section 18.5 Combine  Table S14 #####
  #Events
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  
  PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
  noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
  
  PD.counts_ad<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts_ad<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  Counts_ad<-format(round(sum(PD_MDD$weight))+round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  
  
  Table<-c("","","","Periodontal status","","","","")
  Table<-rbind(Table,c("","Over all","","No/Mild periodontitis","","Moderate/Severe periodontitis","",""))
  Table<-rbind(Table,c("Characteristics","Mean/ %*","SE*","Mean/ %","SE","Mean/ %","SE","P†"))
  Table<-rbind(Table,c("No.(Unweighted)",PD.counts[1]+PD.counts[2],"",PD.counts[1],"",PD.counts[2],"",""))
  Table<-rbind(Table,c("No.(Weighted)",Counts_ad,"",PD.counts_ad,"",noPD.counts_ad,"",""))
  #Age
  Table<-rbind(Table,c("Age (years), mean",
                       Table1["Age Mean ± SE","Mean_all"],Table1["Age Mean ± SE","SE_all"],
                       Table1["Age Mean ± SE","Mean_noPD"],Table1["Age Mean ± SE","SE_noPD"],
                       Table1["Age Mean ± SE","Mean_PD"],Table1["Age Mean ± SE","SE_PD"],
                       Table1["Age Mean ± SE","P.value"] ))
  Table<-rbind(Table,c("Age status, %","","","","","","",Table1["Age_status <45","P.value"]))
  Table<-rbind(Table,c("<45",
                       Table1["Age_status <45","Mean_all"],Table1["Age_status <45","SE_all"],
                       Table1["Age_status <45","Mean_noPD"],Table1["Age_status <45","SE_noPD"],
                       Table1["Age_status <45","Mean_PD"],Table1["Age_status <45","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("[45, 65)",
                       Table1["Age_status [45,65)","Mean_all"],Table1["Age_status [45,65)","SE_all"],
                       Table1["Age_status [45,65)","Mean_noPD"],Table1["Age_status [45,65)","SE_noPD"],
                       Table1["Age_status [45,65)","Mean_PD"],Table1["Age_status [45,65)","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("≥65",
                       Table1["Age_status >=65","Mean_all"],Table1["Age_status >=65","SE_all"],
                       Table1["Age_status >=65","Mean_noPD"],Table1["Age_status >=65","SE_noPD"],
                       Table1["Age_status >=65","Mean_PD"],Table1["Age_status >=65","SE_PD"],
                       "" ))
  
  #Sex
  Table<-rbind(Table,c("Sex, Female",
                       Table1["Sex Female","Mean_all"],Table1["Sex Female","SE_all"],
                       Table1["Sex Female","Mean_noPD"],Table1["Sex Female","SE_noPD"],
                       Table1["Sex Female","Mean_PD"],Table1["Sex Female","SE_PD"],
                       Table1["Sex Female","P.value"]))
  
  #Race/ ethnicity
  Table<-rbind(Table,c("Race/ ethnicity, %","","","","","","",Table1["Race_ethnicity Non-Hispanic White","P.value"]))
  Table<-rbind(Table,c("Non-Hispanic white",
                       Table1["Race_ethnicity Non-Hispanic White","Mean_all"],Table1["Race_ethnicity Non-Hispanic White","SE_all"],
                       Table1["Race_ethnicity Non-Hispanic White","Mean_noPD"],Table1["Race_ethnicity Non-Hispanic White","SE_noPD"],
                       Table1["Race_ethnicity Non-Hispanic White","Mean_PD"],Table1["Race_ethnicity Non-Hispanic White","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Non-Hispanic black",
                       Table1["Race_ethnicity Non-Hispanic Black","Mean_all"],Table1["Race_ethnicity Non-Hispanic Black","SE_all"],
                       Table1["Race_ethnicity Non-Hispanic Black","Mean_noPD"],Table1["Race_ethnicity Non-Hispanic Black","SE_noPD"],
                       Table1["Race_ethnicity Non-Hispanic Black","Mean_PD"],Table1["Race_ethnicity Non-Hispanic Black","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Hispanic",
                       Table1["Race_ethnicity Hispanic","Mean_all"],Table1["Race_ethnicity Hispanic","SE_all"],
                       Table1["Race_ethnicity Hispanic","Mean_noPD"],Table1["Race_ethnicity Hispanic","SE_noPD"],
                       Table1["Race_ethnicity Hispanic","Mean_PD"],Table1["Race_ethnicity Hispanic","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Other race/ ethnicity",
                       Table1["Race_ethnicity Other_Race","Mean_all"],Table1["Race_ethnicity Other_Race","SE_all"],
                       Table1["Race_ethnicity Other_Race","Mean_noPD"],Table1["Race_ethnicity Other_Race","SE_noPD"],
                       Table1["Race_ethnicity Other_Race","Mean_PD"],Table1["Race_ethnicity Other_Race","SE_PD"],
                       "" ))
  #Marital status
  Table<-rbind(Table,c("Marital status, %","","","","","","",Table1["Marital_status Married","P.value"]))
  Table<-rbind(Table,c("Married",
                       Table1["Marital_status Married","Mean_all"],Table1["Marital_status Married","SE_all"],
                       Table1["Marital_status Married","Mean_noPD"],Table1["Marital_status Married","SE_noPD"],
                       Table1["Marital_status Married","Mean_PD"],Table1["Marital_status Married","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Never married",
                       Table1["Marital_status Never_married","Mean_all"],Table1["Marital_status Never_married","SE_all"],
                       Table1["Marital_status Never_married","Mean_noPD"],Table1["Marital_status Never_married","SE_noPD"],
                       Table1["Marital_status Never_married","Mean_PD"],Table1["Marital_status Never_married","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Widowed/ Divorced/ Separated",
                       Table1["Marital_status Separated","Mean_all"],Table1["Marital_status Separated","SE_all"],
                       Table1["Marital_status Separated","Mean_noPD"],Table1["Marital_status Separated","SE_noPD"],
                       Table1["Marital_status Separated","Mean_PD"],Table1["Marital_status Separated","SE_PD"],
                       "" ))
  
  #Socioeconomic Status
  Table<-rbind(Table,c("Socioeconomic Status, %","","","","","","",Table1["SES low","P.value"]))
  Table<-rbind(Table,c("Low",
                       Table1["SES low","Mean_all"],Table1["SES low","SE_all"],
                       Table1["SES low","Mean_noPD"],Table1["SES low","SE_noPD"],
                       Table1["SES low","Mean_PD"],Table1["SES low","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Medium",
                       Table1["SES medium","Mean_all"],Table1["SES medium","SE_all"],
                       Table1["SES medium","Mean_noPD"],Table1["SES medium","SE_noPD"],
                       Table1["SES medium","Mean_PD"],Table1["SES medium","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("High",
                       Table1["SES high","Mean_all"],Table1["SES high","SE_all"],
                       Table1["SES high","Mean_noPD"],Table1["SES high","SE_noPD"],
                       Table1["SES high","Mean_PD"],Table1["SES high","SE_PD"],
                       "" ))
  
  #Smoking status
  Table<-rbind(Table,c("Smoking status, %","","","","","","",Table1["Smoking_status Never_smoker","P.value"]))
  Table<-rbind(Table,c("Never smoker",
                       Table1["Smoking_status Never_smoker","Mean_all"],Table1["Smoking_status Never_smoker","SE_all"],
                       Table1["Smoking_status Never_smoker","Mean_noPD"],Table1["Smoking_status Never_smoker","SE_noPD"],
                       Table1["Smoking_status Never_smoker","Mean_PD"],Table1["Smoking_status Never_smoker","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Former smoker",
                       Table1["Smoking_status Former_smoker","Mean_all"],Table1["Smoking_status Former_smoker","SE_all"],
                       Table1["Smoking_status Former_smoker","Mean_noPD"],Table1["Smoking_status Former_smoker","SE_noPD"],
                       Table1["Smoking_status Former_smoker","Mean_PD"],Table1["Smoking_status Former_smoker","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Current smoker",
                       Table1["Smoking_status Current_smoker","Mean_all"],Table1["Smoking_status Current_smoker","SE_all"],
                       Table1["Smoking_status Current_smoker","Mean_noPD"],Table1["Smoking_status Current_smoker","SE_noPD"],
                       Table1["Smoking_status Current_smoker","Mean_PD"],Table1["Smoking_status Current_smoker","SE_PD"],
                       "" ))
  
  #Drinking status
  Table<-rbind(Table,c("Drinking status, %","","","","","","",Table1["Drinking_status Nondrinker","P.value"]))
  Table<-rbind(Table,c("Nondrinker",
                       Table1["Drinking_status Nondrinker","Mean_all"],Table1["Drinking_status Nondrinker","SE_all"],
                       Table1["Drinking_status Nondrinker","Mean_noPD"],Table1["Drinking_status Nondrinker","SE_noPD"],
                       Table1["Drinking_status Nondrinker","Mean_PD"],Table1["Drinking_status Nondrinker","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Light/ moderate drinker",
                       Table1["Drinking_status Light/moderate_drinker","Mean_all"],Table1["Drinking_status Light/moderate_drinker","SE_all"],
                       Table1["Drinking_status Light/moderate_drinker","Mean_noPD"],Table1["Drinking_status Light/moderate_drinker","SE_noPD"],
                       Table1["Drinking_status Light/moderate_drinker","Mean_PD"],Table1["Drinking_status Light/moderate_drinker","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Heavier drinker",
                       Table1["Drinking_status Heavier_drinker","Mean_all"],Table1["Drinking_status Heavier_drinker","SE_all"],
                       Table1["Drinking_status Heavier_drinker","Mean_noPD"],Table1["Drinking_status Heavier_drinker","SE_noPD"],
                       Table1["Drinking_status Heavier_drinker","Mean_PD"],Table1["Drinking_status Heavier_drinker","SE_PD"],
                       "" ))
  #Physical status
  Table<-rbind(Table,c("Physical status, %","","","","","","",Table1["Physical_status Inactive","P.value"]))
  Table<-rbind(Table,c("Inactive",
                       Table1["Physical_status Inactive","Mean_all"],Table1["Physical_status Inactive","SE_all"],
                       Table1["Physical_status Inactive","Mean_noPD"],Table1["Physical_status Inactive","SE_noPD"],
                       Table1["Physical_status Inactive","Mean_PD"],Table1["Physical_status Inactive","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Insufficient",
                       Table1["Physical_status Insufficient","Mean_all"],Table1["Physical_status Insufficient","SE_all"],
                       Table1["Physical_status Insufficient","Mean_noPD"],Table1["Physical_status Insufficient","SE_noPD"],
                       Table1["Physical_status Insufficient","Mean_PD"],Table1["Physical_status Insufficient","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Recommended",
                       Table1["Physical_status Recommended","Mean_all"],Table1["Physical_status Recommended","SE_all"],
                       Table1["Physical_status Recommended","Mean_noPD"],Table1["Physical_status Recommended","SE_noPD"],
                       Table1["Physical_status Recommended","Mean_PD"],Table1["Physical_status Recommended","SE_PD"],
                       "" ))
  #BMI
  Table<-rbind(Table,c("BMI, Mean",
                       Table1["BMI Mean ± SE","Mean_all"],Table1["BMI Mean ± SE","SE_all"],
                       Table1["BMI Mean ± SE","Mean_noPD"],Table1["BMI Mean ± SE","SE_noPD"],
                       Table1["BMI Mean ± SE","Mean_PD"],Table1["BMI Mean ± SE","SE_PD"],
                       Table1["BMI Mean ± SE","P.value"] ))
  
  Table<-rbind(Table,c("BMI status (kg/m2), %","","","","","","",Table1["BMI_status (0,25)","P.value"]))
  Table<-rbind(Table,c("<25",
                       Table1["BMI_status (0,25)","Mean_all"],Table1["BMI_status (0,25)","SE_all"],
                       Table1["BMI_status (0,25)","Mean_noPD"],Table1["BMI_status (0,25)","SE_noPD"],
                       Table1["BMI_status (0,25)","Mean_PD"],Table1["BMI_status (0,25)","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("[25.0 -30)",
                       Table1["BMI_status [25.0-30)","Mean_all"],Table1["BMI_status [25.0-30)","SE_all"],
                       Table1["BMI_status [25.0-30)","Mean_noPD"],Table1["BMI_status [25.0-30)","SE_noPD"],
                       Table1["BMI_status [25.0-30)","Mean_PD"],Table1["BMI_status [25.0-30)","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("≥30",
                       Table1["BMI_status [30,inf)","Mean_all"],Table1["BMI_status [30,inf)","SE_all"],
                       Table1["BMI_status [30,inf)","Mean_noPD"],Table1["BMI_status [30,inf)","SE_noPD"],
                       Table1["BMI_status [30,inf)","Mean_PD"],Table1["BMI_status [30,inf)","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Comorbidities, %","","","","","","",""))
  #Hypertension
  Table<-rbind(Table,c("Hypertension",
                       Table1["HTN_status YES","Mean_all"],Table1["HTN_status YES","SE_all"],
                       Table1["HTN_status YES","Mean_noPD"],Table1["HTN_status YES","SE_noPD"],
                       Table1["HTN_status YES","Mean_PD"],Table1["HTN_status YES","SE_PD"],
                       Table1["HTN_status YES","P.value"]))
  #Hyperlipidemia
  Table<-rbind(Table,c("Hyperlipidemia",
                       Table1["HPL_status YES","Mean_all"],Table1["HPL_status YES","SE_all"],
                       Table1["HPL_status YES","Mean_noPD"],Table1["HPL_status YES","SE_noPD"],
                       Table1["HPL_status YES","Mean_PD"],Table1["HPL_status YES","SE_PD"],
                       Table1["HPL_status YES","P.value"]))
  #Diabetes mellitus
  Table<-rbind(Table,c("Diabetes mellitus",
                       Table1["T2D_status YES","Mean_all"],Table1["T2D_status YES","SE_all"],
                       Table1["T2D_status YES","Mean_noPD"],Table1["T2D_status YES","SE_noPD"],
                       Table1["T2D_status YES","Mean_PD"],Table1["T2D_status YES","SE_PD"],
                       Table1["T2D_status YES","P.value"]))
  #Cohort
  Table<-rbind(Table,c("Cohort period, %","","","","","","",Table1["Cohort NHANES_CON1","P.value"]))
  
  Table<-rbind(Table,c("NHANES III",
                       Table1["Cohort NHANES_III","Mean_all"],Table1["Cohort NHANES_III","SE_all"],
                       Table1["Cohort NHANES_III","Mean_noPD"],Table1["Cohort NHANES_III","SE_noPD"],
                       Table1["Cohort NHANES_III","Mean_PD"],Table1["Cohort NHANES_III","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("NHANES 1999-2004",
                       Table1["Cohort NHANES_CON1","Mean_all"],Table1["Cohort NHANES_CON1","SE_all"],
                       Table1["Cohort NHANES_CON1","Mean_noPD"],Table1["Cohort NHANES_CON1","SE_noPD"],
                       Table1["Cohort NHANES_CON1","Mean_PD"],Table1["Cohort NHANES_CON1","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("NHANES 2009-2014",
                       Table1["Cohort NHANES_CON2","Mean_all"],Table1["Cohort NHANES_CON2","SE_all"],
                       Table1["Cohort NHANES_CON2","Mean_noPD"],Table1["Cohort NHANES_CON2","SE_noPD"],
                       Table1["Cohort NHANES_CON2","Mean_PD"],Table1["Cohort NHANES_CON2","SE_PD"],
                       "" ))
  
  Table_S14<-Table
  write.table(Table_S14,sep = ",",file ="I:/paper_8_PD&MDD/result/Supplementary Table 14.csv" ,row.names =F,col.names =F )
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 30 Complete data COX (Table S15) ####  
load(file="I:/paper_8_PD&MDD/data/Complete_weighted.Rdata")
Interpolation_weighted<-Complete_weighted
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all model1
  model1<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result<-rbind(result,result2)
  #all model3
  model3<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result<-rbind(result,result3)
  result
  #all model4
  model4<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result_All<-rbind(result,result4)
  result_All
}

{ #* CVD model #####
  
  #CVD Crude
  model1<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result<-rbind(result,result2)
  result
  #CVD model2
  model3<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result<-rbind(result,result3)
  result
  #CVD model3
  model4<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result_CVD<-rbind(result,result4)
  result_CVD
  
}

{ #* Cancer model #####
  
  #Cancer model1
  model1<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model2
  model2<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result<-rbind(result,result2)
  result
  #Cancer model3
  model3<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status,design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result<-rbind(result,result3)
  result
  #Cancer model4
  model4<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result_Cancer<-rbind(result,result4)
  result_Cancer
}



{ #* Combine #####
  #Results
  result.cause<-rbind(result_All,result_CVD,result_Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="10000"]<-"1.000"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause
  #Events
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts
  
  #PD_MDD
  PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
  PD_MDD$year<-PD_MDD$peryear
  PD_MDD$Pyear<-PD_MDD$year*PD_MDD$weight
  PD_MDD_death_All<-PD_MDD[which(PD_MDD$MORT_stat==1),]
  PD_MDD_death_CVD<-PD_MDD[which(PD_MDD$CVD_MORT_stat==1),]
  PD_MDD_death_Cancer<-PD_MDD[which(PD_MDD$Cancer_MORT_stat==1),]
  
  #noPD_MDD
  noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
  noPD_MDD$year<-noPD_MDD$peryear
  noPD_MDD$Pyear<-noPD_MDD$year*noPD_MDD$weight
  noPD_MDD_death_All<-noPD_MDD[which(noPD_MDD$MORT_stat==1),]
  noPD_MDD_death_CVD<-noPD_MDD[which(noPD_MDD$CVD_MORT_stat==1),]
  noPD_MDD_death_Cancer<-noPD_MDD[which(noPD_MDD$Cancer_MORT_stat==1),]
  
  #all.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  
  all.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #CVD.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  CVD.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #Cancer.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  Cancer.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  total.counts_ad<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts_ad
  
  #Cbind
  Table<-c("","Periodontal status","","")
  Table<-rbind(Table,c("", "No/Mild periodontitis","Moderate/Severe periodontitis","P"))
  Table<-rbind(Table,c("All cause*","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[1,1],total.counts[1,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[1,1],total.counts_ad[1,2],""))
  Table<-rbind(Table,c("Model 1†","1.00 [Reference]",paste0(result.cause[1,1]," (",result.cause[1,2],", ",result.cause[1,3],")"),result.cause[1,4]))
  Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",paste0(result.cause[2,1]," (",result.cause[2,2],", ",result.cause[2,3],")"),result.cause[2,4]))
  Table<-rbind(Table,c("Model 3§","1.00 [Reference]",paste0(result.cause[3,1]," (",result.cause[3,2],", ",result.cause[3,3],")"),result.cause[3,4]))
  Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",paste0(result.cause[4,1]," (",result.cause[4,2],", ",result.cause[4,3],")"),result.cause[4,4]))
  Table<-rbind(Table,c("CVD cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[2,1],total.counts[2,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[2,1],total.counts_ad[2,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[5,1]," (",result.cause[5,2],", ",result.cause[5,3],")"),result.cause[5,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[6,1]," (",result.cause[6,2],", ",result.cause[6,3],")"),result.cause[6,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[7,1]," (",result.cause[7,2],", ",result.cause[7,3],")"),result.cause[7,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[8,1]," (",result.cause[8,2],", ",result.cause[8,3],")"),result.cause[8,4]))
  Table<-rbind(Table,c("Cancer cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[3,1],total.counts[3,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[3,1],total.counts_ad[3,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[9,1]," (",result.cause[9,2],", ",result.cause[9,3],")"),result.cause[9,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[10,1]," (",result.cause[10,2],", ",result.cause[10,3],")"),result.cause[10,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[11,1]," (",result.cause[11,2],", ",result.cause[11,3],")"),result.cause[11,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[12,1]," (",result.cause[12,2],", ",result.cause[12,3],")"),result.cause[12,4]))
  Table<-as.data.frame(Table)
  Table_S15 = data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  Table_S15
  write.table(Table_S15,sep = ",",file ="I:/paper_8_PD&MDD/result/Supplementary Table 15.csv",row.names =F,col.names =F )
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 31 omitted NHANES III (Table S16) ####  
load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
Interpolation_weighted<-subset(Interpolation_weighted,Cohort!="NHANES_III")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

{ #* all model #####
  #all model1
  model1<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result<-rbind(result,result2)
  #all model3
  model3<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result<-rbind(result,result3)
  result
  #all model4
  model4<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result_All<-rbind(result,result4)
  result_All
}

{ #* CVD model #####
  
  #CVD Crude
  model1<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result<-rbind(result,result2)
  result
  #CVD model2
  model3<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result<-rbind(result,result3)
  result
  #CVD model3
  model4<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result_CVD<-rbind(result,result4)
  result_CVD
  
}

{ #* Cancer model #####
  
  #Cancer model1
  model1<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model2
  model2<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result<-rbind(result,result2)
  result
  #Cancer model3
  model3<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status,design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result<-rbind(result,result3)
  result
  #Cancer model4
  model4<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result_Cancer<-rbind(result,result4)
  result_Cancer
}



{ #* Combine #####
  #Results
  result.cause<-rbind(result_All,result_CVD,result_Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="10000"]<-"1.000"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause
  #Events
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts
  
  #PD_MDD
  PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
  PD_MDD$year<-PD_MDD$peryear
  PD_MDD$Pyear<-PD_MDD$year*PD_MDD$weight
  PD_MDD_death_All<-PD_MDD[which(PD_MDD$MORT_stat==1),]
  PD_MDD_death_CVD<-PD_MDD[which(PD_MDD$CVD_MORT_stat==1),]
  PD_MDD_death_Cancer<-PD_MDD[which(PD_MDD$Cancer_MORT_stat==1),]
  
  #noPD_MDD
  noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
  noPD_MDD$year<-noPD_MDD$peryear
  noPD_MDD$Pyear<-noPD_MDD$year*noPD_MDD$weight
  noPD_MDD_death_All<-noPD_MDD[which(noPD_MDD$MORT_stat==1),]
  noPD_MDD_death_CVD<-noPD_MDD[which(noPD_MDD$CVD_MORT_stat==1),]
  noPD_MDD_death_Cancer<-noPD_MDD[which(noPD_MDD$Cancer_MORT_stat==1),]
  
  #all.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  
  all.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #CVD.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  CVD.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #Cancer.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  Cancer.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  total.counts_ad<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts_ad
  
  #Cbind
  Table<-c("","Periodontal status","","")
  Table<-rbind(Table,c("", "No/Mild periodontitis","Moderate/Severe periodontitis","P"))
  Table<-rbind(Table,c("All cause*","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[1,1],total.counts[1,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[1,1],total.counts_ad[1,2],""))
  Table<-rbind(Table,c("Model 1†","1.00 [Reference]",paste0(result.cause[1,1]," (",result.cause[1,2],", ",result.cause[1,3],")"),result.cause[1,4]))
  Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",paste0(result.cause[2,1]," (",result.cause[2,2],", ",result.cause[2,3],")"),result.cause[2,4]))
  Table<-rbind(Table,c("Model 3§","1.00 [Reference]",paste0(result.cause[3,1]," (",result.cause[3,2],", ",result.cause[3,3],")"),result.cause[3,4]))
  Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",paste0(result.cause[4,1]," (",result.cause[4,2],", ",result.cause[4,3],")"),result.cause[4,4]))
  Table<-rbind(Table,c("CVD cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[2,1],total.counts[2,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[2,1],total.counts_ad[2,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[5,1]," (",result.cause[5,2],", ",result.cause[5,3],")"),result.cause[5,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[6,1]," (",result.cause[6,2],", ",result.cause[6,3],")"),result.cause[6,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[7,1]," (",result.cause[7,2],", ",result.cause[7,3],")"),result.cause[7,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[8,1]," (",result.cause[8,2],", ",result.cause[8,3],")"),result.cause[8,4]))
  Table<-rbind(Table,c("Cancer cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[3,1],total.counts[3,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[3,1],total.counts_ad[3,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[9,1]," (",result.cause[9,2],", ",result.cause[9,3],")"),result.cause[9,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[10,1]," (",result.cause[10,2],", ",result.cause[10,3],")"),result.cause[10,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[11,1]," (",result.cause[11,2],", ",result.cause[11,3],")"),result.cause[11,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[12,1]," (",result.cause[12,2],", ",result.cause[12,3],")"),result.cause[12,4]))
  Table<-as.data.frame(Table)
  Table_S16 = data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  Table_S16
  write.table(Table_S16,sep = ",",file ="I:/paper_8_PD&MDD/result/Supplementary Table 16.csv",row.names =F,col.names =F )
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# +++++++++++++++++++Figures++++++++++++++++++++++++++ ####
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 32 RCS for CAL and PPD (Figure S4) ####
setwd("I:/paper_8_PD&MDD/result")
load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
RCS <- Interpolation_weighted
{#* All cause mortarity ####
  {#** CAL_mean ####
    pdf("Supplementary Figure 4A.pdf",width=6,height=5)
    ddist<-datadist(RCS)
    options(datadist="ddist")
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    RCS$MORT_stat<-as.numeric(RCS$MORT_stat)
    colnames(RCS)
    for (i in 3:7) {
      fit <- cph(Surv(peryear,MORT_stat) ~ rcs(CAL_mean,i)+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                   Smoking_status+Drinking_status+BMI_status+Physical_status+
                   HTN_status+T2D_status+HPL_status+Cohort,data=RCS, weight= weight,family = binomial())
      tmp <- extractAIC(fit)
      if(i == 3) {AIC = tmp[2]; nk = 3}
      if(tmp[2] < AIC) {AIC = tmp[2]; nk = i} 
    }
    nk
    i
    fit <-  cph(Surv(peryear,MORT_stat)~rcs(CAL_mean,3)+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                  Smoking_status+Drinking_status+BMI_status+Physical_status+
                  HTN_status+T2D_status+HPL_status+Cohort,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.207
    
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,CAL_mean,ref.zero=TRUE,fun=exp)
    violet <- "#EFC000E5"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$CAL_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue + 0.5, 1.5, paste0("ref value = ","1.207")) 
    legend("topright",
           paste0(
             "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  
  {#** PPD_mean ####
    load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
    RCS <- Interpolation_weighted
    colnames(RCS)
    limUp<-3*IQR(RCS[,"PPD_mean"],na.rm = T)+quantile(RCS[,"PPD_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"PPD_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"PPD_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"PPD_mean"]>=limUp&!RCS[,"PPD_mean"]<=limDown,]
    # limUp<-3*IQR(RCS[,18],na.rm = T)+quantile(RCS[,18],3/4,na.rm=T,names=F)
    # limDown<-quantile(RCS[,18],1/4,na.rm=T,names=F)-3*IQR(RCS[,18],na.rm = T)
    # RCS<- RCS[!RCS[,18]>=limUp&!RCS[,18]<=limDown,]
    
    pdf("Supplementary Figure 4D.pdf",width=6,height=5)
    colnames(RCS)
    RCS$MORT_stat<-as.numeric(RCS$MORT_stat)
    colnames(RCS)
    for (i in 3:7) {
      fit <- cph(Surv(peryear,MORT_stat) ~ rcs(PPD_mean,i)+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                   Smoking_status+Drinking_status+BMI_status+Physical_status+
                   HTN_status+T2D_status+HPL_status+Cohort,data=RCS, weight= weight,family = binomial())
      tmp <- extractAIC(fit)
      if(i == 3) {AIC = tmp[2]; nk = 3}
      if(tmp[2] < AIC) {AIC = tmp[2]; nk = i} 
    }
    nk
    i
    fit <-  cph(Surv(peryear,MORT_stat)~rcs(PPD_mean,3)+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                  Smoking_status+Drinking_status+BMI_status+Physical_status+
                  HTN_status+T2D_status+HPL_status+Cohort,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 1.316
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,PPD_mean,ref.zero=TRUE,fun=exp)
    violet <- "#0073C2E5"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$PPD_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean periodontal probing depth",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    points(3.27,1,pch=16,cex=1)
    text(refvalue, 1.2, paste0("ref value = ","1.316")) 
    legend("topright",
           paste0(
             "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    dev.off()
  }
  
}
{#* CVD mortarity ####
  load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
  RCS <- Interpolation_weighted
  RCS$CVD_MORT_stat<-as.character(RCS$CVD_MORT_stat)
  RCS$CVD_MORT_stat<-as.numeric(RCS$CVD_MORT_stat)
  {#** CAL_mean ####
    pdf("Supplementary Figure 4B.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,CVD_MORT_stat==1)~rcs(CAL_mean,3)+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                  Smoking_status+Drinking_status+BMI_status+Physical_status+
                  HTN_status+T2D_status+HPL_status+Cohort,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.215
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,CAL_mean,ref.zero=TRUE,fun=exp)
    violet <- "#EFC000E5"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$CAL_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue, 3, paste0("ref value = ","1.215")) 
    legend("topright",
           paste0(
             "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  
  {#** PPD_mean ####
    load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
    RCS <- Interpolation_weighted
    RCS$CVD_MORT_stat<-as.character(RCS$CVD_MORT_stat)
    RCS$CVD_MORT_stat<-as.numeric(RCS$CVD_MORT_stat)
    colnames(RCS)

    limUp<-3*IQR(RCS[,"PPD_mean"],na.rm = T)+quantile(RCS[,"PPD_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"PPD_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"PPD_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"PPD_mean"]>=limUp&!RCS[,"PPD_mean"]<=limDown,]
    # limUp<-3*IQR(RCS[,18],na.rm = T)+quantile(RCS[,18],3/4,na.rm=T,names=F)
    # limDown<-quantile(RCS[,18],1/4,na.rm=T,names=F)-3*IQR(RCS[,18],na.rm = T)
    # RCS<- RCS[!RCS[,18]>=limUp&!RCS[,18]<=limDown,]
    
    pdf("Supplementary Figure 4E.pdf",width=6,height=5)
    
    fit <-  cph(Surv(peryear,CVD_MORT_stat==1)~rcs(PPD_mean,3)+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                  Smoking_status+Drinking_status+BMI_status+Physical_status+
                  HTN_status+T2D_status+HPL_status+Cohort,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 1.316
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,PPD_mean,ref.zero=TRUE,fun=exp)
    violet <- "#0073C2E5"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$PPD_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean periodontal probing depth",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue, 5, paste0("ref value = ","1.316")) 
    legend("topright",
           paste0(
             "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    dev.off()
  }
  
}
{#* Cancer mortarity ####
  load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
  RCS <- Interpolation_weighted
  RCS$Cancer_MORT_stat<-as.character(RCS$Cancer_MORT_stat)
  RCS$Cancer_MORT_stat<-as.numeric(RCS$Cancer_MORT_stat)
  {#** CAL_mean ####
    pdf("Supplementary Figure 4C.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(CAL_mean,4)+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                  Smoking_status+Drinking_status+BMI_status+Physical_status+
                  HTN_status+T2D_status+HPL_status+Cohort,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.215
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,CAL_mean,ref.zero=TRUE,fun=exp)
    violet <- "#EFC000E5"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$CAL_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue+1, 1.2, paste0("ref value = ","1.215")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  
  {#** PPD_mean ####
    load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
    RCS <- Interpolation_weighted
    limUp<-3*IQR(RCS[,"PPD_mean"],na.rm = T)+quantile(RCS[,"PPD_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"PPD_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"PPD_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"PPD_mean"]>=limUp&!RCS[,"PPD_mean"]<=limDown,]
    # limUp<-3*IQR(RCS[,18],na.rm = T)+quantile(RCS[,18],3/4,na.rm=T,names=F)
    # limDown<-quantile(RCS[,18],1/4,na.rm=T,names=F)-3*IQR(RCS[,18],na.rm = T)
    # RCS<- RCS[!RCS[,18]>=limUp&!RCS[,18]<=limDown,]
    
    pdf("Supplementary Figure 4F.pdf",width=6,height=5)
    
    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(PPD_mean,3)+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                  Smoking_status+Drinking_status+BMI_status+Physical_status+
                  HTN_status+T2D_status+HPL_status+Cohort,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 1.330
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,PPD_mean,ref.zero=TRUE,fun=exp)
    violet <- "#0073C2E5"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$PPD_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean periodontal probing depth",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue, 2, paste0("ref value = ","1.330")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    dev.off()
  }
  
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 33 Stratified RCS for CAL and PPD (Figure S4) ####
c("#0073C2FF", "#EFC000FF", "#CD534CFF")
##### CAL #####
  load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
  setwd("I:/paper_8_PD&MDD/result")
  table(Interpolation_weighted$Age_status,useNA = "ifany")
  Interpolation_weighted$Age_sub[Interpolation_weighted$Age_status=="<45"]<-"<45"
  Interpolation_weighted$Age_sub[Interpolation_weighted$Age_status=="[45,65)"|Interpolation_weighted$Age_status==">=65"]<-">=45"
  
  Interpolation_weighted$Age_sub<-factor(Interpolation_weighted$Age_sub,
                                         levels = c("<45",">=45")) 
  table(Interpolation_weighted$Age_sub)
 
  {#** >=45 ####
    RCS <-subset(Interpolation_weighted,Age_sub==">=45")
    RCS$Cancer_MORT_stat<-as.character(RCS$Cancer_MORT_stat)
    RCS$Cancer_MORT_stat<-as.numeric(RCS$Cancer_MORT_stat)
    pdf("Supplementary Figure 5Aa.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(CAL_mean,4)+Sex+Race_ethnicity+SES+Marital_status+
                  Smoking_status+Drinking_status+BMI_status+Physical_status+
                  HTN_status+T2D_status+HPL_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.546
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,CAL_mean,ref.zero=TRUE,fun=exp)
    violet <- "#0073C2FF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$CAL_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue+0.5, 4, paste0("ref value = ","1.546")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  {#** <45 ####
    RCS <-subset(Interpolation_weighted,Age_sub=="<45")
    RCS$Cancer_MORT_stat<-as.character(RCS$Cancer_MORT_stat)
    RCS$Cancer_MORT_stat<-as.numeric(RCS$Cancer_MORT_stat)
    pdf("Supplementary Figure 5B.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(CAL_mean,4)+Sex+Race_ethnicity+SES+Marital_status+
                  Smoking_status+Drinking_status+BMI_status+Physical_status+
                  HTN_status+T2D_status+HPL_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	0.861
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,CAL_mean,ref.zero=TRUE,fun=exp)
    violet <- "#CD534CFF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(8)
    # ??????????????????????????????????????????
    dens <- density(RCS$CAL_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue+0.5, 2, paste0("ref value = ","0.861")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  {#** Male ####
    RCS <-subset(Interpolation_weighted,Sex=="Male")
    RCS$Cancer_MORT_stat<-as.character(RCS$Cancer_MORT_stat)
    RCS$Cancer_MORT_stat<-as.numeric(RCS$Cancer_MORT_stat)
    pdf("Supplementary Figure 5C.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(CAL_mean,4)+Age_status+Race_ethnicity+SES+Marital_status+
                  Smoking_status+Drinking_status+BMI_status+Physical_status+
                  HTN_status+T2D_status+HPL_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.415
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,CAL_mean,ref.zero=TRUE,fun=exp)
    violet <- "#0073C2FF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(10)
    # ??????????????????????????????????????????
    dens <- density(RCS$CAL_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue+0.5, 3, paste0("ref value = ","1.415")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  {#** Female ####
    RCS <-subset(Interpolation_weighted,Sex=="Female")
    RCS$Cancer_MORT_stat<-as.character(RCS$Cancer_MORT_stat)
    RCS$Cancer_MORT_stat<-as.numeric(RCS$Cancer_MORT_stat)
    pdf("Supplementary Figure 5Dd.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(CAL_mean,4)+Age_status+Race_ethnicity+SES+Marital_status+
                  Smoking_status+Drinking_status+BMI_status+Physical_status+
                  HTN_status+T2D_status+HPL_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 1.095
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,CAL_mean,ref.zero=TRUE,fun=exp)
    violet <- "#CD534CFF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(8)
    # ??????????????????????????????????????????
    dens <- density(RCS$CAL_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue+0.5, 2.8, paste0("ref value = ","1.095")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  
  #### PPD ####
  {#** >=45 ####
    RCS <-subset(Interpolation_weighted,Age_sub==">=45")
    RCS$Cancer_MORT_stat<-as.character(RCS$Cancer_MORT_stat)
    RCS$Cancer_MORT_stat<-as.numeric(RCS$Cancer_MORT_stat)
    pdf("Supplementary Figure 5Ee.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"PPD_mean"],na.rm = T)+quantile(RCS[,"PPD_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"PPD_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"PPD_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"PPD_mean"]>=limUp&!RCS[,"PPD_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(PPD_mean,4)+Sex+Race_ethnicity+SES+Marital_status+
                  Smoking_status+Drinking_status+BMI_status+Physical_status+
                  HTN_status+T2D_status+HPL_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.337
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,PPD_mean,ref.zero=TRUE,fun=exp)
    violet <- "#0073C2FF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(10)
    # ??????????????????????????????????????????
    dens <- density(RCS$PPD_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue, 4, paste0("ref value = ","1.337")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  {#** <45 ####
    RCS <-subset(Interpolation_weighted,Age_sub=="<45")
    RCS$Cancer_MORT_stat<-as.character(RCS$Cancer_MORT_stat)
    RCS$Cancer_MORT_stat<-as.numeric(RCS$Cancer_MORT_stat)
    pdf("Supplementary Figure 5F.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"PPD_mean"],na.rm = T)+quantile(RCS[,"PPD_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"PPD_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"PPD_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"PPD_mean"]>=limUp&!RCS[,"PPD_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(PPD_mean,4)+Sex+Race_ethnicity+SES+Marital_status+
                  Smoking_status+Drinking_status+BMI_status+Physical_status+
                  HTN_status+T2D_status+HPL_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.328
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,PPD_mean,ref.zero=TRUE,fun=exp)
    violet <- "#CD534CFF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(10)
    # ??????????????????????????????????????????
    dens <- density(RCS$PPD_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue, 2.5, paste0("ref value = ","1.328")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  {#** Male ####
    RCS <-subset(Interpolation_weighted,Sex=="Male")
    RCS$Cancer_MORT_stat<-as.character(RCS$Cancer_MORT_stat)
    RCS$Cancer_MORT_stat<-as.numeric(RCS$Cancer_MORT_stat)
    pdf("Supplementary Figure 5G.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"PPD_mean"],na.rm = T)+quantile(RCS[,"PPD_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"PPD_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"PPD_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"PPD_mean"]>=limUp&!RCS[,"PPD_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(PPD_mean,4)+Age_status+Race_ethnicity+SES+Marital_status+
                  Smoking_status+Drinking_status+BMI_status+Physical_status+
                  HTN_status+T2D_status+HPL_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.500
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,PPD_mean,ref.zero=TRUE,fun=exp)
    violet <- "#0073C2FF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(10)
    # ??????????????????????????????????????????
    dens <- density(RCS$PPD_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue, 3, paste0("ref value = ","1.500")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  {#** Female ####
    RCS <-subset(Interpolation_weighted,Sex=="Female")
    RCS$Cancer_MORT_stat<-as.character(RCS$Cancer_MORT_stat)
    RCS$Cancer_MORT_stat<-as.numeric(RCS$Cancer_MORT_stat)
    pdf("Supplementary Figure 5H.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"PPD_mean"],na.rm = T)+quantile(RCS[,"PPD_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"PPD_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"PPD_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"PPD_mean"]>=limUp&!RCS[,"PPD_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(PPD_mean,4)+Age_status+Race_ethnicity+SES+Marital_status+
                  Smoking_status+Drinking_status+BMI_status+Physical_status+
                  HTN_status+T2D_status+HPL_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 1.247
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,PPD_mean,ref.zero=TRUE,fun=exp)
    violet <- "#CD534CFF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(10)
    # ??????????????????????????????????????????
    dens <- density(RCS$PPD_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue+0.8, 2.8, paste0("ref value = ","1.247")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }