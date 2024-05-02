
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
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 14. Multiple interpolation <<<<< ####
{#* section 14.1 Multiple interpolation ####
  load(file="I:/paper_8_PD&MDD/data/Characters.Rdata")
  colnames(Characters)
  mice_data<-Characters[,11:32]
  #methods(mice)
  mice::md.pattern(mice_data)
  sapply(data, function(x) sum(is.na(mice_data)))
  miss <- function(x){sum(is.na(x))/length(x)*100}
  TableS1<-as.data.frame(apply(mice_data,2,miss))
  TableS1$`apply(mice_data, 2, miss)`<-round(TableS1$`apply(mice_data, 2, miss)`,3)
  Table_S1<-cbind(c("Age",
                  "Period","Sex","Race/ ethnicity","Marital status","Socioeconomic index","Poverty income ratio","Health insurance","Education levels",
                  "Smoking status","Drinking status","Physical status","Healthy eating index","Body mass index, kg/m2",
                  "Hypertension","Diabetes mellitus"),
                  c("continuous", 
                    "categorical","categorical","categorical","categorical","categorical","categorical","categorical",
                  "categorical","categorical","categorical","categorical","categorical","continuous",
                  "categorical","categorical"),
                  TableS1[c("Age",
                            "Cohort","Sex","Race_ethnicity","Marital_status","SEI","PIR","Health_insurance","Education_levels",
                             "Smoking_status","Drinking_status","Physical_status","HEI","BMI",
                             "HTN_status","T2D_status"),1])
  Table_S1[Table_S1==0]<-"No missing values"
  Table_S1
  write.table(Table_S1,sep = ",",file ="I:/paper_8_PD&MDD/result/Supplementary Table 1.csv" ,row.names =F,col.names =F )
  
  init = mice(mice_data,maxit= 10, m=10,seed = 500) 
  stripplot(init)
  save(init,file="I:/paper_8_PD&MDD/data/mice_data.Rdata")
  load(file="I:/paper_8_PD&MDD/data/mice_data.Rdata")
  Interpolation_data <- cbind(Characters[,1:10],complete(init,10))
  load(file="I:/paper_8_PD&MDD/data/Characters.Rdata")
  Original_data<-Characters
  save(Original_data,file="I:/paper_8_PD&MDD/data/Original_data.Rdata")
  rownames(Original_data)<-Original_data$ID
  Original_data$ID<-NULL
  Complete_dat<-na.omit(Characters[,11:32])
  Complete_data<-cbind(Characters[rownames(Complete_dat),1:10],Complete_dat)
  save(Complete_data,file="I:/paper_8_PD&MDD/data/Complete_data.Rdata")
  save(Interpolation_data,file="I:/paper_8_PD&MDD/data/Interpolation_data.Rdata")
  
}
{#* section 14.2 Data restoration ####
  load(file="I:/paper_8_PD&MDD/data/Complete_data.Rdata")
  load(file="I:/paper_8_PD&MDD/data/Original_data.Rdata")
  load(file="I:/paper_8_PD&MDD/data/Interpolation_data.Rdata")
  {#** section 14.2.2 Original data ####
    #AGE
    table(Original_data$Age_status)
    Original_data$Age_status[Original_data$Age<45]<-"<45"
    Original_data$Age_status[Original_data$Age>=45&Original_data$Age<65]<-"[45,65)"
    Original_data$Age_status[Original_data$Age>=65]<-">=65"
    Original_data$Age_status<-factor(Original_data$Age_status,
                                     levels = c("<45","[45,65)",">=65"))
    table(Original_data$Age_status)
    
    #BMI_status
    table(Original_data$BMI_status)
    Original_data$BMI_status[Original_data$BMI<25]<-'(0,25)'
    Original_data$BMI_status[Original_data$BMI>=25&Original_data$BMI<30]<-'[25.0-30)'
    Original_data$BMI_status[Original_data$BMI>=30]<-'[30,inf)' 
    Original_data$BMI_status<-factor(Original_data$BMI_status,
                                     levels = c("(0,25)","[25.0-30)","[30,inf)")) 
    table(Original_data$BMI_status)
    Original_weighted<-Original_data
    save(Original_weighted,file="I:/paper_8_PD&MDD/data/Original_weighted.Rdata")
  }
  {#** section 14.2.1 Interpolation data ####
    #AGE
    table(Interpolation_data$Age_status)
    Interpolation_data$Age_status[Interpolation_data$Age<45]<-"<45"
    Interpolation_data$Age_status[Interpolation_data$Age>=45&Interpolation_data$Age<65]<-"[45,65)"
    Interpolation_data$Age_status[Interpolation_data$Age>=65]<-">=65"
    Interpolation_data$Age_status<-factor(Interpolation_data$Age_status,
                                          levels = c("<45","[45,65)",">=65"))
    table(Interpolation_data$Age_status)
    
    #BMI_status
    table(Interpolation_data$BMI_status)
    Interpolation_data$BMI_status[Interpolation_data$BMI<25]<-'(0,25)'
    Interpolation_data$BMI_status[Interpolation_data$BMI>=25&Interpolation_data$BMI<30]<-'[25.0-30)'
    Interpolation_data$BMI_status[Interpolation_data$BMI>=30]<-'[30,inf)' 
    Interpolation_data$BMI_status<-factor(Interpolation_data$BMI_status,
                                          levels = c("(0,25)","[25.0-30)","[30,inf)")) 
    Interpolation_weighted<-Interpolation_data
    save(Interpolation_weighted,file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
  }
  {#** section 14.2.2 Complete data ####
    #AGE
    table(Complete_data$Age_status)
    Complete_data$Age_status[Complete_data$Age<45]<-"<45"
    Complete_data$Age_status[Complete_data$Age>=45&Complete_data$Age<65]<-"[45,65)"
    Complete_data$Age_status[Complete_data$Age>=65]<-">=65"
    Complete_data$Age_status<-factor(Complete_data$Age_status,
                                     levels = c("<45","[45,65)",">=65"))
    table(Complete_data$Age_status)
    
    
    #BMI_status
    table(Complete_data$BMI_status)
    Complete_data$BMI_status[Complete_data$BMI<25]<-'(0,25)'
    Complete_data$BMI_status[Complete_data$BMI>=25&Complete_data$BMI<30]<-'[25.0-30)'
    Complete_data$BMI_status[Complete_data$BMI>=30]<-'[30,inf)' 
    Complete_data$BMI_status<-factor(Complete_data$BMI_status,
                                     levels = c("(0,25)","[25.0-30)","[30,inf)")) 
    table(Complete_data$BMI_status)
    
    Complete_weighted<-Complete_data
    save(Complete_weighted,file="I:/paper_8_PD&MDD/data/Complete_weighted.Rdata")
  }
  
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#>>>>> section 15. Clasfication of quantile ####  
load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
quantile<-svyquantile(~CAL_mean+PPD_mean, rhcSvy, c(.25,.5,.75))
quantile
summary(Interpolation_weighted$peryear)
quantile[["CAL_mean"]][1]
Interpolation_weighted$CAL_quantile[Interpolation_weighted$CAL_mean<=quantile[["CAL_mean"]][1]]<-"Quantile 1"
Interpolation_weighted$CAL_quantile[Interpolation_weighted$CAL_mean>quantile[["CAL_mean"]][1]&
                                      Interpolation_weighted$CAL_mean<= quantile[["CAL_mean"]][2]]<-"Quantile 2"
Interpolation_weighted$CAL_quantile[Interpolation_weighted$CAL_mean>quantile[["CAL_mean"]][2]&
                                      Interpolation_weighted$CAL_mean<= quantile[["CAL_mean"]][3]]<-"Quantile 3"
Interpolation_weighted$CAL_quantile[Interpolation_weighted$CAL_mean>quantile[["CAL_mean"]][3]]<-"Quantile 4"

table(Interpolation_weighted$CAL_quantile)
Interpolation_weighted$PPD_quantile[Interpolation_weighted$PPD_mean<=quantile[["PPD_mean"]][1]]<-"Quantile 1"
Interpolation_weighted$PPD_quantile[Interpolation_weighted$PPD_mean>quantile[["PPD_mean"]][1]&
                                      Interpolation_weighted$PPD_mean<= quantile[["PPD_mean"]][2]]<-"Quantile 2"
Interpolation_weighted$PPD_quantile[Interpolation_weighted$PPD_mean>quantile[["PPD_mean"]][2]&
                                      Interpolation_weighted$PPD_mean<= quantile[["PPD_mean"]][3]]<-"Quantile 3"
Interpolation_weighted$PPD_quantile[Interpolation_weighted$PPD_mean>quantile[["PPD_mean"]][3]]<-"Quantile 4"

table(Interpolation_weighted$PPD_quantile)

save(Interpolation_weighted,file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 16. Mortality arrangement ####
load(file="I:/paper_8_PD&MDD/data/Original_weighted.Rdata")
load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
load(file="I:/paper_8_PD&MDD/data/Complete_weighted.Rdata")
Interpolation_weighted$MORT_stat<-as.character(Interpolation_weighted$MORT_stat)
Interpolation_weighted$MORT_stat[Interpolation_weighted$MORT_stat=="Alive"]<-0
Interpolation_weighted$MORT_stat[Interpolation_weighted$MORT_stat=="Deceased"]<-1
Interpolation_weighted$MORT_stat<-as.factor(Interpolation_weighted$MORT_stat)
Interpolation_weighted$CVD_MORT_stat[Interpolation_weighted$ucod_leading=="CVD"]<-1
Interpolation_weighted$CVD_MORT_stat[Interpolation_weighted$ucod_leading!="CVD"&Interpolation_weighted$MORT_stat==1]<-2
Interpolation_weighted$CVD_MORT_stat[Interpolation_weighted$MORT_stat==0]<-0

Interpolation_weighted$Cancer_MORT_stat[Interpolation_weighted$ucod_leading=="Cancer"]<-1
Interpolation_weighted$Cancer_MORT_stat[Interpolation_weighted$ucod_leading!="Cancer"&Interpolation_weighted$MORT_stat==1]<-2
Interpolation_weighted$Cancer_MORT_stat[Interpolation_weighted$MORT_stat==0]<-0

Original_weighted$MORT_stat<-as.character(Original_weighted$MORT_stat)
Original_weighted$MORT_stat[Original_weighted$MORT_stat=="Alive"]<-0
Original_weighted$MORT_stat[Original_weighted$MORT_stat=="Deceased"]<-1
Original_weighted$CVD_MORT_stat[Original_weighted$ucod_leading=="CVD"]<-1
Original_weighted$CVD_MORT_stat[Original_weighted$ucod_leading!="CVD"&Original_weighted$MORT_stat==1]<-2
Original_weighted$CVD_MORT_stat[Original_weighted$MORT_stat==0]<-0

Original_weighted$Cancer_MORT_stat[Original_weighted$ucod_leading=="Cancer"]<-1
Original_weighted$Cancer_MORT_stat[Original_weighted$ucod_leading!="Cancer"&Original_weighted$MORT_stat==1]<-2
Original_weighted$Cancer_MORT_stat[Original_weighted$MORT_stat==0]<-0

Complete_weighted$MORT_stat<-as.character(Complete_weighted$MORT_stat)
Complete_weighted$MORT_stat[Complete_weighted$MORT_stat=="Alive"]<-0
Complete_weighted$MORT_stat[Complete_weighted$MORT_stat=="Deceased"]<-1

Complete_weighted$CVD_MORT_stat[Complete_weighted$ucod_leading=="CVD"]<-1
Complete_weighted$CVD_MORT_stat[Complete_weighted$ucod_leading!="CVD"&Complete_weighted$MORT_stat==1]<-2
Complete_weighted$CVD_MORT_stat[Complete_weighted$MORT_stat==0]<-0

Complete_weighted$Cancer_MORT_stat[Complete_weighted$ucod_leading=="Cancer"]<-1
Complete_weighted$Cancer_MORT_stat[Complete_weighted$ucod_leading!="Cancer"&Complete_weighted$MORT_stat==1]<-2
Complete_weighted$Cancer_MORT_stat[Complete_weighted$MORT_stat==0]<-0

save(Original_weighted,file="I:/paper_8_PD&MDD/data/Original_weighted.Rdata")
save(Interpolation_weighted,file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
save(Complete_weighted,file="I:/paper_8_PD&MDD/data/Complete_weighted.Rdata")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 17. latent class analysis (Figure S3) ####
load(file="I:/paper_8_PD&MDD/data/Original_weighted.Rdata")
load(file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
load(file="I:/paper_8_PD&MDD/data/Complete_weighted.Rdata")
Interpolation_SES<-Interpolation_weighted[,c("Education_levels","PIR","Health_insurance","SEI")]
Interpolation_SES$Education_levels<-as.numeric(Interpolation_SES$Education_levels)
Interpolation_SES$PIR<-as.numeric(Interpolation_SES$PIR)
Interpolation_SES$Health_insurance<-as.numeric(Interpolation_SES$Health_insurance)
Interpolation_SES$SEI<-as.numeric(Interpolation_SES$SEI)
set.seed(0)
M1 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 1, maxiter = 10000, nrep = 10, graph = TRUE)
M2 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 2, maxiter = 10000, nrep = 10, graph = TRUE)
M3 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 3, maxiter = 10000, nrep = 10, graph = TRUE)
M4 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 4, maxiter = 10000, nrep = 10, graph = TRUE)
M5 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 5, maxiter = 10000, nrep = 10, graph = F)
Aic_bic<-as.data.frame(rbind(c(M1$aic,M1$bic),c(M2$aic,M2$bic),c(M3$aic,M3$bic),c(M4$aic,M4$bic),c(M5$aic,M5$bic)))
colnames(Aic_bic)<-c("AIC","BIC")
rownames(Aic_bic)<-c("nclass = 1","nclass = 2","nclass = 3","nclass = 4","nclass = 5")
Table_S2<-Aic_bic
posterior <- data.frame(M3$posterior)
posterior$label <- rownames(Interpolation_SES)
posterior$class <- as.character(M3$predclass)
write.table(Table_S2,sep = ",",file ="I:/paper_8_PD&MDD/result/Supplementary Table 2.csv")
names(posterior)[1:2] <- c('class1_probabilities', 'class2_probabilities')
Figure_S3A<-ggplot(posterior,max.overlaps = Inf) +
  geom_point(aes(class1_probabilities, class2_probabilities, color = class),size=2.5,alpha=0.5) +
  theme_bw()+  scale_color_nejm()+ scale_fill_nejm()+ 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
ggsave("I:/paper_8_PD&MDD/result/Supplementary Figure 3A.pdf",Figure_S3A, device = "pdf",width = 8, height = 6, units ="in",
       dpi = 300, limitsize = TRUE)
M3 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 3, maxiter = 10000, nrep = 10, graph = TRUE)
M3_probs <- melt(M3$probs, level = 2)
Figure_S3B<-  ggplot(M3_probs,aes(x = value, y =L2 , fill = Var2)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.5) +
  facet_grid(Var1~.) +
  scale_fill_brewer(type = 'seq', palette = 'Red') +
  theme_bw() +
  labs(x = '', fill = 'probabilities') +
  guides(fill = guide_legend(reverse = TRUE))
Figure_S3B
ggsave("I:/paper_8_PD&MDD/result/Supplementary Figure 3B.pdf",Figure_S3B,device = "pdf", width = 8, height = 6, units ="in",
       dpi = 600, limitsize = TRUE)
Interpolation_weighted$SES[M3$predclass==1]<-"high"
Interpolation_weighted$SES[M3$predclass==2]<-"medium"
Interpolation_weighted$SES[M3$predclass==3]<-"low"
table(Interpolation_weighted$PIR,Interpolation_weighted$SES)
table(Interpolation_weighted$Education_levels,Interpolation_weighted$SES)
table(Interpolation_weighted$SEI,Interpolation_weighted$SES)
Interpolation_weighted$SES<-factor(Interpolation_weighted$SES,
                                   levels = c("low","medium","high"))
save(Interpolation_weighted,file="I:/paper_8_PD&MDD/data/Interpolation_weighted.Rdata")
Original_weighted$SES<-Interpolation_weighted$SES
save(Original_weighted,file="I:/paper_8_PD&MDD/data/Original_weighted.Rdata")
SES<-Interpolation_weighted[,c("ID","SES")]
Complete_weighted<-merge(Complete_weighted,SES,by = "ID",all.x = T)
save(Complete_weighted,file="I:/paper_8_PD&MDD/data/Complete_weighted.Rdata")
