# Associations of Periodontitis with Risk of All-cause and Cause-specific Mortality Among US Adults with Chronic Kidney Disease 

Code and original data for "Associations of Periodontitis with Risk of All-cause and Cause-specific Mortality Among US Adults with Depression".


## Table of Contents

- [Background](#background)
- [Install](#install)
- [Usage](#usage)
- [Contributing](#contributing)
- [License](#license)

## Background
The association between periodontitis and the risk of all-cause and cause-specific mortality in participants with depression remains unclear.Therefore, this prospective study aimed to examine the association between periodontitis and all-cause and between periodontitis and cause-specific mortality in a nationally representative sample of adults with depression in the United States.


## Install

This module depends upon packages bellow:

```
car
caret
cmprsk
dplyr
foreign
pec
poLCA
plyr
prodlim
ggplot2
ggsci
ggrepel
PSCBS
lava
Matching
mediation
mice
nhanesR
reshape2
rms
riskRegression
survey
scales
survminer
survival
splines
timeROC
tableone
tidyverse
tidyr
```

### Notes
The dependency package 'nhanesR' needs to be installed (https://github.com/shaoyoucheng/nhanesR) and is paid software. We provide the results after the analysis of the software.
## Usage

### 1. PD, CKD and mortality diagnose [[Code]](https://github.com/leescu/PD-MDD_Cohort/blob/main/Codes/Step.%201%20Exposure%26Outcomes.R)
Diagnosis of periodontitis, depression and Mortality.
Notes: The data for the "\data\NHANESIII" path needs to be downloaded from the NHANESIII database.(https://wwwn.cdc.gov/nchs/nhanes/nhanes3/datafiles.aspx#core)
including.
[[adult.dat]](https://wwwn.cdc.gov/nchs/data/nhanes3/1a/adult.dat), [[exam.dat]](https://wwwn.cdc.gov/nchs/data/nhanes3/1a/exam.dat), [[examdr.dat]](https://wwwn.cdc.gov/nchs/data/nhanes3/2a/examdr.dat), [[hei.dat]](https://wwwn.cdc.gov/nchs/data/nhanes3/6a/hei.dat), and [[lab.dat]](https://wwwn.cdc.gov/nchs/data/nhanes3/1a/lab.dat).

### 2. Covariates diagnose [[Code]](https://github.com/leescu/PD-MDD_Cohort/blob/main/Codes/Step.%202%20Covariates.R)
Covariates: age, sex, race or ethnicity, smoking status, alcohol consumption status, socioeconomic status (SES), physical activity status (PA), and body mass index (BMI),etc.

### 4. Analysis [[Code]](https://github.com/leescu/PD-MDD_Cohort/blob/main/Codes/Step.%203%20Arangement.R)
Data arragment before statistical analysis.

### 3. Analysis [[Code]](https://github.com/leescu/PD-MDD_Cohort/blob/main/Codes/Step.%204%20Analysis.R)
Statistical analysis and drawing of figures and tables.

## Contributing

See [the contributing file](CONTRIBUTING.md).

All authors have made substantial contributions to conception and design of the study. Yonghuan Zhang and Weiqi Li have been involved in data analysis, data interpretation, drafting the manuscript and revising it critically. Li Lin and Weiqi Li have been involved in designing the study and revising the manuscript critically. Li Lin and Yonghuan Zhang has given final approval of the version to be published.


## License

[The Apache license 2.0](LICENSE)
