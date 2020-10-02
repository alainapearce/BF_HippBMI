############ Basic Data Load/Setup########
## you may need to install these libraries. 
## to do so type: install.packages('packagename')
library(reporttools)
library(xtable)
library(car)
library(stats)
library(lavaan)
library(psych)
library(childsds)

#set working directory to location of script--not needed when called 
#through Rmarkdown doc. Uncomment below if running locally/manually
# this.dir = getActiveDocumentContext()$path
# setwd(dirname(this.dir))

source('functions.R')
##load datasets
#run if have not compiled the following datasets:
#1)1_CompileData.R
#summary data for task

#load data
BFstructural_Dat = read.csv('Data/BFstructural_Data.csv')

#now have to specify factor because variables with character values no longer defualt to class factor
BFstructural_Dat$sex = factor(BFstructural_Dat$sex)

#ratings
BFstructural_ICCratings = read.csv('Data/MRIstruct_FinalRatings_ICC.csv')


## percent above overweight were generated with code below and package childsds
# BFstructural_Dat$cdc_bmi85 = NA
# BFstructural_Dat$cdc_bmi95 = NA
# 
# for(p in 1:nrow(BFstructural_Dat)){
#   sex_cor = ifelse(BFstructural_Dat$sex[[p]] == 'Female', 'female', 'male')
#   cdc_bmi8595_tab = make_percentile_tab(cdc.ref, item = "bmi",perc = c(85, 95), 
#                                         age = BFstructural_Dat$cAge_yr[p], 
#                                         sex = sex_cor, stack = FALSE)
#   BFstructural_Dat$cdc_bmi85[p] = cdc_bmi8595_tab[cdc_bmi8595_tab$sex == sex_cor, ]$perc_85_0
#   BFstructural_Dat$cdc_bmi95[p] = cdc_bmi8595_tab[cdc_bmi8595_tab$sex == sex_cor, ]$perc_95_0
# }
# 
# BFstructural_Dat$cdc_p85th = BFstructural_Dat$cBodyMass_index/BFstructural_Dat$cdc_bmi85
# BFstructural_Dat$cdc_p95th = BFstructural_Dat$cBodyMass_index/BFstructural_Dat$cdc_bmi95


#####################################
####                            
####    Demo Data - By Sex       ####
####                            
#####################################
##OBstatus
OBstatus_tab = xtabs(~cBodyMass_OBstatus, data = BFstructural_Dat)
OBstatus.sex_tab = xtabs(~cBodyMass_OBstatus + sex, data = BFstructural_Dat)

#chi square test - all numbers are greater than 5
OBstatus.sex_chi = chisq.test(OBstatus.sex_tab)

##Weight Class
WeightClass_tab = xtabs(~cBodyMass_class, data = BFstructural_Dat)
WeightClass.sex_tab = xtabs(~cBodyMass_class + sex, data = BFstructural_Dat)
WeightClass.sex_chi = chisq.test(WeightClass.sex_tab)

##Age
age_mean = mean(BFstructural_Dat$cAge_yr, na.rm = TRUE)
age_sd = sd(BFstructural_Dat$cAge_yr, na.rm = TRUE)
age_range = range(BFstructural_Dat$cAge_yr, na.rm = TRUE)

age_sex_t = t.test(cAge_yr~sex, data = BFstructural_Dat)
age_sex_sd = sd.function.na(BFstructural_Dat, BFstructural_Dat$cAge_yr, BFstructural_Dat$sex)
age_sex_range = range.function.na(BFstructural_Dat, BFstructural_Dat$cAge_yr, BFstructural_Dat$sex)
age_sex_d = cohensD(BFstructural_Dat[BFstructural_Dat$sex == 'Female', ]$cAge_yr, BFstructural_Dat[BFstructural_Dat$sex == 'Male', ]$cAge_yr)

##BMI
BMI_mean = mean(BFstructural_Dat$cBodyMass_index, na.rm = TRUE)
BMI_sd = sd(BFstructural_Dat$cBodyMass_index, na.rm = TRUE)
BMI_range = range(BFstructural_Dat$cBodyMass_index, na.rm = TRUE)

BMI_sex_t = t.test(cBodyMass_index~sex, data = BFstructural_Dat)
BMI_sex_sd = sd.function.na(BFstructural_Dat, BFstructural_Dat$cBodyMass_index, BFstructural_Dat$sex)
BMI_sex_range = range.function.na(BFstructural_Dat, BFstructural_Dat$cBodyMass_index, BFstructural_Dat$sex)
BMI_sex_d = cohensD(BFstructural_Dat[BFstructural_Dat$sex == 'Female', ]$cBodyMass_index, BFstructural_Dat[BFstructural_Dat$sex == 'Male', ]$cBodyMass_index)

##cdc_p85th
cdc_p85th_mean = mean(BFstructural_Dat$cdc_p85th, na.rm = TRUE)
cdc_p85th_sd = sd(BFstructural_Dat$cdc_p85th, na.rm = TRUE)
cdc_p85th_range = range(BFstructural_Dat$cdc_p85th, na.rm = TRUE)

cdc_p85th_sex_t = t.test(cdc_p85th~sex, data = BFstructural_Dat)
cdc_p85th_sex_sd = sd.function.na(BFstructural_Dat, BFstructural_Dat$cdc_p85th, BFstructural_Dat$sex)
cdc_p85th_sex_range = range.function.na(BFstructural_Dat, BFstructural_Dat$cdc_p85th, BFstructural_Dat$sex)
cdc_p85th_sex_d = cohensD(BFstructural_Dat[BFstructural_Dat$sex == 'Female', ]$cdc_p85th, BFstructural_Dat[BFstructural_Dat$sex == 'Male', ]$cdc_p85th)

##BMI percentile
BMIp_mean = mean(BFstructural_Dat$cBodyMass_p, na.rm = TRUE)
BMIp_sd = sd(BFstructural_Dat$cBodyMass_p, na.rm = TRUE)
BMIp_range = range(BFstructural_Dat$cBodyMass_p, na.rm = TRUE)

BMIp_sex_t = t.test(cBodyMass_p~sex, data = BFstructural_Dat)
BMIp_sex_sd = sd.function.na(BFstructural_Dat, BFstructural_Dat$cBodyMass_p, BFstructural_Dat$sex)
BMIp_sex_range = range.function.na(BFstructural_Dat, BFstructural_Dat$cBodyMass_p, BFstructural_Dat$sex)
BMI_sex_d = cohensD(BFstructural_Dat[BFstructural_Dat$sex == 'Female', ]$cBodyMass_p, BFstructural_Dat[BFstructural_Dat$sex == 'Male', ]$cBodyMass_p)


#Ryan, edit below as we can use the the 3cat variable for m & p ed -- can just use fisher.test (*tab) instead of the manual matrix
##mEducation
mED_tab = xtabs(~mEducation, data = BFstructural_Dat)
mED.sex_tab = xtabs(~mEducation + sex, data = BFstructural_Dat)

##you will need to count the number that fall into each category: >BA, BA, Associates/technical, High school, other/NA
##you will get all from the mED.sex_tab above
##enter the numbers by collumn - first all for boys, then girls. You can just put the numbers for the whole sample into the table
##in BreastFeeding_Hipp.Rmd document

#fisher.test - use because one of the entered number is 5 or less
mED.sex_fisher = fisher.test(matrix(c(22, 30, 7, 8, 0, 28, 24, 11, 7, 0), nrow = 5, ncol = 2, byrow = FALSE))

##pEducation
pED_tab = xtabs(~dEducation, data = BFstructural_Dat)
pED.sex_tab = xtabs(~dEducation + sex, data = BFstructural_Dat)
pED.sex_fisher = fisher.test(matrix(c(28, 22, 5, 9, 1, 1, 29, 16, 10, 14, 0, 0), nrow = 6, ncol = 2, byrow = FALSE))

##Sex
sex_tab = xtabs(~sex, data = BFstructural_Dat)

##Race
race_tab = xtabs(~cRace, data = BFstructural_Dat)
race.sex_tab = xtabs(~cRace + sex, data = BFstructural_Dat)
race.sex_fisher = fisher.test(matrix(c(5, 64, 4, 0, 2, 72, 2, 0), nrow = 4, ncol = 2, byrow = FALSE))

##Ethnicity
ethnicity_tab = xtabs(~cEthnicity, data = BFstructural_Dat)
ethnicity.sex_tab = xtabs(~cEthnicity + sex, data = BFstructural_Dat)
ethnicity.sex_fisher = fisher.test(matrix(c(3, 59, 1, 3, 61, 0), nrow = 3, ncol = 2, byrow = FALSE))

##SES
#Ryan, edit below as we can use the the 3cat variable for SES -- can just use fisher.test (*tab) instead of the manual matrix

income_tab = xtabs(~income, data = BFstructural_Dat)
income.sex_tab = xtabs(~income + sex, data = BFstructural_Dat)
income.sex_fisher = fisher.test(matrix(c(26, 30, 16, 0, 23, 39, 12, 0), nrow = 4, ncol = 2, byrow = FALSE))


#### ####
#### ####
BreastFed_3cat_tab = xtabs(~BreastFed_3cat, data = BFstructural_Dat)
BreastFed_3cat.sex_tab = xtabs(~BreastFed_3cat + sex, data = BFstructural_Dat)
BreastFed_3cat.sex_chi = chisq.test(BreastFed_3cat.sex_tab)


##Age 
age_BreastFed_3cat_anova = Anova(lm(cAge_yr~BreastFed_3cat, data = BFstructural_Dat))
age_BreastFed_3cat_mean = means.function(BFstructural_Dat, BFstructural_Dat$cAge_yr, BFstructural_Dat$BreastFed_3cat)
age_BreastFed_3cat_sd = sd.function.na(BFstructural_Dat, BFstructural_Dat$cAge_yr, BFstructural_Dat$BreastFed_3cat)
age_BreastFed_3cat_range = range.function.na(BFstructural_Dat, BFstructural_Dat$cAge_yr, BFstructural_Dat$BreastFed_3cat)

##BMI
BMI_BreastFed_3cat_anova = Anova(lm(cBodyMass_index~BreastFed_3cat, data = BFstructural_Dat))
BMI_BreastFed_3cat_mean = means.function(BFstructural_Dat, BFstructural_Dat$cBodyMass_index, BFstructural_Dat$BreastFed_3cat)
BMI_BreastFed_3cat_sd = sd.function.na(BFstructural_Dat, BFstructural_Dat$cBodyMass_index, BFstructural_Dat$BreastFed_3cat)
BMI_BreastFed_3cat_range = range.function.na(BFstructural_Dat, BFstructural_Dat$cBodyMass_index, BFstructural_Dat$BreastFed_3cat)

##cdc_p85th
cdc_p85th_BreastFed_3cat_anova = Anova(lm(cdc_p85th~BreastFed_3cat, data = BFstructural_Dat))
cdc_p85th_BreastFed_3cat_mean = means.function(BFstructural_Dat, BFstructural_Dat$cdc_p85th, BFstructural_Dat$BreastFed_3cat)
cdc_p85th_BreastFed_3cat_sd = sd.function.na(BFstructural_Dat, BFstructural_Dat$cdc_p85th, BFstructural_Dat$BreastFed_3cat)
cdc_p85th_BreastFed_3cat_range = range.function.na(BFstructural_Dat, BFstructural_Dat$cdc_p85th, BFstructural_Dat$BreastFed_3cat)

##BMI percentile
BMIp_BreastFed_3cat_anova = Anova(lm(cBodyMass_p~BreastFed_3cat, data = BFstructural_Dat))
BMIp_BreastFed_3cat_mean = means.function(BFstructural_Dat, BFstructural_Dat$cBodyMass_p, BFstructural_Dat$BreastFed_3cat)
BMIp_BreastFed_3cat_sd = sd.function.na(BFstructural_Dat, BFstructural_Dat$cBodyMass_p, BFstructural_Dat$BreastFed_3cat)
BMIp_BreastFed_3cat_range = range.function.na(BFstructural_Dat, BFstructural_Dat$cBodyMass_p, BFstructural_Dat$BreastFed_3cat)

##sex
#Ryan, edit below as we can use the the 3cat variable for BF -- can just use fisher.test (*tab) instead of the manual matrix, also, the matrix numbers don't match so you should double check!
sex.BreastFed_3cat_tab = xtabs(~sex + BreastFed_3cat, data = BFstructural_Dat)
sex.BreastFed_3cat_fisher = fisher.test(matrix(c(5, 64, 4, 0, 2, 72, 2, 0), nrow = 4, ncol = 2, byrow = FALSE))

##OBstatus
OBstatus.BreastFed_3cat_tab = xtabs(~cBodyMass_OBstatus + BreastFed_3cat, data = BFstructural_Dat)
OBstatus.BreastFed_3cat_chi = chisq.test(OBstatus.BreastFed_3cat_tab)

##Weight Class
#Ryan, edit below as we can use the the 3cat variable for BF -- can just use fisher.test (*tab) instead of the manual matrix, also, the matrix numbers don't match so you should double check!
WeightClass.BreastFed_3cat_tab = xtabs(~cBodyMass_class + BreastFed_3cat, data = BFstructural_Dat)
WeightClass.BreastFed_3cat_fisher = fisher.test(matrix(c(10, 6, 24, 5, 5, 45, 5, 10, 39), nrow = 3, ncol = 3, byrow = FALSE))

##maternal education
#Ryan, edit below as we can use the the 3cat variable for BF and M & P ed -- can just use fisher.test (*tab) instead of the manual matrix,
mED.BreastFed_3cat_tab = xtabs(~mEducation + BreastFed_3cat, data = BFstructural_Dat)
mED.BreastFed_3cat_fisher = fisher.test(matrix(c(13, 15, 6, 3, 0, 0, 17, 21, 5, 6, 0, 0, 20, 18, 7, 6, 0, 0), nrow = 6, ncol = 3, byrow = FALSE))

##paternal education
#Ryan, edit below as we can use the the 3cat variable for BF  and M & P ed-- can just use fisher.test (*tab) instead of the manual matrix,
pED.BreastFed_3cat_tab = xtabs(~dEducation + BreastFed_3cat, data = BFstructural_Dat)
pED.BreastFed_3cat_fisher = fisher.test(matrix(c(9, 11, 5, 9, 1, 0, 29, 10, 3, 6, 0, 0, 19, 17, 7, 8, 0, 1), nrow = 6, ncol = 3, byrow = FALSE))

##SES income
#Ryan, edit below as we can use the the 3cat variable for BF and SES cat- can just use fisher.test (*tab) instead of the manual matrix,
income.BreastFed_3cat_tab = xtabs(~income + BreastFed_3cat, data = BFstructural_Dat)
income.BreastFed_3cat_fisher = fisher.test(matrix(c(10, 16, 12, 0, 22, 22, 10, 0, 17, 31, 6, 0), nrow = 4, ncol = 3, byrow = FALSE))

##Ethnicity
ethnicity.BreastFed_3cat_tab = xtabs(~cEthnicity + BreastFed_3cat, data = BFstructural_Dat)
ethnicity.BreastFed_3cat_fisher = fisher.test(matrix(c(2, 30, 1, 2, 50, 0, 2, 40, 0), nrow = 3, ncol = 3, byrow = FALSE))

##Race
race.BreastFed_3cat_tab = xtabs(~cRace + BreastFed_3cat, data = BFstructural_Dat)
race.BreastFed_3cat_fisher = fisher.test(matrix(c(2, 35, 3, 0, 1, 53, 1, 0, 4, 48, 2, 0), nrow = 4, ncol = 3, byrow = FALSE))

#####################################
####                            
####    CEBQ alpha  ####
####                            
#####################################
psych::alpha(BFstructural_Dat[c('cebq3_numRev', 'cebq17_num', 'cebq21_num', 'cebq26_num', 'cebq30_num')])

#####################################
####                            
####    Get Rating and Study info  ####
####                            
#####################################

#ICC
wrap_kappa = cohen.kappa(data.frame(BFstructural_ICCratings$Wrap_Ryan, BFstructural_ICCratings$Wrap_Jane))
clip_kappa = cohen.kappa(data.frame(BFstructural_ICCratings$Clipped_Ryan, BFstructural_ICCratings$Clipped_Jane))
ringing_kappa = cohen.kappa(data.frame(BFstructural_ICCratings$Ringing_Ryan, BFstructural_ICCratings$Ringing_Jane))
ghost_kappa = cohen.kappa(data.frame(BFstructural_ICCratings$Ghosting_Ryan, BFstructural_ICCratings$Ghosting_Jane))
blur_kappa = cohen.kappa(data.frame(BFstructural_ICCratings$Blurriness_Ryan, BFstructural_ICCratings$Blurriness_Jane))

wrap_ICC = ICC(data.frame(BFstructural_ICCratings$Wrap_Ryan, BFstructural_ICCratings$Wrap_Jane), missing = TRUE, lmer = TRUE)
clip_ICC = ICC(data.frame(BFstructural_ICCratings$Clipped_Ryan, BFstructural_ICCratings$Clipped_Jane), missing = TRUE, lmer = TRUE)
ringing_ICC = ICC(data.frame(BFstructural_ICCratings$Ringing_Ryan, BFstructural_ICCratings$Ringing_Jane), missing = TRUE, lmer = TRUE)
ghost_ICC = ICC(data.frame(BFstructural_ICCratings$Ghosting_Ryan, BFstructural_ICCratings$Ghosting_Jane), missing = TRUE, lmer = TRUE)
blur_ICC = ICC(data.frame(BFstructural_ICCratings$Blurriness_Ryan, BFstructural_ICCratings$Blurriness_Jane), missing = TRUE, lmer = TRUE)

#1 removed for QC rating >=4
BFstructural_Dat = BFstructural_Dat[BFstructural_Dat$AverageRating < 4, ]

#####################################
####                            
####     Descriptive Stats  ####
####                            
#####################################
BFstructural_HIPProis_corvars = BFstructural_Dat[c(32, 29, 23, 4:6, 2:3)]
BFstructural_HIPProis_cornames = names(BFstructural_Dat)[c(32, 29, 23, 4:6, 2:3)]
BFstructural_HIPProis_cormat = cor.matrix(BFstructural_HIPProis_corvars, BFstructural_HIPProis_cornames)

##Ryan -- add all the analyses you did for Table 2 (that will now be written into sup materials below. Use comments to label each test/sections

#####################################
####                            
####     All Data Path Models Mediation    ####
####     BF -> SR, Hip Med } -> cdc_p85th  ####
####                            
#####################################


## Left  with extra covariates
HippL_BF2SR_p85th_cov_pathmod <-'
# c path
cebq_SR ~ mEducation_dummy + income_dummy + cPreMat_dummy + c*BreastFed_3cat_dummy

# b1/a2 path
cebq_SR ~ TIV + IQR + Study_dummy + cAge_yr + sex + b1a2*lHip_21

# a1 path
lHip_21 ~ TIV + IQR + Study_dummy + cAge_yr + sex + income_dummy + cPreMat_dummy + a1*BreastFed_3cat_dummy

# c2 model
cdc_p85th ~ TIV + IQR + Study_dummy + cAge_yr + sex + c2*lHip_21

# b2 prime model
cdc_p85th ~  mEducation_dummy + income_dummy + cPreMat_dummy + b2*cebq_SR


#indirect effects
lHipMed_BF2SR := a1*b1a2
SRMed_lHip2p85th := b1a2*b2


#total effect
lHip_BF2SR_total := c + lHipMed_BF2SR
SR_lHip2p85th_total := c2 + SRMed_lHip2p85th

#proportion med
lHip_propmed := lHipMed_BF2SR/lHip_BF2SR_total
SR_propmed := SRMed_lHip2p85th/SR_lHip2p85th_total
'

# fit the model
HippL_BF2SR_p85th_cov_fit <- sem(HippL_BF2SR_p85th_cov_pathmod, data = BFstructural_Dat, estimator = "ML")
HippL_BF2SR_p85th_cov_summary <- summary(HippL_BF2SR_p85th_cov_fit, fit.measures = T, rsquare=TRUE)
HippL_BF2SR_p85th_cov_std = standardizedSolution(HippL_BF2SR_p85th_cov_fit, type = 'std.all', output = 'pretty')

# post-hoc for breastfeeding
##Breastfeeding groups t-tests
ttest_0t3mo_g6mo = t.test(lHip_21~BreastFed_3cat, data = BFstructural_Dat[BFstructural_Dat$BreastFed_3cat != '>6mo', ])
ttest_4t6mo_g6mo = t.test(lHip_21~BreastFed_3cat, data = BFstructural_Dat[BFstructural_Dat$BreastFed_3cat != '0-3mo', ])
ttest_0t3mo_g6mo = t.test(lHip_21~BreastFed_3cat, data = BFstructural_Dat[BFstructural_Dat$BreastFed_3cat != '4-6mo', ])

##standard deviation to report with measns
LHipp_BreastFeedingCat_sd = means.function(BFstructural_Dat, BFstructural_Dat$lHip_21, BFstructural_Dat$BreastFed_3cat)

# post-hoc for income and SR
##Income groups t-tests

#copy from breastfeeding post-hoc and change variablenames/categories 

# post-hoc for m education and cdc_p85th

#copy from breastfeeding post-hoc and change variablenames/categories 

## Right  with extra covariates
HippR_BF2SR_p85th_cov_pathmod <-'
# c path
cebq_SR ~ mEducation_dummy + income_dummy + cPreMat_dummy + c*BreastFed_3cat_dummy

# b1/a2 path
cebq_SR ~ TIV + IQR + Study_dummy + cAge_yr + sex + b1a2*rHip_22

# a1 path
rHip_22 ~ TIV + IQR + Study_dummy + cAge_yr + sex + income_dummy + cPreMat_dummy + a1*BreastFed_3cat_dummy

# c2 model
cdc_p85th ~ TIV + IQR + Study_dummy + cAge_yr + sex + c2*rHip_22

# b2 prime model
cdc_p85th ~  mEducation_dummy + income_dummy + cPreMat_dummy + b2*cebq_SR


#indirect effects
rHipMed_BF2SR := a1*b1a2
SRMed_rHip2p85th := b1a2*b2


#total effect
rHip_BF2SR_total := c + rHipMed_BF2SR
SR_rHip2p85th_total := c2 + SRMed_rHip2p85th

#proportion med
rHip_propmed := rHipMed_BF2SR/rHip_BF2SR_total
SR_propmed := SRMed_rHip2p85th/SR_rHip2p85th_total'

# fit the model
HippR_BF2SR_p85th_cov_fit <- sem(HippR_BF2SR_p85th_cov_pathmod, data = BFstructural_Dat, estimator = "ML")
HippR_BF2SR_p85th_cov_summary <- summary(HippR_BF2SR_p85th_cov_fit, fit.measures = T, rsquare=TRUE)
HippR_BF2SR_p85th_cov_std = standardizedSolution(HippR_BF2SR_p85th_cov_fit, type = 'std.all', output = 'pretty')

# post-hoc for breastfeeding
##Breastfeeding groups t-tests
ttest_0t3mo_g6mo = t.test(lHip_21~BreastFed_3cat, data = BFstructural_Dat[BFstructural_Dat$BreastFed_3cat != '>6mo', ])
ttest_4t6mo_g6mo = t.test(lHip_21~BreastFed_3cat, data = BFstructural_Dat[BFstructural_Dat$BreastFed_3cat != '0-3mo', ])
ttest_0t3mo_g6mo = t.test(lHip_21~BreastFed_3cat, data = BFstructural_Dat[BFstructural_Dat$BreastFed_3cat != '4-6mo', ])

##standard deviation to report with measns
LHipp_BreastFeedingCat_sd = means.function(BFstructural_Dat, BFstructural_Dat$lHip_21, BFstructural_Dat$BreastFed_3cat)
