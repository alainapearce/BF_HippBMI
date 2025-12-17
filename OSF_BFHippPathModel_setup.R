############ Basic Data Load/Setup########
## you may need to install these libraries. 
## to do so type: install.packages('packagename')
library(reporttools)
library(xtable)
library(car)
library(stats)
library(lavaan)
library(lsr)
library(psych)
library(childsds)
library(emmeans)
library(reshape2)


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
BFstructural_Dat = read.csv('Data/BFstructural_AllData.csv', na.strings = 'NA')

#now have to specify factor because variables with character values no longer defualt to class factor
BFstructural_Dat$sex = factor(BFstructural_Dat$sex)

#ratings
BFstructural_ICCratings = read.csv('Data/MRIstruct_FinalRatings_ICC.csv')

# percent above overweight were generated with code below and package childsds
BFstructural_Dat$cdc_bmi85 = NA
BFstructural_Dat$cdc_bmi95 = NA

for(p in 1:nrow(BFstructural_Dat)){
  sex_cor = ifelse(BFstructural_Dat$sex[[p]] == 'Female', 'female', 'male')
  cdc_bmi8595_tab = make_percentile_tab(cdc.ref, item = "bmi",perc = c(85, 95),
                                        age = BFstructural_Dat$cAge_yr[p],
                                        sex = sex_cor, stack = FALSE)
  BFstructural_Dat$cdc_bmi85[p] = cdc_bmi8595_tab[cdc_bmi8595_tab$sex == sex_cor, ]$perc_85_0
  BFstructural_Dat$cdc_bmi95[p] = cdc_bmi8595_tab[cdc_bmi8595_tab$sex == sex_cor, ]$perc_95_0
}

BFstructural_Dat$cdc_p85th = BFstructural_Dat$cBodyMass_index/BFstructural_Dat$cdc_bmi85
BFstructural_Dat$cdc_p95th = BFstructural_Dat$cBodyMass_index/BFstructural_Dat$cdc_bmi95

#adjust cRace
BFstructural_Dat$cRace_cat = ifelse(BFstructural_Dat$cRace == 'Black', 'Black/AA', ifelse(BFstructural_Dat$cRace == 'White', 'White', 'Other/Multi'))          
#mark missing in cEthnicity
BFstructural_Dat$cEthnicity_cat = ifelse(BFstructural_Dat$cEthnicity == 'check', NA, as.character(BFstructural_Dat$cEthnicity))     

#####################################
####                            
####    Demo Data - By Sex       ####
####                            
#####################################
##OBstatus
BFstructural_Dat$cBodyMass_OBstatus = ifelse(BFstructural_Dat$cBodyMass_class == 'HW', 'HW', 'OW_OB')
OBstatus_tab = xtabs(~cBodyMass_OBstatus, data = BFstructural_Dat)
OBstatus.sex_tab = xtabs(~cBodyMass_OBstatus + sex, data = BFstructural_Dat)
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

##mEducation
BFstructural_Dat$mEducation_cat = ifelse(BFstructural_Dat$mEducation == "TechnicalDegree" | BFstructural_Dat$mEducation == "AssociatesDegree", "AA/Technical",ifelse(BFstructural_Dat$mEducation == "SomeGraduateSchool" | BFstructural_Dat$mEducation == "GradSchool" | BFstructural_Dat$mEducation == "DoctoralDegree" | BFstructural_Dat$mEducation == "MastersDegree", ">BA", ifelse(BFstructural_Dat$mEducation == "SomeHighSchool" | BFstructural_Dat$mEducation == "HighSchool" | BFstructural_Dat$mEducation == "SomeCollege", 'HighSchool/SomeCollege', as.character(BFstructural_Dat$mEducation))))

mED_tab = xtabs(~mEducation_cat, data = BFstructural_Dat)
mED.sex_tab = xtabs(~mEducation_cat + sex, data = BFstructural_Dat)

mED.sex_fisher = fisher.test(mED.sex_tab)

##pEducation
BFstructural_Dat$dEducation_cat = ifelse(BFstructural_Dat$dEducation == "TechnicalDegree" | BFstructural_Dat$dEducation == "AssociatesDegree", "AA/Technical",ifelse(BFstructural_Dat$dEducation == "SomeGraduateSchool" | BFstructural_Dat$dEducation == "GradSchool" | BFstructural_Dat$dEducation == "DoctoralDegree" | BFstructural_Dat$dEducation == "MastersDegree", ">BA", ifelse(BFstructural_Dat$dEducation == "SomeHighSchool" | BFstructural_Dat$dEducation == "HighSchool" | BFstructural_Dat$dEducation == "SomeCollege", 'HighSchool/SomeCollege', as.character(BFstructural_Dat$dEducation))))

pED_tab = xtabs(~dEducation_cat, data = BFstructural_Dat)
pED.sex_tab = xtabs(~dEducation_cat + sex, data = BFstructural_Dat)
pED.sex_fisher = fisher.test(pED.sex_tab)

##Sex
sex_tab = xtabs(~sex, data = BFstructural_Dat)

##Race
BFstructural_Dat$cRace_cat = ifelse(BFstructural_Dat$cRace == 'Multi-racial' | BFstructural_Dat$cRace == 'Asian', 'Other/Mixed', as.character(BFstructural_Dat$cRace))

race_tab = xtabs(~cRace_cat, data = BFstructural_Dat)
race.sex_tab = xtabs(~cRace_cat + sex, data = BFstructural_Dat)
race.sex_fisher = fisher.test(race.sex_tab)

##Ethnicity
ethnicity_tab = xtabs(~cEthnicity, data = BFstructural_Dat)
ethnicity.sex_tab = xtabs(~cEthnicity + sex, data = BFstructural_Dat)
ethnicity.sex_fisher = fisher.test(ethnicity.sex_tab)

##SES
BFstructural_Dat$income_3cat = ifelse(BFstructural_Dat$income == '$76-100K' | BFstructural_Dat$income == '$51-75K', '$51,000 - $100,000', ifelse(BFstructural_Dat$income == '$36-50K' | BFstructural_Dat$income == '$21-35K' | BFstructural_Dat$income == '<$20K', '< $50,000', as.character(BFstructural_Dat$income)))

income_tab = xtabs(~income_3cat, data = BFstructural_Dat)
income.sex_tab = xtabs(~income_3cat + sex, data = BFstructural_Dat)
income.sex_fisher = fisher.test(income.sex_tab)

#### ####
BFstructural_Dat$BreastFed_3cat = ifelse(BFstructural_Dat$BreastFed_wk == '>24 wks',  '>6mo', ifelse(BFstructural_Dat$BreastFed_wk == '16-24 wks', '4-6mo', ifelse(BFstructural_Dat$BreastFed_wk == '4-12 wks' | BFstructural_Dat$BreastFed_wk == 'Never', '0-3mo', ifelse(as.numeric(BFstructural_Dat$BreastFed_wk) > 24, '>6mo', ifelse(as.numeric(BFstructural_Dat$BreastFed_wk) >= 16, '4-6mo', ifelse(as.numeric(BFstructural_Dat$BreastFed_wk) < 16, '0-3mo', as.character(BFstructural_Dat$BreastFed_wk)))))))

BreastFed_3cat_tab = xtabs(~BreastFed_3cat, data = BFstructural_Dat)
BreastFed_3cat_sex_tab = xtabs(~BreastFed_3cat + sex, data = BFstructural_Dat)
BreastFed_3cat_sex_chi = chisq.test(BreastFed_3cat_sex_tab)

#####################################
####                            
####    CEBQ alpha  ####
####                            
#####################################
BFstructural_Dat$cebq3_numRev = ifelse(BFstructural_Dat$cebq3_num == 5, 1, ifelse(BFstructural_Dat$cebq3_num == 4, 2, 3))

psych::alpha(BFstructural_Dat[c('cebq3_numRev', 'cebq17_num', 'cebq21_num', 'cebq26_num', 'cebq30_num')])

#####################################
####                            
####    Set up Structural Data  ####
####                            
#####################################
BFstructural_ROIdat_all = read.csv('Data/TPMall_BFstructural_ROI_dat_neuromorphometrics.csv')

BFstructural_ROIdat_hip = BFstructural_ROIdat_all[BFstructural_ROIdat_all$region == 'rHip' | BFstructural_ROIdat_all$region == 'lHip', ]

BFstructural_ROIdat_hip_wide = dcast(BFstructural_ROIdat_hip, parID~region + ids, value.var = 'vgm')

#add QC data 
BFstructural_covars = read.csv('Data/BFstructural_covars.csv')

BFstructural_ROIdat_hip_wide = merge(BFstructural_ROIdat_hip_wide, BFstructural_covars[c(1:4, 5, 17)], by = 'parID')
names(BFstructural_ROIdat_hip_wide)[c(4, 7)] = c('StudyID', 'LabID')

#merge into primary dataset
BFstructural_Dat = merge(BFstructural_Dat, BFstructural_ROIdat_hip_wide, by = c('StudyID', 'LabID'))

#####################################
####                            
####    Get Rating and Study info  ####
####                            
#####################################

#ICC
wrap_kappa = cohen.kappa(data.frame(BFstructural_ICCratings$Wrap_R1, BFstructural_ICCratings$Wrap_R2))
clip_kappa = cohen.kappa(data.frame(BFstructural_ICCratings$Clipped_R1, BFstructural_ICCratings$Clipped_R2))
ringing_kappa = cohen.kappa(data.frame(BFstructural_ICCratings$Ringing_R1, BFstructural_ICCratings$Ringing_R2))
ghost_kappa = cohen.kappa(data.frame(BFstructural_ICCratings$Ghosting_R1, BFstructural_ICCratings$Ghosting_R2))
blur_kappa = cohen.kappa(data.frame(BFstructural_ICCratings$Blurriness_R1, BFstructural_ICCratings$Blurriness_R2))

wrap_ICC = ICC(data.frame(BFstructural_ICCratings$Wrap_R1, BFstructural_ICCratings$Wrap_R2), missing = TRUE, lmer = TRUE)
clip_ICC = ICC(data.frame(BFstructural_ICCratings$Clipped_R1, BFstructural_ICCratings$Clipped_R2), missing = TRUE, lmer = TRUE)
ringing_ICC = ICC(data.frame(BFstructural_ICCratings$Ringing_R1, BFstructural_ICCratings$Ringing_R2), missing = TRUE, lmer = TRUE)
ghost_ICC = ICC(data.frame(BFstructural_ICCratings$Ghosting_R1, BFstructural_ICCratings$Ghosting_R2), missing = TRUE, lmer = TRUE)
blur_ICC = ICC(data.frame(BFstructural_ICCratings$Blurriness_R1, BFstructural_ICCratings$Blurriness_R2), missing = TRUE, lmer = TRUE)

#1 removed for QC rating >=4
BFstructural_Dat = BFstructural_Dat[BFstructural_Dat$AverageRating < 4, ]

#####################################
####                            
####     Descriptive Stats  ####
####                            
#####################################

##Breastfeeding ####

##Age 
age_BreastFed_3cat_anova = Anova(lm(cAge_yr~BreastFed_3cat, data = BFstructural_Dat))
age_BreastFed_3cat_mean = means.function(BFstructural_Dat, BFstructural_Dat$cAge_yr, BFstructural_Dat$BreastFed_3cat)
age_BreastFed_3cat_sd = sd.function.na(BFstructural_Dat, BFstructural_Dat$cAge_yr, BFstructural_Dat$BreastFed_3cat)
age_BreastFed_3cat_range = range.function.na(BFstructural_Dat, BFstructural_Dat$cAge_yr, BFstructural_Dat$BreastFed_3cat)

##TIV 
age_BreastFed_3cat_anova = Anova(lm(TIV~BreastFed_3cat, data = BFstructural_Dat))
age_BreastFed_3cat_mean = means.function(BFstructural_Dat, BFstructural_Dat$TIV, BFstructural_Dat$BreastFed_3cat)
age_BreastFed_3cat_sd = sd.function.na(BFstructural_Dat, BFstructural_Dat$TIV, BFstructural_Dat$BreastFed_3cat)
age_BreastFed_3cat_range = range.function.na(BFstructural_Dat, BFstructural_Dat$TIV, BFstructural_Dat$BreastFed_3cat)

##sex
sex.BreastFed_3cat_tab = xtabs(~sex + BreastFed_3cat, data = BFstructural_Dat)
sex.BreastFed_3cat_fisher = fisher.test(sex.BreastFed_3cat_tab)

##maternal education
mED.BreastFed_3cat_tab = xtabs(~mEducation_cat + BreastFed_3cat, data = BFstructural_Dat)
mED.BreastFed_3cat_fisher = fisher.test(mED.BreastFed_3cat_tab)

##SES income
income.BreastFed_3cat_tab = xtabs(~income_3cat + BreastFed_3cat, data = BFstructural_Dat)
income.BreastFed_3cat_chi = chisq.test(income.BreastFed_3cat_tab)

#premature
premat.BreastFed_3cat_tab = xtabs(~BreastFed_3cat + cPreMat, data = BFstructural_Dat)
premat.BreastFed_3cat_chi = chisq.test(premat.BreastFed_3cat_tab)

##SR ####
#age
cebq_SR_age_cor.test = cor.test(BFstructural_Dat$cebq_SR,BFstructural_Dat$cAge_yr)

#TIV
cebq_SR_TIV_cor.test = cor.test(BFstructural_Dat$TIV, BFstructural_Dat$cebq_SR)

#sex
cebq_SR_sex_t.test = t.test(cebq_SR~sex, data = BFstructural_Dat)

#m education
cebq_SR_mEducation_anova = Anova(lm(cebq_SR~mEducation_cat, data = BFstructural_Dat))

# Income
cebq_SR_income_anova = Anova(lm(cebq_SR~income_3cat, data = BFstructural_Dat))

groupDif_SR_income = emmeans(lm(cebq_SR~income_3cat, data = BFstructural_Dat), specs = pairwise~income_3cat, adjust = "none")

cebq_SR_income_mean = means.function(BFstructural_Dat, BFstructural_Dat$cebq_SR, BFstructural_Dat$income_3cat)

cebq_SR_income_sd = sd.function(BFstructural_Dat, BFstructural_Dat$cebq_SR, BFstructural_Dat$income_3cat)

#premature
cebq_SR_premat_t.test = t.test(cebq_SR~cPreMat, data=BFstructural_Dat)

##Right Hippocampus ####
#age
rHip_age_cor.test = cor.test(BFstructural_Dat$cAge_yr, BFstructural_Dat$rHip_22)

#TIV
rHip_TIV_cor.test = cor.test(BFstructural_Dat$TIV, BFstructural_Dat$rHip_22)

#sex
rHip_sex_t.test = t.test(rHip_22~ sex, data=BFstructural_Dat)

#race
rHip_race_anova = Anova(lm(rHip_22~ cRace_cat, data = BFstructural_Dat))

#ethnicity
rHip_ethnicity_anova = Anova(lm(rHip_22~ cEthnicity_cat, data = BFstructural_Dat))

#m ed
rHip_mEducation_anova = Anova(lm(rHip_22~ mEducation_cat, data = BFstructural_Dat))

#income
rHip_income_anova = Anova(lm(rHip_22~income_3cat, data = BFstructural_Dat))

groupDif_rHip_income = emmeans(lm(rHip_22~income_3cat, data = BFstructural_Dat), specs = pairwise~income_3cat, adjust = "none")

rHip_income_mean = means.function(BFstructural_Dat, BFstructural_Dat$rHip_22, BFstructural_Dat$income_3cat)

rHip_income_sd = sd.function(BFstructural_Dat, BFstructural_Dat$rHip_22, BFstructural_Dat$income_3cat)

#premature
rHip_premat_t.test = t.test(rHip_22~ cPreMat, data=BFstructural_Dat)

##Left Hippocampus ####
#age
lHip_age_cor.test = cor.test(BFstructural_Dat$cAge_yr, BFstructural_Dat$lHip_21)

#TIV
lHip_TIV_cor.test = cor.test(BFstructural_Dat$TIV, BFstructural_Dat$lHip_21)

#sex
lHip_sex_t.test = t.test(lHip_21~ sex, data=BFstructural_Dat)

#race
lHip_race_anova = Anova(lm(lHip_21~cRace_cat, data = BFstructural_Dat))

# ethnicity
lHip_ethnicity_anova = Anova(lm(lHip_21~cEthnicity_cat, data = BFstructural_Dat))

#m education
lHip_mEducation_anova = Anova(lm(lHip_21~ mEducation_cat, data = BFstructural_Dat))

#income
lHip_income_anova = Anova(lm(lHip_21~income_3cat, data = BFstructural_Dat))

groupDif_lHip_income = emmeans(lm(lHip_21~ income_3cat, data = BFstructural_Dat), specs = pairwise~income_3cat, adjust = "none")

lHip_income_mean = means.function(BFstructural_Dat, BFstructural_Dat$lHip_21, BFstructural_Dat$income_3cat)

lHip_income_sd = sd.function(BFstructural_Dat, BFstructural_Dat$lHip_21, BFstructural_Dat$income_3cat)

#premature
lHip_premat_t.test = t.test(lHip_21~ cPreMat, data=BFstructural_Dat)

##%BMIp85 ####

#age
BMIp85BMI_age_cor.test = cor.test(BFstructural_Dat$cAge_yr, BFstructural_Dat$cdc_p85th)

#TIV
BMIp85BMI_TIV_cor.test = cor.test(BFstructural_Dat$TIV, BFstructural_Dat$cdc_p85th)

#sex
BMIp85BMI_sex_t.test = t.test(cdc_p85th~ sex, data=BFstructural_Dat)

#race
BMIp85BMI_race_anova = Anova(lm(cdc_p85th~ BFstructural_Dat$cRace_cat, data = BFstructural_Dat))

# ethnicity
BMIp85BMI_ethnicity_anova = Anova(lm(cdc_p85th~ BFstructural_Dat$cEthnicity_cat, data = BFstructural_Dat))

#m education
BMIp85BMI_mEducation_anova = Anova(lm(cdc_p85th~ mEducation_cat, data = BFstructural_Dat))

groupDif_BMIp85BMI_mEd = emmeans(lm(cdc_p85th~mEducation_cat, data = BFstructural_Dat), specs = pairwise~mEducation_cat, adjust = "none")

BMIp85BMI_mEducation_mean = means.function(BFstructural_Dat, BFstructural_Dat$cdc_p85th, BFstructural_Dat$mEducation_cat)

BMIp85BMI_mEducation_sd = sd.function(BFstructural_Dat, BFstructural_Dat$cdc_p85th, BFstructural_Dat$mEducation_cat)

#income
BMIp85BMI_income_anova = Anova(lm(cdc_p85th~income_3cat, data = BFstructural_Dat))

#premature
BMIp85BMI_premat_t.test = t.test(cdc_p85th~ cPreMat, data=BFstructural_Dat)

#####################################
####                            
####    Path Models Mediation
####          HIP
####        /   \
####      BF -> SR -> cdc_p85th  
####                            
#####################################

## make dummy variables ####
##breastfeeding
BFstructural_Dat$BreastFed_3cat_dummy = ifelse(BFstructural_Dat$BreastFed_3cat == '0-3mo', 0, ifelse(BFstructural_Dat$BreastFed_3cat == '4-6mo', 1, ifelse(BFstructural_Dat$BreastFed_3cat == '>6mo', 2, 'error')))
BFstructural_Dat$BreastFed_3cat_dummy = as.numeric(BFstructural_Dat$BreastFed_3cat_dummy)

##sex
BFstructural_Dat$sex_dummy = ifelse(BFstructural_Dat$sex == 'Male', 0, 1)

##mEducation
BFstructural_Dat$mEducation_dummy = ifelse(BFstructural_Dat$mEducation_cat == 'HighSchool/SomeCollege' , 0, ifelse(BFstructural_Dat$mEducation_cat == 'AA/Technical', 1, ifelse(BFstructural_Dat$mEducation_cat == 'BachelorDegree', 2, 3)))

##income
BFstructural_Dat$income_dummy = ifelse(BFstructural_Dat$income_3cat == '< $50,000', 0, ifelse(BFstructural_Dat$income_3cat == '$51,000 - $100,000', 1, 2))

##premat
BFstructural_Dat$cPreMat_dummy = ifelse(BFstructural_Dat$cPreMat == 'No', 0, 1)

##study
BFstructural_Dat$Study = gsub("_.*", "", BFstructural_Dat$StudyID)
BFstructural_Dat$Study_dummy = ifelse(BFstructural_Dat$Study == 'FBS', 0, ifelse(BFstructural_Dat$Study == 'DMK', 1, ifelse(BFstructural_Dat$Study == 'TestRetest', 2, ifelse(BFstructural_Dat$Study == 'cceb', 3, 4))))

## Left Hipp ####
HippL_pathmod <-'
# c path
cebq_SR ~ mEducation_dummy + income_dummy + cPreMat_dummy + c*BreastFed_3cat_dummy

# b1/a2 path
cebq_SR ~ TIV + IQR + Study_dummy + cAge_yr + sex_dummy + b1a2*lHip_21

# a1 path
lHip_21 ~ TIV + IQR + Study_dummy + cAge_yr + sex_dummy + cPreMat_dummy + a1*BreastFed_3cat_dummy

# c2 model
cdc_p85th ~ TIV + IQR + Study_dummy + cAge_yr + sex_dummy + c2*lHip_21

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
HippL_pathmod_fit <- sem(HippL_pathmod, data = BFstructural_Dat, estimator = "ML")
HippL_pathmod_summary <- summary(HippL_pathmod_fit, fit.measures = T, rsquare=TRUE)

# post-hoc for breastfeeding
##Breastfeeding groups t-tests
LHipp_ttest_0t3mo_4t6mo = t.test(lHip_21~BreastFed_3cat, data = BFstructural_Dat[BFstructural_Dat$BreastFed_3cat != '>6mo', ])
LHipp_ttest_4t6mo_g6mo = t.test(lHip_21~BreastFed_3cat, data = BFstructural_Dat[BFstructural_Dat$BreastFed_3cat != '0-3mo', ])
LHipp_ttest_0t3mo_g6mo = t.test(lHip_21~BreastFed_3cat, data = BFstructural_Dat[BFstructural_Dat$BreastFed_3cat != '4-6mo', ])

##standard deviation to report with measns
LHipp_BreastFeedingCat_sd = means.function(BFstructural_Dat, BFstructural_Dat$lHip_21, BFstructural_Dat$BreastFed_3cat)

## Right ####
HippR_pathmod <-'
# c path
cebq_SR ~ mEducation_dummy + income_dummy + cPreMat_dummy + c*BreastFed_3cat_dummy

# b1/a2 path
cebq_SR ~ TIV + IQR + Study_dummy + cAge_yr + sex_dummy + b1a2*rHip_22

# a1 path
rHip_22 ~ TIV + IQR + Study_dummy + cAge_yr + sex_dummy + cPreMat_dummy + a1*BreastFed_3cat_dummy

# c2 model
cdc_p85th ~ TIV + IQR + Study_dummy + cAge_yr + sex_dummy + c2*rHip_22

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
HippR_pathmod_fit <- sem(HippR_pathmod, data = BFstructural_Dat, estimator = "ML")
HippR_pathmod_summary <- summary(HippR_pathmod_fit, fit.measures = T, rsquare=TRUE)

# post-hoc for breastfeeding
##Breastfeeding groups t-tests
RHipp_ttest_0t3mo_4t6mo = t.test(rHip_22~BreastFed_3cat, data = BFstructural_Dat[BFstructural_Dat$BreastFed_3cat != '>6mo', ])
RHipp_ttest_4t6mo_g6mo = t.test(rHip_22~BreastFed_3cat, data = BFstructural_Dat[BFstructural_Dat$BreastFed_3cat != '0-3mo', ])
RHipp_ttest_0t3mo_g6mo = t.test(rHip_22~BreastFed_3cat, data = BFstructural_Dat[BFstructural_Dat$BreastFed_3cat != '4-6mo', ])

##standard deviation to report with measns
RHipp_BreastFeedingCat_sd = means.function(BFstructural_Dat, BFstructural_Dat$rHip_22, BFstructural_Dat$BreastFed_3cat)

#####################################
####                            
####    Path Models Mediation 
####          HIP
####        /   \
####      BF -> SR -> cdc_p85th  
####                            
####    senisitivity: add income and mEd to BF - HIP path
#####################################

## Left Hipp ####
HippL_SensitivityTest <-'
# c path
cebq_SR ~ mEducation_dummy + income_dummy + cPreMat_dummy + c*BreastFed_3cat_dummy

# b1/a2 path
cebq_SR ~ TIV + IQR + Study_dummy + cAge_yr + sex_dummy + b1a2*lHip_21

# a1 path
lHip_21 ~  mEducation_dummy + income_dummy + TIV + IQR + Study_dummy + cAge_yr + sex_dummy + cPreMat_dummy + a1*BreastFed_3cat_dummy

# c2 model
cdc_p85th ~ TIV + IQR + Study_dummy + cAge_yr + sex_dummy + c2*lHip_21

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
HippL_SensitivityTest_fit <- sem(HippL_SensitivityTest, data = BFstructural_Dat, estimator = "ML")
HippL_SensitivityTest_summary <- summary(HippL_SensitivityTest_fit, fit.measures = T, rsquare=TRUE)

## Right ####
HippR_SensitivityTest <-'
# c path
cebq_SR ~ mEducation_dummy + income_dummy + cPreMat_dummy + c*BreastFed_3cat_dummy

# b1/a2 path
cebq_SR ~ TIV + IQR + Study_dummy + cAge_yr + sex_dummy + b1a2*rHip_22

# a1 path
rHip_22 ~  mEducation_dummy + income_dummy + TIV + IQR + Study_dummy + cAge_yr + sex_dummy + cPreMat_dummy + a1*BreastFed_3cat_dummy

# c2 model
cdc_p85th ~ TIV + IQR + Study_dummy + cAge_yr + sex_dummy + c2*rHip_22

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
HippR_SensitivityTest_fit <- sem(HippR_SensitivityTest, data = BFstructural_Dat, estimator = "ML")
HippR_SensitivityTest_summary <- summary(HippR_SensitivityTest_fit, fit.measures = T, rsquare=TRUE)