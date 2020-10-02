############ Basic Data Load/Setup########
## you may need to install these libraries. 
## to do so type: install.packages('packagename')
library(reporttools)
library(xtable)
library(car)
library(stats)
library(lavaan)
library(lavaanPlot)
library(psych)
library(lsr)
library(childsds)
library(reshape2)


#set working directory to location of script--not needed when called 
#through Rmarkdown doc. Uncomment below if running locally/manually
#this.dir = getActiveDocumentContext()$path
#setwd(dirname(this.dir))

source('functions.R')
##load datasets
#run if have not compiled the following datasets:
#1)1_CompileData.R
#summary data for task

#load data
BFstructural_Alldata = read.csv('Data/BFstructural_Alldata.csv')
BFstructural_Alldata$cAge_yr = BFstructural_Alldata$cAge_mo/12

#use QC data 
BFstructural_covars = read.csv('Data/BFstructural_covars.csv')

#ratings
BFstructural_ICCratings = read.csv('Data/MRIstruct_FinalRatings_ICC.csv')

##remember to use: names(BFstructural_Alldata) to double check the variable names

#structural data
BFstructural_ROIdat_all = read.csv('Data/TPMall_BFstructural_ROI_dat_neuromorphometrics.csv')
BFstructural_ROIdat_mtl = BFstructural_ROIdat_all[BFstructural_ROIdat_all$region == 'lEnt' |
                                                    BFstructural_ROIdat_all$region == 'rEnt' |
                                                    BFstructural_ROIdat_all$region == 'lHip' |
                                                    BFstructural_ROIdat_all$region == 'rHip' |
                                                    BFstructural_ROIdat_all$region == 'lParHipGy'|
                                                    BFstructural_ROIdat_all$region == 'rParHipGy', ]


BFstructural_ROIdat_cobra = read.csv('Data/TPMall_BFstructural_ROI_dat_cobra.csv')
BFstructural_ROIdat_cobra_red = BFstructural_ROIdat_cobra[BFstructural_ROIdat_cobra$region == 'lHCA1' |
                                                            BFstructural_ROIdat_cobra$region == 'rHCA1' |
                                                            BFstructural_ROIdat_cobra$region == 'lSub' |
                                                            BFstructural_ROIdat_cobra$region == 'rSub' |
                                                            BFstructural_ROIdat_cobra$region == 'lCA4' |
                                                            BFstructural_ROIdat_cobra$region == 'rCA4' |
                                                            BFstructural_ROIdat_cobra$region == 'lCA2_3' |
                                                            BFstructural_ROIdat_cobra$region == 'rCA2_3' |
                                                            BFstructural_ROIdat_cobra$region == 'lStratum,Left Stratum Radiatum/Lacunosum/Moleculare' |
                                                            BFstructural_ROIdat_cobra$region == 'rStratum,Right Stratum Radiatum/Lacunosum/Moleculare', ]
  
##get percent above overweight
BFstructural_Alldata$cdc_bmi85 = NA
BFstructural_Alldata$cdc_bmi95 = NA

for(p in 1:nrow(BFstructural_Alldata)){
  sex_cor = ifelse(BFstructural_Alldata$sex[[p]] == 'Female', 'female', 'male')
  cdc_bmi8595_tab = make_percentile_tab(cdc.ref, item = "bmi",perc = c(85, 95), 
                                        age = BFstructural_Alldata$cAge_yr[p], 
                                        sex = sex_cor, stack = FALSE)
  BFstructural_Alldata$cdc_bmi85[p] = cdc_bmi8595_tab[cdc_bmi8595_tab$sex == sex_cor, ]$perc_85_0
  BFstructural_Alldata$cdc_bmi95[p] = cdc_bmi8595_tab[cdc_bmi8595_tab$sex == sex_cor, ]$perc_95_0
}

BFstructural_Alldata$cdc_p85th = BFstructural_Alldata$cBodyMass_index/BFstructural_Alldata$cdc_bmi85
BFstructural_Alldata$cdc_p95th = BFstructural_Alldata$cBodyMass_index/BFstructural_Alldata$cdc_bmi95


#####################################
####                            
####    Demo Data - By Sex       ####
####                            
#####################################
##OBstatus
BFstructural_Alldata$cBodyMass_OBstatus = ifelse(BFstructural_Alldata$cBodyMass_class == 'HW', 'HW', 'OB')

OBstatus_tab = xtabs(~cBodyMass_OBstatus, data = BFstructural_Alldata)
OBstatus.sex_tab = xtabs(~cBodyMass_OBstatus + sex, data = BFstructural_Alldata)

#chi square test - all numbers are greater than 5
OBstatus.sex_chi = chisq.test(OBstatus.sex_tab)

##Weight Class
WeightClass_tab = xtabs(~cBodyMass_class, data = BFstructural_Alldata)
WeightClass.sex_tab = xtabs(~cBodyMass_class + sex, data = BFstructural_Alldata)
WeightClass.sex_chi = chisq.test(WeightClass.sex_tab)

##Age
age_mean = mean(BFstructural_Alldata$cAge_yr, na.rm = TRUE)
age_sd = sd(BFstructural_Alldata$cAge_yr, na.rm = TRUE)
age_range = range(BFstructural_Alldata$cAge_yr, na.rm = TRUE)

age_sex_t = t.test(cAge_yr~sex, data = BFstructural_Alldata)
age_sex_sd = sd.function.na(BFstructural_Alldata, BFstructural_Alldata$cAge_yr, BFstructural_Alldata$sex)
age_sex_range = range.function.na(BFstructural_Alldata, BFstructural_Alldata$cAge_yr, BFstructural_Alldata$sex)
age_sex_d = cohensD(BFstructural_Alldata[BFstructural_Alldata$sex == 'Female', ]$cAge_yr, BFstructural_Alldata[BFstructural_Alldata$sex == 'Male', ]$cAge_yr)

##enter mean and sd in table

##BMI
BMI_mean = mean(BFstructural_Alldata$cBodyMass_index, na.rm = TRUE)
BMI_sd = sd(BFstructural_Alldata$cBodyMass_index, na.rm = TRUE)
BMI_range = range(BFstructural_Alldata$cBodyMass_index, na.rm = TRUE)

BMI_sex_t = t.test(cBodyMass_index~sex, data = BFstructural_Alldata)
BMI_sex_sd = sd.function.na(BFstructural_Alldata, BFstructural_Alldata$cBodyMass_index, BFstructural_Alldata$sex)
BMI_sex_range = range.function.na(BFstructural_Alldata, BFstructural_Alldata$cBodyMass_index, BFstructural_Alldata$sex)
BMI_sex_d = cohensD(BFstructural_Alldata[BFstructural_Alldata$sex == 'Female', ]$cBodyMass_index, BFstructural_Alldata[BFstructural_Alldata$sex == 'Male', ]$cBodyMass_index)

##cdc_p85th
cdc_p85th_mean = mean(BFstructural_Alldata$cdc_p85th, na.rm = TRUE)
cdc_p85th_sd = sd(BFstructural_Alldata$cdc_p85th, na.rm = TRUE)
cdc_p85th_range = range(BFstructural_Alldata$cdc_p85th, na.rm = TRUE)

cdc_p85th_sex_t = t.test(cdc_p85th~sex, data = BFstructural_Alldata)
cdc_p85th_sex_sd = sd.function.na(BFstructural_Alldata, BFstructural_Alldata$cdc_p85th, BFstructural_Alldata$sex)
cdc_p85th_sex_range = range.function.na(BFstructural_Alldata, BFstructural_Alldata$cdc_p85th, BFstructural_Alldata$sex)
cdc_p85th_sex_d = cohensD(BFstructural_Alldata[BFstructural_Alldata$sex == 'Female', ]$cdc_p85th, BFstructural_Alldata[BFstructural_Alldata$sex == 'Male', ]$cdc_p85th)

##BMI percentile
BMIp_mean = mean(BFstructural_Alldata$cBodyMass_p, na.rm = TRUE)
BMIp_sd = sd(BFstructural_Alldata$cBodyMass_p, na.rm = TRUE)
BMIp_range = range(BFstructural_Alldata$cBodyMass_p, na.rm = TRUE)

BMIp_sex_t = t.test(cBodyMass_p~sex, data = BFstructural_Alldata)
BMIp_sex_sd = sd.function.na(BFstructural_Alldata, BFstructural_Alldata$cBodyMass_p, BFstructural_Alldata$sex)
BMIp_sex_range = range.function.na(BFstructural_Alldata, BFstructural_Alldata$cBodyMass_p, BFstructural_Alldata$sex)
BMI_sex_d = cohensD(BFstructural_Alldata[BFstructural_Alldata$sex == 'Female', ]$cBodyMass_p, BFstructural_Alldata[BFstructural_Alldata$sex == 'Male', ]$cBodyMass_p)

##mEducation
mED_tab = xtabs(~mEducation, data = BFstructural_Alldata)
mED.sex_tab = xtabs(~mEducation + sex, data = BFstructural_Alldata)

##you will need to count the number that fall into each category: >BA, BA, Associates/technical, High school, other/NA
##you will get all from the mED.sex_tab above
##enter the numbers by collumn - first all for boys, then girls. You can just put the numbers for the whole sample into the table
##in BreastFeeding_Hipp.Rmd document

#fisher.test - use because one of the entered number is 5 or less
mED.sex_fisher = fisher.test(matrix(c(22, 30, 7, 8, 0, 28, 24, 11, 7, 0), nrow = 5, ncol = 2, byrow = FALSE))

##pEducation
pED_tab = xtabs(~dEducation, data = BFstructural_Alldata)
pED.sex_tab = xtabs(~dEducation + sex, data = BFstructural_Alldata)
pED.sex_fisher = fisher.test(matrix(c(28, 22, 5, 9, 1, 1, 29, 16, 10, 14, 0, 0), nrow = 6, ncol = 2, byrow = FALSE))

##copy same format as maternal eduction for paternal education

##Sex
sex_tab = xtabs(~sex, data = BFstructural_Alldata)

##Race
race_tab = xtabs(~cRace, data = BFstructural_Alldata)
race.sex_tab = xtabs(~cRace + sex, data = BFstructural_Alldata)
race.sex_fisher = fisher.test(matrix(c(5, 64, 4, 0, 2, 72, 2, 0), nrow = 4, ncol = 2, byrow = FALSE))

##copy same format as maternal eduction for race

##Ethnicity
ethnicity_tab = xtabs(~cEthnicity, data = BFstructural_Alldata)
ethnicity.sex_tab = xtabs(~cEthnicity + sex, data = BFstructural_Alldata)
ethnicity.sex_fisher = fisher.test(matrix(c(3, 59, 1, 3, 61, 0), nrow = 3, ncol = 2, byrow = FALSE))

##copy same format as maternal eduction for ethnicity


##SES
income_tab = xtabs(~income, data = BFstructural_Alldata)
income.sex_tab = xtabs(~income + sex, data = BFstructural_Alldata)
income.sex_fisher = fisher.test(matrix(c(26, 30, 16, 0, 23, 39, 12, 0), nrow = 4, ncol = 2, byrow = FALSE))


#### ####
#### ####
#####################################
####                            
####     Breast Feeding categories  ####
####                            
#####################################

##Breast Feeding
BFstructural_Alldata$BreastFed_g1year = ifelse(!is.na(as.numeric(as.character(BFstructural_Alldata$BreastFed_wk))), 
                                              ifelse(as.numeric(as.character(BFstructural_Alldata$BreastFed_wk)) > 48, 'Y', 'N'), 'N')

BFstructural_Alldata$BreastFed_monthsNumeric = ifelse(!is.na(as.numeric(as.character(BFstructural_Alldata$BreastFed_wk))), 'Y', 'N')
BFstructural_Alldata$BreastFed_months = ifelse(BFstructural_Alldata$BreastFed_wk == 'Never', 0,
                                                             ifelse(!is.na(as.numeric(as.character(BFstructural_Alldata$BreastFed_wk))), as.numeric(as.character(BFstructural_Alldata$BreastFed_wk))/4, NA))

BFstructural_Alldata$BreastFed_6mo = ifelse(BFstructural_Alldata$BreastFed_wk == '>24 wks', 1,
                                            ifelse(BFstructural_Alldata$BreastFed_wk == '16-24 wks', 0,
                                                   ifelse(BFstructural_Alldata$BreastFed_wk == '4-12 wks', 0,
                                                          ifelse(BFstructural_Alldata$BreastFed_wk == 'Never', 0,
                                                                 ifelse(as.numeric(as.character(BFstructural_Alldata$BreastFed_wk)) > 24, 1, 0)))))

BreastFed_6mo_tab = xtabs(~BreastFed_6mo, data = BFstructural_Alldata)
BreastFed_6mo.sex_tab = xtabs(~BreastFed_6mo + sex, data = BFstructural_Alldata)
BreastFed_6mo.sex_chi = chisq.test(BreastFed_6mo.sex_tab)

BFstructural_Alldata$BreastFed_3cat = ifelse(BFstructural_Alldata$BreastFed_wk == '>24 wks', '>6mo',
                                             ifelse(BFstructural_Alldata$BreastFed_wk == '16-24 wks', '4-6mo',
                                                    ifelse(BFstructural_Alldata$BreastFed_wk == '4-12 wks', '0-3mo',
                                                           ifelse(BFstructural_Alldata$BreastFed_wk == 'Never', '0-3mo',
                                                                  ifelse(as.numeric(as.character(BFstructural_Alldata$BreastFed_wk)) > 24, '>6mo', 
                                                                         ifelse(as.numeric(as.character(BFstructural_Alldata$BreastFed_wk)) >= 16, '4-6mo', '0-3mo'))))))

BreastFed_3cat_tab = xtabs(~BreastFed_3cat, data = BFstructural_Alldata)
BreastFed_3cat.sex_tab = xtabs(~BreastFed_3cat + sex, data = BFstructural_Alldata)
BreastFed_3cat.sex_chi = chisq.test(BreastFed_3cat.sex_tab)


##Age 
age_BreastFed_3cat_anova = Anova(lm(cAge_yr~BreastFed_3cat, data = BFstructural_Alldata))
age_BreastFed_3cat_mean = means.function(BFstructural_Alldata, BFstructural_Alldata$cAge_yr, BFstructural_Alldata$BreastFed_3cat)
age_BreastFed_3cat_sd = sd.function.na(BFstructural_Alldata, BFstructural_Alldata$cAge_yr, BFstructural_Alldata$BreastFed_3cat)
age_BreastFed_3cat_range = range.function.na(BFstructural_Alldata, BFstructural_Alldata$cAge_yr, BFstructural_Alldata$BreastFed_3cat)

##BMI
BMI_BreastFed_3cat_anova = Anova(lm(cBodyMass_index~BreastFed_3cat, data = BFstructural_Alldata))
BMI_BreastFed_3cat_mean = means.function(BFstructural_Alldata, BFstructural_Alldata$cBodyMass_index, BFstructural_Alldata$BreastFed_3cat)
BMI_BreastFed_3cat_sd = sd.function.na(BFstructural_Alldata, BFstructural_Alldata$cBodyMass_index, BFstructural_Alldata$BreastFed_3cat)
BMI_BreastFed_3cat_range = range.function.na(BFstructural_Alldata, BFstructural_Alldata$cBodyMass_index, BFstructural_Alldata$BreastFed_3cat)

##cdc_p85th
cdc_p85th_BreastFed_3cat_anova = Anova(lm(cdc_p85th~BreastFed_3cat, data = BFstructural_Alldata))
cdc_p85th_BreastFed_3cat_mean = means.function(BFstructural_Alldata, BFstructural_Alldata$cdc_p85th, BFstructural_Alldata$BreastFed_3cat)
cdc_p85th_BreastFed_3cat_sd = sd.function.na(BFstructural_Alldata, BFstructural_Alldata$cdc_p85th, BFstructural_Alldata$BreastFed_3cat)
cdc_p85th_BreastFed_3cat_range = range.function.na(BFstructural_Alldata, BFstructural_Alldata$cdc_p85th, BFstructural_Alldata$BreastFed_3cat)


##BMI percentile
BMIp_BreastFed_3cat_anova = Anova(lm(cBodyMass_p~BreastFed_3cat, data = BFstructural_Alldata))
BMIp_BreastFed_3cat_mean = means.function(BFstructural_Alldata, BFstructural_Alldata$cBodyMass_p, BFstructural_Alldata$BreastFed_3cat)
BMIp_BreastFed_3cat_sd = sd.function.na(BFstructural_Alldata, BFstructural_Alldata$cBodyMass_p, BFstructural_Alldata$BreastFed_3cat)
BMIp_BreastFed_3cat_range = range.function.na(BFstructural_Alldata, BFstructural_Alldata$cBodyMass_p, BFstructural_Alldata$BreastFed_3cat)

##sex
sex.BreastFed_3cat_tab = xtabs(~sex + BreastFed_3cat, data = BFstructural_Alldata)
sex.BreastFed_3cat_fisher = fisher.test(matrix(c(5, 64, 4, 0, 2, 72, 2, 0), nrow = 4, ncol = 2, byrow = FALSE))

##OBstatus
OBstatus.BreastFed_3cat_tab = xtabs(~cBodyMass_OBstatus + BreastFed_3cat, data = BFstructural_Alldata)
OBstatus.BreastFed_3cat_chi = chisq.test(OBstatus.BreastFed_3cat_tab)

##Weight Class
WeightClass.BreastFed_3cat_tab = xtabs(~cBodyMass_class + BreastFed_3cat, data = BFstructural_Alldata)
WeightClass.BreastFed_3cat_fisher = fisher.test(matrix(c(10, 6, 24, 5, 5, 45, 5, 10, 39), nrow = 3, ncol = 3, byrow = FALSE))

##maternal education
BFstructural_Alldata$mEducation_cat = ifelse(BFstructural_Alldata$mEducation == "TechnicalDegree" | 
                                               BFstructural_Alldata$mEducation == "AssociatesDegree" |
                                               BFstructural_Alldata$mEducation == "SomeCollege", "AA/Technical", 
                                             ifelse(BFstructural_Alldata$mEducation == "SomeGraduateSchool" | 
                                                      BFstructural_Alldata$mEducation == "MastersDegree" |
                                                      BFstructural_Alldata$mEducation == "GradSchool" |
                                                      BFstructural_Alldata$mEducation == "DoctoralDegree", ">BA", as.character(BFstructural_Alldata$mEducation)))

mED.BreastFed_3cat_tab = xtabs(~mEducation + BreastFed_3cat, data = BFstructural_Alldata)
mED.BreastFed_3cat_fisher = fisher.test(matrix(c(13, 15, 6, 3, 0, 0, 17, 21, 5, 6, 0, 0, 20, 18, 7, 6, 0, 0), nrow = 6, ncol = 3, byrow = FALSE))

##paternal education
pED.BreastFed_3cat_tab = xtabs(~dEducation + BreastFed_3cat, data = BFstructural_Alldata)
pED.BreastFed_3cat_fisher = fisher.test(matrix(c(9, 11, 5, 9, 1, 0, 29, 10, 3, 6, 0, 0, 19, 17, 7, 8, 0, 1), nrow = 6, ncol = 3, byrow = FALSE))

##SES income
BFstructural_ROIdat_mtl_wide$income_3cat = ifelse(BFstructural_ROIdat_mtl_wide$income == '<$20K' | 
                                                     BFstructural_ROIdat_mtl_wide$income == '$21-35K' | 
                                                     BFstructural_ROIdat_mtl_wide$income == '$36-50K', '<=50K',
                                                   ifelse(BFstructural_ROIdat_mtl_wide$income == '$51-75K' | 
                                                            BFstructural_ROIdat_mtl_wide$income == '$76-100K', '51-100K', '>100K'))

income.BreastFed_3cat_tab = xtabs(~income + BreastFed_3cat, data = BFstructural_Alldata)
income.BreastFed_3cat_fisher = fisher.test(matrix(c(10, 16, 12, 0, 22, 22, 10, 0, 17, 31, 6, 0), nrow = 4, ncol = 3, byrow = FALSE))

##Ethnicity
ethnicity.BreastFed_3cat_tab = xtabs(~cEthnicity + BreastFed_3cat, data = BFstructural_Alldata)
ethnicity.BreastFed_3cat_fisher = fisher.test(matrix(c(2, 30, 1, 2, 50, 0, 2, 40, 0), nrow = 3, ncol = 3, byrow = FALSE))

##Race
race.BreastFed_3cat_tab = xtabs(~cRace + BreastFed_3cat, data = BFstructural_Alldata)
race.BreastFed_3cat_fisher = fisher.test(matrix(c(2, 35, 3, 0, 1, 53, 1, 0, 4, 48, 2, 0), nrow = 4, ncol = 3, byrow = FALSE))

#####################################
####                            
####    CEBQ alpha  ####
####                            
#####################################
BFstructural_Alldata$cebq3_numRev = ifelse(BFstructural_Alldata$cebq3_num == 5, 1,
                                           ifelse(BFstructural_Alldata$cebq3_num == 4, 2, 3))

psych::alpha(BFstructural_Alldata[c('cebq3_numRev', 'cebq17_num', 'cebq21_num', 'cebq26_num', 'cebq30_num')])

#####################################
####                            
####    Set up Structural Data  ####
####                            
#####################################
BFstructural_ROIdat_mtl_wide = dcast(BFstructural_ROIdat_mtl, parID~region + ids, value.var = 'vgm')
BFstructural_ROIdat_cobra_red_wideGM = dcast(BFstructural_ROIdat_cobra_red, parID~region + ids, value.var = 'vgm')
BFstructural_ROIdat_cobra_red_wideWM = dcast(BFstructural_ROIdat_cobra_red, parID~region + ids, value.var = 'vwm')

names(BFstructural_ROIdat_cobra_red_wideGM)[c(5, 10)] = c('lStratum/StratumRadiatum/Lacunosum/Moleculare_36',
                                                          'rStratum/StratumRadiatum/Lacunosum/Moleculare_136')
names(BFstructural_ROIdat_cobra_red_wideGM)[2:11] = paste0(names(BFstructural_ROIdat_cobra_red_wideGM)[2:11], '_GM')
names(BFstructural_ROIdat_cobra_red_wideWM)[c(5, 10)] = c('lStratum/StratumRadiatum/Lacunosum/Moleculare_36',
                                                          'rStratum/StratumRadiatum/Lacunosum/Moleculare_136')
names(BFstructural_ROIdat_cobra_red_wideWM)[2:11] = paste0(names(BFstructural_ROIdat_cobra_red_wideWM)[2:11], '_WM')

BFstructural_ROIdat_mtl_wide = merge(BFstructural_ROIdat_mtl_wide, BFstructural_ROIdat_cobra_red_wideGM, by = 'parID')
BFstructural_ROIdat_mtl_wide = merge(BFstructural_ROIdat_mtl_wide, BFstructural_ROIdat_cobra_red_wideWM, by = 'parID')

#add ids to match other dsets
BFstructural_ROIdat_mtl_wide = merge(BFstructural_ROIdat_mtl_wide, BFstructural_covars[c(1:4, 5, 18)], by = 'parID')
names(BFstructural_ROIdat_mtl_wide)[c(28, 31)] = c('StudyID', 'LabID')

BFstructural_ROIdat_mtl_wide = merge(BFstructural_ROIdat_mtl_wide, BFstructural_Alldata, by = 'StudyID')
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


#add Study var as control
BFstructural_ROIdat_mtl_wide$Study = gsub("_.*", "", BFstructural_ROIdat_mtl_wide$StudyID)

#####################################
####                            
####    Set up dummy coded vars  ####
####                            
#####################################

BFstructural_ROIdat_mtl_wide$BreastFed_3cat_dummy = ifelse(BFstructural_ROIdat_mtl_wide$BreastFed_3cat == '0-3mo', 0,
                                                           ifelse(BFstructural_ROIdat_mtl_wide$BreastFed_3cat == '4-6mo', 1,
                                                                  ifelse(BFstructural_ROIdat_mtl_wide$BreastFed_3cat == '>6mo', 2, 'error')))
BFstructural_ROIdat_mtl_wide$BreastFed_3cat_dummy = as.numeric(BFstructural_ROIdat_mtl_wide$BreastFed_3cat_dummy)

BFstructural_ROIdat_mtl_wide$mEducation_dummy = ifelse(BFstructural_ROIdat_mtl_wide$mEducation == 'HighSchool' |
                                                         BFstructural_ROIdat_mtl_wide$mEducation == 'SomeCollege', 0,
                                                       ifelse(BFstructural_ROIdat_mtl_wide$mEducation == 'TechnicalDegree' | 
                                                                BFstructural_ROIdat_mtl_wide$mEducation == 'AssociatesDegree', 1,
                                                              ifelse(BFstructural_ROIdat_mtl_wide$mEducation == 'BachelorDegree', 2, 3)))

BFstructural_ROIdat_mtl_wide$income_dummy = ifelse(BFstructural_ROIdat_mtl_wide$income == '<$20K' | 
                                                     BFstructural_ROIdat_mtl_wide$income == '$21-35K' | 
                                                     BFstructural_ROIdat_mtl_wide$income == '$36-50K', 0,
                                                   ifelse(BFstructural_ROIdat_mtl_wide$income == '$51-75K' | 
                                                            BFstructural_ROIdat_mtl_wide$income == '$76-100K', 1, 2))

BFstructural_ROIdat_mtl_wide$cPreMat_dummy = ifelse(BFstructural_ROIdat_mtl_wide$cPreMat == 'No', 0, 1)

BFstructural_ROIdat_mtl_wide$Study_dummy = ifelse(BFstructural_ROIdat_mtl_wide$Study == 'FBS', 0,
                                                  ifelse(BFstructural_ROIdat_mtl_wide$Study == 'DMK', 1,
                                                         ifelse(BFstructural_ROIdat_mtl_wide$Study == 'TestRetest', 2,
                                                                ifelse(BFstructural_ROIdat_mtl_wide$Study == 'cceb', 3, 4))))

write.csv(BFstructural_ROIdat_mtl_wide, 'OSF_files/Data/BFstructural_Data.csv', row.names = FALSE)

#1 removed for QC rating >=4
BFstructural_ROIdat_mtl_wide = BFstructural_ROIdat_mtl_wide[BFstructural_ROIdat_mtl_wide$AverageRating < 4, ]




#####################################
####                            
####     All - Correlation with Volumes  ####
####                            
#####################################
BFstructural_MTLrois_corvars = BFstructural_ROIdat_BFcont_mtl_wide[c(139, 136, 92, 32, 29:30, 3:8)]
BFstructural_MTLrois_cornames = names(BFstructural_ROIdat_BFcont_mtl_wide)[c(139, 136, 92, 32, 29:30, 3:8)]
BFstructural_MTLrois_cormat = cor.matrix(BFstructural_MTLrois_corvars, BFstructural_MTLrois_cornames)

BFstructural_ROIdat_subregionsGM_corvars = BFstructural_ROIdat_BFcont_mtl_wide[c(139, 136, 92, 32, 29:30, 4, 7, 9:18)]
BFstructural_ROIdat_subregionsGM_cornames = names(BFstructural_ROIdat_BFcont_mtl_wide)[c(139, 136, 92, 32, 29:30,4, 7, 9:18)]
BFstructural_ROIdat_subregionsGM_cormat = cor.matrix(BFstructural_ROIdat_subregionsGM_corvars, BFstructural_ROIdat_subregionsGM_cornames)

BFstructural_ROIdat_subregionsWM_corvars = BFstructural_ROIdat_BFcont_mtl_wide[c(101, 104, 92, 32, 29:30, 4, 7, 19:28)]
BFstructural_ROIdat_subregionsWM_cornames = names(BFstructural_ROIdat_BFcont_mtl_wide)[c(139, 136, 92, 32, 29:30, 4, 7, 19:28)]
BFstructural_ROIdat_subregionsWM_cormat = cor.matrix(BFstructural_ROIdat_subregionsWM_corvars, BFstructural_ROIdat_subregionsWM_cornames)

##Breastfeeding ANOVA
BMIp_BreastFed_3cat_anova = Anova(lm(lEnt_59~BreastFed_3cat, data = BFstructural_ROIdat_BFcont_mtl_wide))
BMIp_BreastFed_3cat_mean = means.function(BFstructural_Alldata, BFstructural_Alldata$cBodyMass_p, BFstructural_Alldata$BreastFed_3cat)
BMIp_BreastFed_3cat_sd = sd.function.na(BFstructural_Alldata, BFstructural_Alldata$cBodyMass_p, BFstructural_Alldata$BreastFed_3cat)
BMIp_BreastFed_3cat_range = range.function.na(BFstructural_Alldata, BFstructural_Alldata$cBodyMass_p, BFstructural_Alldata$BreastFed_3cat)

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
HippL_BF2SR_p85th_cov_fit <- sem(HippL_BF2SR_p85th_cov_pathmod, data = BFstructural_ROIdat_mtl_wide, estimator = "ML")
HippL_BF2SR_p85th_cov_summary <- summary(HippL_BF2SR_p85th_cov_fit, fit.measures = T, rsquare=TRUE)
HippL_BF2SR_p85th_cov_std = standardizedSolution(HippL_BF2SR_p85th_cov_fit, type = 'std.all', output = 'pretty')

HippL_BF2SR_p85th_cov_plot = lavaanPlot(model = HippL_BF2SR_p85th_cov_fit, node_options = list(shape = "box", fontname = "Times"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE, stars = "regress")

# post-hoc for breastfeeding
##Breastfeeding groups t-tests
ttest_0t3mo_g6mo = t.test(lHip_21~BreastFed_3cat, data = BFstructural_ROIdat_mtl_wide[BFstructural_ROIdat_mtl_wide$BreastFed_3cat != '>6mo', ])
ttest_4t6mo_g6mo = t.test(lHip_21~BreastFed_3cat, data = BFstructural_ROIdat_mtl_wide[BFstructural_ROIdat_mtl_wide$BreastFed_3cat != '0-3mo', ])
ttest_0t3mo_g6mo = t.test(lHip_21~BreastFed_3cat, data = BFstructural_ROIdat_mtl_wide[BFstructural_ROIdat_mtl_wide$BreastFed_3cat != '4-6mo', ])

##standard deviation to report with measns
LHipp_BreastFeedingCat_sd = means.function(BFstructural_ROIdat_mtl_wide, BFstructural_ROIdat_mtl_wide$lHip_21, BFstructural_ROIdat_mtl_wide$BreastFed_3cat)

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
HippR_BF2SR_p85th_cov_fit <- sem(HippR_BF2SR_p85th_cov_pathmod, data = BFstructural_ROIdat_mtl_wide, estimator = "ML")
HippR_BF2SR_p85th_cov_summary <- summary(HippR_BF2SR_p85th_cov_fit, fit.measures = T, rsquare=TRUE)
HippR_BF2SR_p85th_cov_std = standardizedSolution(HippR_BF2SR_p85th_cov_fit, type = 'std.all', output = 'pretty')
HippR_BF2SR_p85th_plot = lavaanPlot(model = HippR_BF2SR_p85th_cov_fit, node_options = list(shape = "box", fontname = "Times"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE, stars = "regress")


# post-hoc for breastfeeding
##Breastfeeding groups t-tests
ttest_0t3mo_g6mo = t.test(lHip_21~BreastFed_3cat, data = BFstructural_ROIdat_mtl_wide[BFstructural_ROIdat_mtl_wide$BreastFed_3cat != '>6mo', ])
ttest_4t6mo_g6mo = t.test(lHip_21~BreastFed_3cat, data = BFstructural_ROIdat_mtl_wide[BFstructural_ROIdat_mtl_wide$BreastFed_3cat != '0-3mo', ])
ttest_0t3mo_g6mo = t.test(lHip_21~BreastFed_3cat, data = BFstructural_ROIdat_mtl_wide[BFstructural_ROIdat_mtl_wide$BreastFed_3cat != '4-6mo', ])

##standard deviation to report with measns
LHipp_BreastFeedingCat_sd = means.function(BFstructural_ROIdat_mtl_wide, BFstructural_ROIdat_mtl_wide$lHip_21, BFstructural_ROIdat_mtl_wide$BreastFed_3cat)



# #####################################
# ####                            
# ####     All Data Path Models Mediation    ####
# ####     SR -> Hip, BF Med } -> cdc_p85th  ####
# ####                            
# #####################################
# 
# ## Left Hippocampus
# HippL_SR2H_medBF_p85th_pathmod <-'
# # c path
# lHip_21 ~ TIV + IQR + Study_dummy + cAge_yr + sex + mEducation_dummy + income_dummy + cPreMat_dummy + c*cebq_SR
# 
# # b1/a2 path
# lHip_21 ~ b1a2*BreastFed_3cat_dummy
# 
# # a1 path
# BreastFed_3cat_dummy ~ mEducation_dummy + income_dummy + cPreMat_dummy + a1*cebq_SR
# 
# # b2 prime model
# cdc_p85th ~ TIV + IQR + Study_dummy + cAge_yr + sex +  b2*lHip_21
# 
# # c2 model
# cdc_p85th ~ mEducation_dummy + income_dummy + cPreMat_dummy + c2*BreastFed_3cat_dummy
# 
# #indirect effects
# BFMed_SR2lHip := a1*b1a2
# lHipMed_BF2p85th := b1a2*b2
# 
# #total effect
# BFMed_SR2lHip_total := c + BFMed_SR2lHip
# lHipMed_BF2p85th_total := c2 + lHipMed_BF2p85th
# 
# #proportion med
# BF_propmed := BFMed_SR2lHip/BFMed_SR2lHip_total
# lHip_propmed := lHipMed_BF2p85th/lHipMed_BF2p85th_total
# '
# 
# # fit the model
# HippL_SR2H_medBF_p85th_fit <- sem(HippL_SR2H_medBF_p85th_pathmod, data = BFstructural_ROIdat_mtl_wide, estimator = "ML")
# HippL_SR2H_medBF_p85th_summary <- summary(HippL_SR2H_medBF_p85th_fit, fit.measures = T, rsquare=TRUE)
# HippL_SR2H_medBF_p85th_std = standardizedSolution(HippL_SR2H_medBF_p85th_fit, type = 'std.all', output = 'pretty')
# HippL_SR2H_medBF_p85th_plot = lavaanPlot(model = HippL_SR2H_medBF_p85th_fit, node_options = list(shape = "box", fontname = "Times"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = FALSE, stars = "regress")
# 
# ## Right Hippocampus
# HippR_SR2H_medBF_p85th_pathmod <-'
# # c path
# rHip_22 ~ TIV + IQR + Study_dummy + cAge_yr + sex + mEducation_dummy + income_dummy + cPreMat_dummy + c*cebq_SR
# 
# # b1/a2 path
# rHip_22 ~ b1a2*BreastFed_3cat_dummy
# 
# # a1 path
# BreastFed_3cat_dummy ~ mEducation_dummy + income_dummy + cPreMat_dummy + a1*cebq_SR
# 
# # b2 prime model
# cdc_p85th ~ TIV + IQR + Study_dummy + cAge_yr + sex +  b2*rHip_22
# 
# # c2 model
# cdc_p85th ~ mEducation_dummy + income_dummy + cPreMat_dummy + c2*BreastFed_3cat_dummy
# 
# #indirect effects
# BFMed_SR2rHip := a1*b1a2
# rHipMed_BF2p85th := b1a2*b2
# 
# #total effect
# BFMed_SR2rHip_total := c + BFMed_SR2rHip
# rHipMed_BF2p85th_total := c2 + rHipMed_BF2p85th
# 
# #proportion med
# BF_propmed := BFMed_SR2rHip/BFMed_SR2rHip_total
# rHip_propmed := rHipMed_BF2p85th/rHipMed_BF2p85th_total
# '
# 
# # fit the model
# HippR_SR2H_medBF_p85th_fit <- sem(HippR_SR2H_medBF_p85th_pathmod, data = BFstructural_ROIdat_mtl_wide, estimator = "ML")
# HippR_SR2H_medBF_p85th_summary <- summary(HippR_SR2H_medBF_p85th_fit, fit.measures = T, rsquare=TRUE)
# HippR_SR2H_medBF_p85th_std = standardizedSolution(HippR_SR2H_medBF_p85th_fit, type = 'std.all', output = 'pretty')
# HippR_SR2H_medBF_p85th_plot = lavaanPlot(model = HippR_SR2H_medBF_p85th_fit, node_options = list(shape = "box", fontname = "Times"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE, stars = "regress")

