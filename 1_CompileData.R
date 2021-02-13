############ Basic Data Load/Setup########
library(memisc)
library(rstudioapi)
library(lubridate)
library(psych)

#set working directory to BFation of script--not needed when called 
#through Rmarkdown doc. Uncomment below if running BFally/manually
#this.dir = getActiveDocumentContext()$path
#setwd(dirname(this.dir))

source('functions.R')

##load datasets
all_dat = read.csv('Data/All_data.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "#NULL!", " "))
BF_dat = all_dat[!is.na(all_dat$BreastFed_wk), c(1:16, 22:28, 38:40, 129:138, 94:128)]
BF_dat$LabID = factor(BF_dat$LabID)
BF_dat$StudyID = factor(BF_dat$StudyID)
BF_dat$Keep = 'N'

#Load QC data
TPMall_QC = read.csv('Data/TPMall_QCdata.csv', header = TRUE)
TPMall_QC$ScanDate = format(as.POSIXct(TPMall_QC$ScanDate,format='%m/%d/%y'),format='%Y/%m/%d')
TPMall_QC$ScanDate = as.Date(lubridate::ymd(TPMall_QC$ScanDate))
TPMall_QC$BFdat = 'N'
TPMall_QC$Keep = 'N'
TPMall_QC$StudyID_datMatch = NA

#Load Ratings
Ratings_dat = read.csv('Data/MRIstruct_FinalRatings_ICC.csv', header = TRUE)
names(Ratings_dat)[1] = "parID"

QC_parIDs = unique(TPMall_QC$LabID)
BF_parIDs = unique(BF_dat$LabID)


#loop through to get the best rated for those with multiple visits
for(p in 1:length(QC_parIDs)){
  
  #get IDs that match
  QC_pardat = TPMall_QC[TPMall_QC$LabID == QC_parIDs[p], ]
  
  #check that ID has BF data
  for(id in 1:nrow(QC_pardat)){
    
    studyID_split = strsplit(as.character(QC_pardat$parID[id]), '_')
    
    if(studyID_split[[1]][1] == 'brand'){
      study_Fix = "Brand"
    } else if(studyID_split[[1]][1] == 'port'){
      study_Fix = "FBS"
    } else if(studyID_split[[1]][1] == 'trt'){
      study_Fix = "TestRetest"
      studyID_split[[1]][2] = substr(studyID_split[[1]][2], start = 1, stop = nchar(studyID_split[[1]][2])-1)
    } else if(studyID_split[[1]][1] == 'cceb'){
      study_Fix = "cceb"
    } else if(studyID_split[[1]][1] == 'dmk'){
      study_Fix = "DMK"
    }
    
    studyID_match = paste0(study_Fix, '_', as.character(studyID_split[[1]][2]))
    
    TPMall_QC[TPMall_QC$parID == QC_pardat$parID[id], 'StudyID_datMatch'] = studyID_match

    QC_pardat$StudyID_datMatch[id] = studyID_match
    
    if (any(studyID_match == BF_dat$StudyID)){
      TPMall_QC[TPMall_QC$parID == QC_pardat$parID[id], 'BFdat'] = 'Y'
      QC_pardat$BFdat[id] = 'Y'
    }
    
  }
  
  QC_pardat_BF = QC_pardat[QC_pardat$BFdat == 'Y', ]

  #if there is more than 1 in dset
  if(nrow(QC_pardat_BF) > 1){
    
    #create a dset to hold comparisons for IDS
    compare_dat = data.frame(QC_pardat_BF[c('parID', 'StudyID', 'ScanDate', 'StudyID_datMatch')])
    
    #compare IQR and mark the lowest with a '1' if they differ
    compare_dat$minIQR = 0
    if(length(unique(QC_pardat_BF$IQR))>1){
      minIQR = QC_pardat_BF[QC_pardat_BF$IQR == min(QC_pardat_BF$IQR), 'parID']
      compare_dat[compare_dat$parID %in% minIQR, 'minIQR'] = 1
    }
    
    #compare Ringing and mark the lowest with a '1' if they differ
    compare_dat$minRinging = 0
    if(length(unique(QC_pardat_BF$Ringing))>1){
      minRinging = QC_pardat_BF[QC_pardat_BF$Ringing == min(QC_pardat_BF$Ringing), 'parID']
      compare_dat[compare_dat$parID %in% minRinging, 'minRinging'] = 1
    }
    
    #compare Ghosting and mark the lowest with a '1' if they differ
    compare_dat$minGhosting = 0
    if(length(unique(QC_pardat_BF$Ghosting))>1){
      minGhosting = QC_pardat_BF[QC_pardat_BF$Ghosting == min(QC_pardat_BF$Ghosting), 'parID']
      compare_dat[compare_dat$parID %in% minGhosting, 'minGhosting'] = 1
    }
    
    #compare Bluriness and mark the lowest with a '1' if they differ
    compare_dat$minBlurriness = 0
    if(length(unique(QC_pardat_BF$Blurriness))>1){
      minBlurriness = QC_pardat_BF[QC_pardat_BF$Blurriness == min(QC_pardat_BF$Blurriness), 'parID']
      compare_dat[compare_dat$parID %in% minBlurriness, 'minBlurriness'] = 1
    }
    
    #get total score
    compare_dat$totalScore = rowSums(compare_dat[c('minIQR', 'minRinging', 'minGhosting', 'minBlurriness')])
    
    nMax = compare_dat[compare_dat$totalScore == max(compare_dat$totalScore), 'StudyID_datMatch']
    
    #if there is no difference in total score, look at rating consistency
    if(length(nMax)>1){
      
      #add raw ratings to compare dset
      compare_dat = merge(compare_dat, Ratings_dat[c(1, 4:6, 10:12)], by = "parID")
      
      #compare Ringing and mark the lowest with a '1' if they differ
      compare_dat$Ringing_dif = compare_dat$Ringing_Ryan - compare_dat$Ringing_Jane
      compare_dat$Ghosting_dif = compare_dat$Ghosting_Ryan - compare_dat$Ghosting_Jane
      compare_dat$Blurriness_dif = compare_dat$Blurriness_Ryan - compare_dat$Blurriness_Jane
      
      #compare Ringing and mark the lowest with a '1' if they differ
      compare_dat$minRinging_dif = 0
      if(length(unique(abs(compare_dat$Ringing_dif)))>1){
        minRinging_dif = compare_dat[abs(compare_dat$Ringing_dif) == abs(min(compare_dat$Ringing_dif)), 'parID']
        compare_dat[compare_dat$parID %in% minRinging_dif, 'minRinging_dif'] = 1
      }
      
      #compare Ghosting and mark the lowest with a '1' if they differ
      compare_dat$minGhosting_dif = 0
      if(length(unique(abs(compare_dat$Ghosting_dif)))>1){
        minGhosting_dif = compare_dat[abs(compare_dat$Ghosting_dif) == abs(min(compare_dat$Ghosting_dif)), 'parID']
        compare_dat[compare_dat$parID %in% minGhosting_dif, 'minGhosting_dif'] = 1
      }
      
      #compare Bluriness and mark the lowest with a '1' if they differ
      compare_dat$minBlurriness_dif = 0
      if(length(unique(abs(compare_dat$Blurriness_dif)))>1){
        minBlurriness_dif = compare_dat[abs(compare_dat$Blurriness_dif) == min(abs(compare_dat$Blurriness_dif)), 'parID']
        compare_dat[compare_dat$parID %in% minBlurriness_dif, 'minBlurriness_dif'] = 1
      }
      
      compare_dat$totalScore = rowSums(compare_dat[c('minIQR', 'minRinging', 'minGhosting', 'minBlurriness',
                                                     'minRinging_dif', 'minGhosting_dif', 'minBlurriness')])
    }
    
    nMax = compare_dat[compare_dat$totalScore == max(compare_dat$totalScore), 'StudyID_datMatch']
    
    #if the two scan are still not unique, choose the 
    if(length(nMax)>1){
      
      #compare dates and mark the earlier with a '1'
      compare_dat$date = 0
      compare_dat[compare_dat$ScanDate == min(compare_dat$ScanDate, 'date'), 'date'] = 1
      
      compare_dat$totalScore = rowSums(compare_dat[c('minIQR', 'minRinging', 'minGhosting', 'minBlurriness',
                                                     'minRinging_dif', 'minGhosting_dif', 'minBlurriness', 
                                                     'date')])
      
    }
    
    QC_studyIDmatch = compare_dat[compare_dat$totalScore == max(compare_dat$totalScore), 'StudyID_datMatch']
    QC_studyID = compare_dat[compare_dat$totalScore == max(compare_dat$totalScore), 'parID']
    
    BF_dat[BF_dat$StudyID == QC_studyIDmatch, 'Keep'] = 'Y'
    TPMall_QC[TPMall_QC$parID == QC_studyID, 'Keep'] = 'Y'
    
  } else if(nrow(QC_pardat_BF) == 1){
    
    QC_studyID = TPMall_QC[TPMall_QC$parID == QC_pardat_BF$parID, 'StudyID_datMatch']
    
    
    BF_dat[BF_dat$StudyID == QC_studyID, 'Keep'] = 'Y'
    TPMall_QC[!is.na(TPMall_QC$StudyID_datMatch) & TPMall_QC$StudyID_datMatch == QC_studyID, 'Keep'] = 'Y'
  } 
}

# Get Average Rating 
TPMall_QC$AverageRating = rowMeans(TPMall_QC[c('Ringing', 'Ghosting', 'Blurriness')])

#get ratings for subset
Ratings_dat = merge(Ratings_dat, TPMall_QC[c(1, 15:17)], by = "parID")

#make covariate database
BFstructural_covars = merge(TPMall_QC[TPMall_QC$Keep == 'Y', ], BF_dat[c(1:2, 9:11, 69)], by.x = 'StudyID_datMatch', by.y = 'StudyID')

#reduce to 'Keep' and write out dsets
write.csv(TPMall_QC[TPMall_QC$Keep == 'Y', ], 'Data/BFstructural_TPMall_QCdata.csv', row.names = FALSE)
write.csv(TPMall_QC[TPMall_QC$Keep == 'Y', c('LabID', 'parID')], 'Data/BFstructural_parlist.csv', row.names = F)
write.csv(BFstructural_covars, 'Data/BFstructural_covars.csv', row.names = F)
write.csv(BF_dat[BF_dat$Keep == 'Y', ], 'Data/BFstructural_Alldata.csv', row.names = FALSE)
write.csv(Ratings_dat[Ratings_dat$Keep == 'Y', ], 'Data/BFstructural_MRIstruct_FinalRatings_ICC.csv', row.names = FALSE)

