# BF_HippBMI
The projected used structural MRI data compiled across 5 studies to examine the impact of exclusive breastfeeding duration on satiety responsiveness, hippocampal grey matter density, and BMI in 7-11 year old children.

## Directory Structure and Files

### BFstructural_docker.Rproj
This is an R project file.

### renv/renv.lock
These are associated with the renv package in R that allows for consistency in the package versions associated with a project. When opening the project, these will ensure that the same versions of packages are installed/loaded to reproduce the analyses. However, this will not resolve any operating system dependent influences. 

### BFstructural_docker_results.pdf 
This is a .pdf generated from the BFstructural_docker_results.Rmd via the BFstructural_dockerImage.tar.gz docker image.

### BFstructural_docker_setup.R
This is an R script that contains all the analysis and setup code

### Data
1) BFstructural_Alldata.csv: contains all behavioral and demographic data
2) BFstructural_covars.csv: contains common structural covariates
3) MRIstruct_FinalRatings_ICC.csv: contains QC ratings by independent raters
4) TPMall_BFstructural_ROI_dat_neuromorphometrics.csv: extracted grey matter volumes for the neuromorphometrics atlas using CAT. Only hippocamapal volumes were used do to apriori theoretical model

### BFstructural_docker_results.Rmd 
This is a R markdown document that will generate a .pdf of the results. 
Note: if running this file outside the docker image, exact replication of results is not garenteed. If project is first loaded, then package versions will match due to the use of the package renv (will load project specific version of R packages listed in renv.lock). However, this will not resolve opperating system dependent discrepencies.

### Docker Image

bf_structural-AJCNsubmission.tar is the docker image for this project and should replicate reported results. To use image, download to your system and use the terminal to execute the following steps:

1) Load docker image: 
        docker load bf_structural-AJCNsubmission.tar

2) Run docker image: 
        docker run -v "$(pwd):/project/output" bf_structural-AJCNsubmission
        Note: if on windows machine, you will need to use different syntax for $(pwd) part of the command
