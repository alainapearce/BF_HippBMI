# BF_HippBMI
The projected used structural MRI data compiled across 5 studies to examine the impact of exclusive breastfeeding duration on satiety responsiveness, hippocampal grey matter density, and BMI in 7-11 year old children.

## OSF Directory Structure and Files

### 1_CompileData.R: this is an R script to select the individual participants and choose the visit with the best quality MRI scan if the participant was part of more than 1 study. The subset data used in this project is fully available in both the Data directory and the docker image. We are currently working to make the larger database fully available with a persistant DOI.

### BFstructural_docker_results.pdf: this is a .pdf generated fromt eh BFstructural_docker_results.Rmd via the BFstructural_dockerImage.tar.gz docker image.

### BFstructural_docker_results.Rmd is a R markdown document that will generate a .pdf of the results. 
Note: if running this file outside the docker image, exact replication of results is not garenteed. If project is first loaded, then package versions will match due to the use of the package renv (will load project specific version of R packages listed in renv.lock). However, this will not resolve opperating system dependent discrepencies.

### Data
1) BFstructural_Alldata.csv: contains all behavioral and demographic data
2) BFstructural_covars.csv: contains common structural covariates
3) MRIstruct_FinalRatings_ICC.csv: contains QC ratings by independent raters
4) TPMall_BFstructural_ROI_dat_neuromorphometrics.csv: extracted grey matter volumes for the neuromorphometrics atlas using CAT. Only hippocamapal volumes were used do to apriori theoretical model

### DockerImage

BFstructural_dockerImage.tar.gz is the docker image for this project and should replicate reported results. To use image, download to your system and use the terminal to execute the following steps:

1) Load docker image: 
        docker load BFstructural_dockerImage.tar.gz

2) Run docker image: 
        docker run -v "$(pwd):/project/output" dockerfile
        Note: if on windows machine, you will need to use different syntax for $(pwd) part of the command
