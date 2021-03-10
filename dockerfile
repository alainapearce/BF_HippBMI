FROM rocker/r-ver:4.0.2

RUN apt-get update
RUN apt-get install -y libcurl4-openssl-dev 
RUN apt-get install -y libssl-dev 
RUN apt-get install -y libxml2-dev 
RUN apt-get install -y pandoc
RUN apt-get install -y libxt-dev
RUN apt-get install -y libz-dev
RUN apt-get install -y texlive-latex-extra

#set enivronment variable for version of renv
ENV RENV_VERSION 0.13.0

#install r package remotes
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"

#install renv
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

#get the renv.lock files
WORKDIR /project

#copy .lock file into project/
COPY renv.lock renv.lock

#add required packages
RUN R -e 'renv::consent(provided = TRUE)'
RUN R -e 'renv::restore()'
RUN R -e 'install.packages("rmarkdown")'

#copy relevant files over
COPY BFstructural_docker_results.Rmd BFstructural_docker_results.Rmd

RUN mkdir Data
COPY Data/* /Data/

COPY BFstructural_docker_setup.R BFstructural_docker_setup.R
COPY functions.R functions.R

#render .Rmd file
RUN mkdir output

CMD R -e "rmarkdown::render('BFstructural_docker_results.Rmd', output_file ='output/BFstructural_docker_results.pdf')"

