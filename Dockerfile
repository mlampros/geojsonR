FROM rocker/rstudio:devel 

 
LABEL maintainer='Lampros Mouselimis' 

 
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update && \ 
 apt-get install -y pandoc pandoc-citeproc git-core libssl-dev libcurl4-openssl-dev && \ 
 apt-get install -y sudo && \ 
 apt-get install -y libarmadillo-dev  && \ 
 apt-get install -y libxml2-dev && \ 
 apt-get install -y libssh2-1-dev && \ 
 apt-get install -y zlib1g-dev && \ 
 R -e "install.packages('devtools', dependencies = TRUE, repos = 'https://cloud.r-project.org/')" && \ 
 R -e "install.packages(c( 'Rcpp', 'R6', 'RcppArmadillo', 'testthat', 'covr', 'knitr', 'rmarkdown', 'remotes' ), repos =  'https://cloud.r-project.org/' )" && \ 
 R -e "remotes::install_github('mlampros/geojsonR', upgrade = 'never', dependencies = FALSE, repos = 'https://cloud.r-project.org/')" && \ 
 apt-get autoremove -y && \ 
 apt-get clean 

 
ENV USER rstudio 

 
