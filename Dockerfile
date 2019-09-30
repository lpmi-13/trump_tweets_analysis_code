FROM rocker/r-apt:bionic

RUN apt-get update && \
    apt-get install -y -qq \
      r-cran-dplyr \
      r-cran-ggplot2 \
      r-cran-factominer \
      r-cran-forecast && \
# no binary is available to install this package, so need to compile from source
    R -e "install.packages('factoextra',dependencies=TRUE, repos='http://cran.rstudio.com/')"

# copy over the analysis code
COPY . /home/analysis
WORKDIR /home/analysis

CMD Rscript script.R
