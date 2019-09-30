# Stylistic variation on the Donald Trump Twitter account: A linguistic analysis of tweets posted between 2009 and 2018

[link to the original paper](https://doi.org/10.1371/journal.pone.0222062)

## Code and Data

This is the repository/supplemental materials for 'Isobelle Clarke and Jack Grieve. 2019. 
Stylistic variation on the Donald Trump Twitter account: A linguistic analysis of tweets 
posted between 2009 and 2018. Accepted for publication in PloS one.'


- CORPUS contains the @realDonaldTrump Twitter corpus in raw and tagged form. 

- FEATURE_SET contains information on the set of linguistic features analysed for this 
study.

- OUTPUT is where the results will be generated and saved.

- R_ANALYSIS contains the data and code analysis for this study. Please see the original paper for more information.

- TWEET_EXAMPLES contain tweets scored on the 5 dimensions identified in this study. 
Specifically, we present 100 tweets with the highest and lowest coordinates on
each of the five dimension, as well as coordinates for the full set of tweets in the 
corpus scored on the five dimensions.

## Running the analysis yourself (Docker version)

### Installing Docker

- For instructions on how to install docker for Windows 10, go
[here](https://docs.docker.com/docker-for-windows/install/),
or for slightly older Windows computers,
go [here](https://docs.docker.com/toolbox/overview/).

- For instructions on how to install docker for MacOS (at
least El Capitan 10.11), go
[here](https://docs.docker.com/docker-for-mac/install/),
or for slightly older MacOS computers,
go [here](https://docs.docker.com/toolbox/overview/)

- For instructions on how to install docker for Ubuntu (at
least 14.04), go [here](https://docs.docker.com/install/linux/docker-ce/ubuntu/). This link also has options for other
Linux distributions.

### Testing Docker
To test your installation, just type:
`docker --version`
at the terminal/command prompt

A successful install will result in something that looks like:
`Docker version 17.05.0-ce, build 89658be`

### Using Docker 

Once you have docker up and running, the following commands will
help you run the container locally from within the root
directory of the project:


- first build the docker container locally

`docker build -t NAME_FOR_THE_CONTAINER`

- now run it (this should create the output files in your host system)

`docker run -v $(pwd)/OUTPUT:/home/analysis/OUTPUT NAME_FOR_THE_CONTAINER`

and a copy of the figures and charts should be output to the `OUTPUT` directory
in the root of the cloned repo.


## Source documents

A copy of all material is available at [https://osf.io/qhm5z/](https://osf.io/qhm5z/)

Original repository prepared by Jack Grieve (j.grieve@bham.ac.uk) on 2019-08-15. 
