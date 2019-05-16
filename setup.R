##################################################
## setup.R : for the ASHviz R markdown notebooks
##################################################
# ASH dump data pkg
install.packages("../Jupyter/ashdat_0.1.0.tar.gz", repos = NULL)
# load libraries
library(repr)
library(ashdat)
library(dplyr)
library(ggplot2)
library(gganimate)
library(png)
library(gifski)
# plotting defaults
options(repr.plot.width=7, repr.plot.height=4.5)
# data frames
ashDF <- ashdat::ASHDUMP1
Events <- ashDF %>% filter( TIME_WAITED > 0 )
###
