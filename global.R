
#GLOBAL.R
## Allocate memory
options(java.parameters = "-Xmx10g")

## clear console
cat("\014")

## clear global variables
rm(list=ls())


## list of packages required
list.of.packages = c("pwr")

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


#require(devtools)
#nstall_github("hadley/devtools")
#library(devtools)
#install_github("geoffjentry/twitteR")

## data manipultion
library(pwr)
cat("\014")

