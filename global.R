#########################################################
## Rshiny application to analyze mass spectrometry data
## for possible combinations of masses with tolerance range.
##
## Author: Boris Grinshpun
## 2019
##
#########################################################

# Use the following command to run:
# runApp('path/to/app')

setwd('path/to/app')  # set to location of input files
options(stringsAsFactors = F)
source("functions.R")

##########
# Check for Required packages
##########
for (package in c('shiny', 'shinyjs', 'openxlsx')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

##########
# Load data
##########
defaults <- read.table('defaults.csv',header=F, sep=',')

inputfile <- defaults$V2[defaults$V1=='elementsfile']
elementfile <- read.table(inputfile, header=T, sep=',')
elementfile=elementfile[order(elementfile$MW, decreasing = T),]

##########
# Preprocessing
##########
defaultSum <- as.numeric(defaults$V2[defaults$V1=='sum'])
defaultTolerance <- as.numeric(defaults$V2[defaults$V1=='tolerance'])

BaseElements <- elementfile$BaseElements
MW <-  elementfile$MW
Minimums <- elementfile$Minimums
Maximums <- elementfile$Maximums

m.min <- Minimums
m.min[is.na(m.min)] <- ""
for(i in seq(m.min)){
  if(m.min[i] != ""){
    m.min[i] <- paste('; min = ',m.min[i], sep='')
  }
}
m.max <- Maximums
m.max[is.na(m.max)] <- ""
for(i in seq(m.max)){
  if(m.max[i] != ""){
    m.max[i] <- paste('; max = ',m.max[i], sep='')
  }
}

m.restrictions <- paste(m.min, m.max, sep='')

