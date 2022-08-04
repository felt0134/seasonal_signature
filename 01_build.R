
#setup workspace

#check version
#R.version

#clearworkspace
rm(list=ls())

#load packages
pkgs <- c("raster",'tidyverse','data.table',
          'rstudioapi','readr') #sp and rgdal deleted 1/4/2022
lapply(pkgs, library, character.only = TRUE) 

# Set working directory to local directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

#load functions
source('02_Functions.R')
