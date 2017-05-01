###########################
# Purpose: Get insurance covariates
# Date: 4/27/2017
# Author: Rebecca Stubbs
###########################

library(data.table)
library(stringr)

# Load in raw data files
files_dir<-"C:/Users/stubbsrw/Documents/thesis_files/"

#########################################
# Health Insurance Coverage by FIPS/Year
#########################################
  
  filenames <- list.files(path = paste0(files_dir,"saihe_health_insurance_county/clean/"), full.names = T) 
  saihe<-do.call("rbind", lapply(filenames, fread, header = TRUE)) 
  
  saihe[,cnty:=paste0(as.character(statefips),str_pad(as.character(countyfips),width=3,side="left",pad="0"))]

  saihe<-saihe[geocat==50 & # county identifiers
               agecat==0 & # all ages
               racecat==0 & # all races
               sexcat==0 & # both sexes together
               iprcat==0,] # all income levels
  saihe<-saihe[,list(year,cnty,uninsured=PCTUI)]
  