##########################################################
# Author: Rebecca Stubbs
# Purpose: Combine and Clean raw medicare data files
# Date: Nov 11, 2016
#########################################################

library(data.table)

rm(list=ls())

# Load in useful functions
source("C:/Users/stubbsrw/Documents/us_counties_stubbs_gitrepo/r_shared/check_data.R")

# Load in raw data files
  files_dir<-"C:/Users/stubbsrw/Documents/thesis_files/"

# Reading in Inpatient Files
  inpatient_dir<-paste0(files_dir,"/charges/data_raw/CRUDE/inpatient_charges/")
  raw_11<-fread(paste0(inpatient_dir,"Medicare_Provider_Charge_Inpatient_DRG100_FY2011.csv")); raw_11[,year:=2011] # 2011-2013 has inpatient data for top 100 DRGs
  raw_12<-fread(paste0(inpatient_dir,"Medicare_Provider_Charge_Inpatient_DRG100_FY2012.csv")); raw_12[,year:=2012]
  raw_13<-fread(paste0(inpatient_dir,"Medicare_Provider_Charge_Inpatient_DRG100_FY2013.csv")); raw_13[,year:=2013]
  raw_14<-fread(paste0(inpatient_dir,"Medicare_Provider_Charge_Inpatient_DRGALL_FY2014.csv")); raw_14[,year:=2014] # 2014 has data on inpatient charges for ALL DRGs
  raw<-rbind(raw_11,raw_12,raw_13,raw_14); rm(raw_11,raw_12,raw_13,raw_14)

# Renaming fields to meaningful values
  names(raw)<-c("drg", # Disease reporting group
                "prov_id", # Provider ID
                "prov_name", # Hospital name
                "prov_street", # Provider Street address
                "prov_city", 
                "prov_state", 
                "prov_zip",
                "hrr", # Hospital referral region (HRR)
                "n_discharge", # Total number of discharges from that hospital in that year from that code
                "charges", # the average covered charges for that DRG in that year, at that facility
                "payments", # the average total amount paid for that DRG in that year, at that facility
                "medicare_payments", # What was paid by medicare, on average, for that DRG in that year, at that facility
                "year")

# Eliminate any rows that aren't unique-- there are currently 4147 rows with just NA values.
  raw<-unique(raw); raw<-copy(raw[is.na(prov_id)==F,])
  raw[,full_address:=paste0(prov_street," ",prov_city," ",prov_state," ",prov_zip)]

# Creating a file of only unique locations and names to geocode (02_geocode_facilities.R geocodes the data)
  unique_facilities<-unique(raw[,list(prov_id,prov_name,prov_street,prov_city,prov_state,prov_zip,full_address)])
  save(unique_facilities,file=paste0(files_dir,"/locations/unique_facilities.rdata"))
 
# Creating more sensical columns for DRG codes and names 
  raw[,drg_code:=substr(drg,1,3)]
  raw[,drg_name:=substr(drg,7,length(drg))]
  raw[,drg:=NULL]
  drgs<-unique(raw[,list(drg_code,drg_name)])
  
  save(drgs,file=paste0(files_dir,"/charges/data_clean/drgs.rdata"))
  save(raw,file=paste0(files_dir,"/charges/data_raw/raw.rdata"))
  
# Attach on provider of service information
