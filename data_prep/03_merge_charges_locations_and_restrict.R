################################################################################
# Author: Rebecca Stubbs
# Date: 1/25/2017
# Purpose: Merge together locations and charge data for analysis; 
#         drop observations that:
#                         - Didn't geocode correctly (were a PO box)
################################################################################

library(data.table)
library(woodson)

rm(list=ls())

# Main directory
files_dir<-"C:/Users/stubbsrw/Documents/thesis_files/"

# Loading locations, cleaned/combined data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  load(paste0(files_dir,"/locations/loc.rdata"))
  load(paste0(files_dir,"/charges/data_raw/raw.rdata"))
  
  clean<-merge(raw,loc,by=c("prov_id","prov_state")) # Dropping the observations that have no geocoding
  
  clean<-clean[,list(prov_id,
                     year,
                     mcnty,
                     cnty,
                     state_name,
                     cnty_name,
                     drg_code=as.numeric(as.character(drg_code)),
                     lon_carto,
                     lat_carto,
                     lat_albers,
                     lon_albers,
                     n_discharge,
                     markup=charges/medicare_payments,
                     charges,
                     payments,
                     medicare_payments)]
  rm(loc,raw); gc()

# Add on more sensible DRG names and MDG names for better aggregation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  drgs<-fread("C:/Users/stubbsrw/Documents/thesis_files/mdg_drg_codes/drg_mdc.csv")
  mdc<-fread("C:/Users/stubbsrw/Documents/thesis_files/mdg_drg_codes/mdc_names.csv")
  
  drgs<-merge(drgs,mdc,by="mdc_code"); rm(mdc)
  
  clean<-merge(clean,drgs,by="drg_code", all.x=T)
  
  # note: All of the rows that didn't merge were all 237: Major Cardiovascular Procedures w MCC
  # This is in the same range of numbers as MDC 5, so we're just going to assing that now.
    clean[is.na(mdc_code), drg_name:="Major Cardiovascular Procedures w MCC"]
    clean[is.na(mdc_code), mdc_name:="Diseases & Disorders of the Circulatory System"]
    clean[is.na(mdc_code), mdc_code:=5]
  
  save(clean,file=paste0(files_dir,"/charges/data_clean/clean.rdata"))
