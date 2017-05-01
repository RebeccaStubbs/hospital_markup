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
    
# Add on information about each provider
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Getting the IME GME files
    
#  Info on DSH (disproportionate share hospital):
#  https://www.cms.gov/medicare/medicare-fee-for-service-payment/acuteinpatientpps/dsh.html
  # The DSH patient percentage is equal to the sum of the percentage of Medicare inpatient days 
  # attributable to patients eligible for both Medicare Part A and Supplemental Security Income (SSI),
  # and the percentage of total inpatient days attributable to patients eligible for Medicaid by not 
  # Medicare Part A. The DSH patient percentage is defined as:
  # DSH Patient Percent = (Medicare SSI Days / Total Medicare Days) + (Medicaid, Non-Medicare Days / Total Patient Days)

# https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Cost-Reports/DOCS/HCRIS-FAQ.pdf
  # Q. What do IME 1, IME 2, DSH 1, and DSH 2 mean in the IME_GME report?
  # A. You should add IME 1 and IME 2 together to get a total of IME for the year.
  # These amounts are reported at different times during the reporting period and are
  # captured separately in the cost report.
  # This is the same for the DSH. 

# What do these things mean?
    # IME: Indirect Medical Education. Additoinal payment based on "IME adjustment factor"- ratio of residents to beds. 
    
# Load in IME files, and combine across years to 1 data set into a list
  ime_gme <- lapply(2010:2016, function(yr){
        data<-fread(paste0(files_dir,"/cms_hospital_level_data/hosp10-reports/IME_GME/IME_GME",yr,".csv"))
        data[,year:=yr]
        data[,prov_id:=PROVIDER_NUMBER]
        # data<-data[,list(prov_id=PROVIDER_NUMBER,
        #                        year=yr,
        #                        status=STATUS,
        #                        dsh_percentage=DSH_SHARE_PERCENTAGE,
        #                        dsh_total=sum(as.numeric(DSH1),as.numeric(DSH2),as.numeric(DSH3),na.rm=T), # "Disproportionate Share Hospital"
        #                        ime_total=sum(as.numeric(IME1),as.numeric(IME2),as.numeric(IME3),na.rm=T), # "Indirect Medical Education"
        #                        beds_total=TOTAL_HOSPITAL_BEDS,
        #                        bed_days_total=TOTAL_HOSPITAL_BED_DAYS_AVAILABLE,
        #                        days_medicare=TOTAL_HOSPITAL_MEDICARE_DAYS,
        #                        days_medicaid=TOTAL_HOSPITAL_MEDICAID_DAYS,
        #                        days_hospita=TOTAL_HOSPITAL_DAYS,
        #                        interns_residents=INTERNS_AND_RESIDENTS,
        #                        employees=TOTAL_HOSPITAL_EMPLOYEES_ON_PAYROL,
        #                        discharges_total=TOTAL_HOSPITAL_DISCHARGES,
        #                        discharges_medicare=TOTAL_HOSPITAL_MEDICARE_DISCHARGES,
        #                        discharges_medicaid=TOTAL_HOSPITAL_MEDICAID_DISCHARGES
        #)]
        data    # return the dataframe
      })
  
# Combine different years in list into 1 data frame with unique values only, and most up-to-date information
  ime_gme <- unique(rbindlist(ime_gme))
  ime_gme[,dup:=1]
  ime_gme[,dupsum:=sum(dup),by=c("prov_id","year")]
  View(ime_gme[dupsum>1])
  

  clean[,dup:=1]
  Cl<-merge(clean,ime_gme,by=c("year","prov_id"),all.x=T,all.y=F)
  Cl[,dups2:=sum(dup),by=c("year","prov_id","drg_code")]
  View(Cl[dups2>1])

  GNRL_CNTL_TYPE_CD,"type"
  01=CHURCH
  02=PRIVATE (NOT FOR PROFIT)
  03=OTHER (SPECIFY)
  04=PRIVATE (FOR PROFIT)
  05=FEDERAL
  06=STATE
  07=LOCAL
  08=HOSPITAL DISTRICT OR AUTHORITY
  09=PHYSICIAN OWNERSHIP
  10=TRIBAL

  Medical School Affiliation
  MDCL_SCHL_AFLTN_CD
  1=MAJOR
  2=LIMITED
  3=GRADUATE
  4=NO AFFILIATION
  
  PHYSN_CNT  n_physicians
  RN_CNT n_nurses
  
##############################################
# Load in provider of service information
###############################################
  # Load in IME files, and combine across years to 1 data set into a list
  pos_files <- lapply(11:14, function(yr){
    data<-fread(paste0(files_dir,"charges/data_raw/CRUDE/provider_of_services/POS_OTHER_DEC",yr,".csv"))
    data<-data[PRVDR_CTGRY_CD==1]
    data[,cnty:=paste0(as.character(FIPS_STATE_CD),str_pad(as.character(FIPS_CNTY_CD),width=3,side="left",pad="0"))]
    stetnames(data,"INTRMDRY_CARR_CD","ambulance_carrier")
    stetnames(data,"BED_CNT","total_beds")
    
    data[,year:=2000+yr]
    data[,prov_id:=PROVIDER_NUMBER]
    # data<-data[,list(prov_id=PROVIDER_NUMBER,
    #                        year=yr,
    #                        status=STATUS,
    #                        dsh_percentage=DSH_SHARE_PERCENTAGE,
    #                        dsh_total=sum(as.numeric(DSH1),as.numeric(DSH2),as.numeric(DSH3),na.rm=T), # "Disproportionate Share Hospital"
    #                        ime_total=sum(as.numeric(IME1),as.numeric(IME2),as.numeric(IME3),na.rm=T), # "Indirect Medical Education"
    #                        beds_total=TOTAL_HOSPITAL_BEDS,
    #                        bed_days_total=TOTAL_HOSPITAL_BED_DAYS_AVAILABLE,
    #                        days_medicare=TOTAL_HOSPITAL_MEDICARE_DAYS,
    #                        days_medicaid=TOTAL_HOSPITAL_MEDICAID_DAYS,
    #                        days_hospita=TOTAL_HOSPITAL_DAYS,
    #                        interns_residents=INTERNS_AND_RESIDENTS,
    #                        employees=TOTAL_HOSPITAL_EMPLOYEES_ON_PAYROL,
    #                        discharges_total=TOTAL_HOSPITAL_DISCHARGES,
    #                        discharges_medicare=TOTAL_HOSPITAL_MEDICARE_DISCHARGES,
    #                        discharges_medicaid=TOTAL_HOSPITAL_MEDICAID_DISCHARGES
    #)]
    data    # return the dataframe
  })
  
  
  
  save(clean,file=paste0(files_dir,"/charges/data_clean/clean.rdata"))
