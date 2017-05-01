############################################
# Author: Rebecca Stubbs
# Date: 1/30/2017
# Purpose: Prepare data for CPP TMB model
###########################################

library(MapSuite)
library(TMB)
library(data.table)

rm(list=ls())

# Main directory
files_dir<-"C:/Users/stubbsrw/Documents/thesis_files/"
code_dir<-"C:/Users/stubbsrw/Documents/us_counties_stubbs_gitrepo/hospital_markup/"
covars<-c("race_black","poverty","income_median","log_pop_density")

# Loading in clean hospital markup data
  load(paste0(files_dir,"charges/data_clean/clean.rdata"))

# Load in Covariates
  load("J:/Project/us_counties/covariates/counties/prepped_covariates.rdata")
  years<-unique(clean$year) # getting the years within the cleaned data set
  covar <- covar[year %in% years,c("year","mcnty",covars),with=F]
  
# Load in Mx (age-standardized mortality rate estimated from US Counties project)
  load(paste0(files_dir,"Mx.rdata"))

# Add in MX to Covars
  covar<-merge(covar, Mx,by=c("year","mcnty")); rm(Mx)
  
# Converting covariate values to z-scores
  covar[, c(covars) := lapply(.SD, function(x) (x - mean(x))/sd(x)), .SDcols=covars]

# Merge data into 1 big table
  data<-merge(clean[,list(markup,n_discharge,state_name,cnty,mcnty,prov_id,year)],covar,by=c("mcnty", "year"),all.x=T,all.y=F)
  rm(clean,covar)

# Creating 0:n indexes for state, and county
  data[,cnty_i:=as.integer(factor(cnty,levels=unique(cnty)))-1]
  cnty_ids<-unique(data[,list(cnty,cnty_i)])
  data[,state_i:=as.integer(factor(state_name,levels=unique(state_name)))-1]
  state_ids<-unique(data[,list(state_name,state_i)])
  data[,prov_i:=as.integer(factor(prov_id,levels=unique(prov_id)))-1]
  prov_ids<-unique(data[,list(prov_id,prov_i)])
  
# recode year from 0
  data[, year := as.integer(year - years[1])]
# Adding an intercept column
  data[,Intercept:=1]

# Generating Nesting Structure 
  states_counties_long<-unique(data[,list(state_i,cnty_i)][order(cnty_i)])$state_i
  counties_providers_long<-unique(data[,list(cnty_i,prov_i)][order(prov_i)])$cnty_i


## Compiling CPP Code
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cpp_version<-"basic_linear_predictor"
cpp_path<-paste0(code_dir,"/analysis/models/basic_linear_predictor")
# Get rid of the .dll file if it already exists, that way we know the most recent version will go through...
  if (file.exists(paste0(cpp_path,".dll"))) file.remove(paste0(cpp_path,".dll"), paste0(cpp_path,".o"))
# Compile dat file!
  compile(paste0(cpp_path,".cpp"))
# Check to see if it went through:
  if (file.exists(paste0(cpp_path,".dll"))) print ("Yep, that seems to have worked!") else stop("Whoah, looks like your code didn't actually compile...")

###################################
# Set up named TMB data structures
###################################
  
    Data <- list("outcome_i" = data$markup,
                 "state_i" = as.vector(data$state_i),
                 "county_i" = as.vector(data$cnty_i),
                 "prov_i" = as.vector(data$prov_i),
                 "year_i"= as.vector(data$year),
                 "X_ij" = as.matrix(covariates), # Matrix of covars given specified variables
                 "n_i" = nrow(data),
                 "n_state" = length(unique(data$state_id)),
                 "n_county" = length(unique(data$county_id)),
                 "n_year" = length(unique(data$year)),
                 "n_covars"=ncol(as.matrix(covariates))
    )
    
    Params<-list("betas_j"=rep(0,ncol(as.matrix(covariates))),
                 "state_re" = rep(0,length(unique(data$state_i))),
                 "county_re" = rep(0,length(unique(data$cnty_i))),
                 "prov_re" = rep(0,length(unique(data$prov_i))),
                 "log_sd_state_re"=.01,
                 "log_sd_county_re"=.01,
                 "log_sd_prov_re"=.01,
                 "log_SDmod"=.01,
                 #"log_sd_time_re"=.01,
                 "rho_ts"=0,
                 "ln_sigma_ts"=0
    ) # This creates a list of betas as for as many columns as you give it
    
    Random_effects<-c("state_re","county_re","prov_re")


## Fitting TMB Model
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Remove things unecessary from data memory
    #rm(data)
    
    # Start the clock!
    ptm <- proc.time()
    
    dyn.load(dynlib(cpp_version))
    
    Obj <- MakeADFun( data=Data, 
                      parameters=Params,
                      #random=Random_effects,
                      DLL=cpp_version)
    #if(silent) Obj$env$beSilent() # If you want to supress output, this will turn off 
    Opt <- nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr )
    Report <- Obj$report()
    
    
    time_elapsed<-proc.time()-ptm
    print(time_elapsed)
    
    sd.out <- sdreport(Obj, getJointPrecision=TRUE)
    
    dyn.unload(dynlib(cpp_version)) # Apparently it's good practice to unload your library after you load it in, just in case!
    
    


