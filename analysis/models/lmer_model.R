##############################################################
# Author: Rebecca Stubbs
# Purpose: Create basic graphs and maps for data exploration
###############################################################

library(data.table)
library(MapSuite)
library(geoR) # has variogram fns
library(gridExtra)
library(lme4)
library(sp)

rm(list=ls())

# Main directory
files_dir<-"C:/Users/stubbsrw/Documents/thesis_files/"

# Load cleaned data set and cartographic shapefiles
load(paste0(files_dir,"/locations/county_carto.rdata"))
load(paste0(files_dir,"/charges/data_clean/clean.rdata"))

load("J:/Project/us_counties/covariates/counties/prepped_covariates.rdata")
load(paste0(files_dir,"Mx.rdata"))

covar<-merge(covar,Mx,by=c("mcnty","year"))
save(covar,file=paste0(files_dir,"covar.rdata"))

clean<-merge(clean,covar,by=c("mcnty","year"),all.x=T,all.y=F)
clean[,mcnty:=as.factor(mcnty)]
clean[,state_name:=as.factor(state_name)]
clean[,prov_id:=as.factor(prov_id)]

start_time <- Sys.time() # initiate model fit timing
model<-lmer(data=clean,markup~1+mx+edu_hs+ethn_hisp+race_black+poverty+income_median+log_pop_density+(1|state_name)+(1|mcnty/prov_id))
time<-start_time-Sys.time() # end model fit timing

conf<-confint(model)
effs <- as.data.frame(effect(c("batch"), model))

ran<-ranef(model,augFrame=T)
fix<-fixef(model)
fix<-data.table(name=names(fix),effect=fix)
