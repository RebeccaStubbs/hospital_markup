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

#Standard Deviations of markup within facility 


# # Calculate distance between each facility and each other facility
# spDists(clean[list(lon_albers,lat_albers)],clean[list(lon_albers,lat_albers)])
# 
# # Making a data frame of only the unique facilities
# fac<-unique(clean[,list(prov_id,lon=lon_albers,lat=lat_albers,mcnty)])
# fac_carto<-unique(clean[,list(prov_id,lat_carto,lon_carto,mcnty)])
# 
# ids<-unique(fac$prov_id)
# distance_summary<-list()
# for (i in seq(1,length(ids))){
#   p<-ids[i]
#   print(i)
#   provider<-c(fac[prov_id==p]$lon,fac[prov_id==p]$lat)
#   pnt_mcnty<-fac[prov_id==p]$mcnty
#   dists<-spDistsN1(pts=as.matrix(fac[,list(lon,lat)]),pt=provider,longlat=FALSE)
#   dists<-data.table(dists)
#   dists[,prov_id_1:=p]
#   dists[,prov_id_2:=unique(fac$prov_id)]
#   dists[,dist_km:=dists/1000]
#   dists[,mcnty:=pnt_mcnty]
#   dists[,mcnty_2:=fac$mcnty]
#   n_sub_5<-sum(dists$dist_km<=5)-1
#   dists[,same_county:=ifelse(mcnty==mcnty_2,1,0)]
#   n_sub_5cnty<-sum(dists[dist_km<5]$same_county)-1
#   n_sub_10<-sum(dists$dist_km<=10)-1
#   n_sub_10cnty<-sum(dists[dist_km<10]$same_county)-1
#   n_sub_20<-sum(dists$dist_km<=20)-1
#   n_sub_20cnty<-sum(dists[dist_km<20]$same_county)-1
#   distance_summary[[as.character(p)]]<- data.table(prov_id=p,sub_20=n_sub_20,sub_20cnty=n_sub_20cnty,
#                                                    sub_10=n_sub_10,sub_10cnty=n_sub_10cnty,
#                                                    sub_5=n_sub_5,sub_5cnty=n_sub_5cnty)
# }
# 
# distance_summary<-rbindlist(distance_summary)
# distance_summary[,prop_cnty20:=100*sub_20cnty/sub_20]
# distance_summary[is.nan(prop_cnty20),prop_cnty20:=NA]
# 
# distance_summary[,prop_cnty10:=100*sub_10cnty/sub_10]
# distance_summary[,prop_cnty5:=100*sub_5cnty/sub_5]
# 
# PointMap(coord = fac_carto,
#          x="lon_carto",
#          y="lat_carto",
#          data=distance_summary,
#          id="prov_id",
#          map_colors=wpal("bright_fire"),
#          map_title=paste0("Facility-Level Data"),
#          variable="prop_cnty20",
#          histogram=T,
#          map_NAcolor = "blue")


load("J:/Project/us_counties/covariates/counties/prepped_covariates.rdata")
load(paste0(files_dir,"Mx.rdata"))

covar<-merge(covar,Mx,by=c("mcnty","year"))
save(covar,file=paste0(files_dir,"covar.rdata"))

clean<-merge(clean,covar,by=c("mcnty","year"),all.x=T,all.y=F)

model<-lmer(data=clean,markup~1+mx+edu_hs+ethn_hisp+race_black+poverty+log_pop_density+(1|state_name/mcnty/prov_id))
ranef(state_cnty_re)
fixef(state_cnty_re)


