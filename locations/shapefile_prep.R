############################
# Author: Rebecca Stubbs
# Date: 1/29/2017
# Purpose: Produce RSpatialPolygon objects for 
# county-level shapefiles from Census TigerLine products.
##################################

library(data.table)
library(woodson)

rm(list=ls())

# Listing Directories
  files_dir<-"C:/Users/stubbsrw/Documents/thesis_files/"
  shapefiles_dir<-paste0(files_dir,"/locations/CRUDE/shapefiles/")

# Bringinging in shapefiles using function in Woodson library
  county<-shp_to_Rpolygons(shapefiles_dir,"tl_2016_us_county","GEOID")
  county_carto<-shp_to_Rpolygons(shapefiles_dir,"cb_2015_us_county_20m","GEOID")

# Plotting the difference of the two shapefiles (analysis and cartographic)
  plot(county_carto[county_carto@data$STATEFP=="05",],main="Shapefile for Cartography")
  plot(county[county@data$STATEFP=="05",],main="Shapefile for Analysis")
  
# Transforming to an albers equal area projection centered in the US (so that Alaska isn't terrible)
  albers<-"+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
  county_carto <- spTransform(county_carto, CRS(albers)) 
  county <- spTransform(county, CRS(albers)) 
  
  plot(county_carto,main="Counties before transformation")
  
  # pull out and relocate AK and HI
  alaska <- county_carto[county_carto$STATEFP == "02",]
  alaska@data <- data.frame(alaska@data)
  rownames(alaska@data) <- as.character(alaska@data$GEOID)
  alaska <- maptools::elide(alaska, rotate=-50)
  alaska <- maptools::elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2)
  alaska <- maptools::elide(alaska, shift=c(-2500000, -2500000))
  proj4string(alaska) <- proj4string(county_carto)
  
  hawaii <- county_carto[county_carto$STATEFP == "15",]
  hawaii@data <- data.frame(hawaii@data)
  rownames(hawaii@data) <- as.character(hawaii@data$GEOID)
  hawaii <- maptools::elide(hawaii, rotate=-35)
  hawaii <- maptools::elide(hawaii, shift=c(5400000, -1400000))
  proj4string(hawaii) <- proj4string(county_carto)
  
  # recombine
  county_carto <- county_carto[!county_carto$STATEFP %in% c("02", "15","72"),]
  county_carto@data <- data.frame(county_carto@data)
  rownames(county_carto@data) <- as.character(county_carto@data$GEOID)
  county_carto <- rbind(county_carto, alaska, hawaii)
  county_carto <- county_carto[order(county_carto$GEOID),]
  county_carto@data <- data.table(county_carto@data)
  plot(county_carto,main="Counties after transformation")
  rm(alaska,hawaii)
  
  # Adding on Mcnty and US counties ID fields:
  load("J:/Project/us_counties/locations/counties/merged_counties.rdata")
  county_carto@data[,cnty:=as.numeric(as.character(GEOID))]
  county_carto@data<-merge(county_carto@data,loc,by="cnty",all.X=T,all.Y=F)[order(polygon_order)]
  
  # Eliminating territories 
  county@data<-copy(county@data)
  county@data[,state_id:=as.numeric(as.character(STATEFP))]
  county@data[,cnty:=as.numeric(as.character(GEOID))]
  county<-copy(county[county@data$state_id<=56,])
  county@data[,state_id:=NULL]
  county@data<-merge(county@data,loc,by="cnty",all.X=T,all.Y=F)[order(polygon_order)]
  
  save(county,file=paste0(files_dir,"/locations/county.rdata"))
  save(county_carto,file=paste0(files_dir,"/locations/county_carto.rdata"))  
