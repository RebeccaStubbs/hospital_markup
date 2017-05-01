library(data.table)
library(woodson)

rm(list=ls())

# Main directory
files_dir<-"C:/Users/stubbsrw/Documents/thesis_files/"


# Loading in data
load(paste0(files_dir,"/data_intermediate/raw.rdata"))
load(paste0(files_dir,"/data_intermediate/geocoded_with_mcnty.rdata"))
load(paste0(files_dir,"/data_intermediate/facilities_sp.rdata"))
load("J:/Project/us_counties/covariates/counties/prepped_covariates.rdata")
load("J:/Project/us_counties/locations/counties/prepped_shp/mcnty_shape_file.rdata") # Loading shapefile with mcnty mappings

albers<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

facilities <- spTransform(facilities, CRS(albers)) # Transforming facilities to lay on top of the county map
mcnty_map <- spTransform(mcnty_map, CRS(albers)) # Transforming facilities to lay on top of the county map

facilities@data<-cbind(facilities@data,data.table(facilities@coords)) # Adding in the new coordinate lat/lon with the albers projection


# Merging the facility information onto the full data set
  
  # Finding the duplicate address-geocode pairs, and visually checking that they are all actually the same, so that we can
  # use prov_id as a primary key alone, without the address field: 
  geocoded_facilities[,index:=1]
  geocoded_facilities[,by_address_and_id:=sum(index),by=c("prov_id","full_address")]
  geocoded_facilities[,by_address_id:=sum(index),by=c("prov_id")]
  #View(geocoded_facilities[by_address_id!=by_address_and_id])
  #View(geocoded_facilities[by_address_and_id>1,])
    # From this exploration, I've confirmed that the prov-id does refer to the same facility, based on checking the
    # names of the facilities, even if the address isn't exactly the same.
    # This means that we can reduce the geocoded_facilities to only prov_id as far as figuring out a lat/lon and
    # what county they are in.
  geocoded_facilities<-unique(geocoded_facilities[,list(prov_id,LAT,LON,mcnty)])
  geocoded_facilities[,index:=1]
  geocoded_facilities[,by_id:=sum(index),by=c("prov_id")]
  #View(geocoded_facilities[by_id>2,])
    # For facilities with multiple buildings or lat/lons provided, the average lat/lon is taken.
  geocoded_facilities<-geocoded_facilities[,list(lon=mean(LON),lat=mean(LAT)),by=c("prov_id")]
  
  
  
  # Mapping the facilities onto the continental US
    basemap<-wmap(chloropleth_map = mcnty_map[!(mcnty_map@data$state_name %in% c("Alaska","Hawaii")),],
                  geog_id = "mcnty",data=covar[year==2014],
                  variable="income_median",
                  return_map_object_only = TRUE)
    
    map<-basemap+labs(title=paste0("Hospitals overlaid onto household median income"))+
      geom_point(data = facilities@data[!(facilities@data$prov_state %in% c("AK","HI")),], 
                 aes(x = coords.x1, y= coords.x2), 
                 size=1, alpha=.6)
    map
  
  