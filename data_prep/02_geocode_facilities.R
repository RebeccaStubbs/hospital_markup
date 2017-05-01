################################################################################
# Author: Rebecca Stubbs
# Date: 1/25/2017
# Purpose: Geocode unique facility addresses down to latutide and longitude.
################################################################################

  library(data.table)
  library(rgeos)
  library(sp)

  rm(list=ls())
  geocode<-F # Whether you still need to geocode the files, or just prep the output.
 
  # Main directory
    files_dir<-"C:/Users/stubbsrw/Documents/thesis_files/"
  
# Geocoding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(geocode){ # if "Geocode" is toggled at the top of the script..
    
    # Using Google's API to pull lat/lon for the different addresses
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Note: This code runs, but breaks the loop when the GOOGLE api can't reach a server,
    # or something is wrong with the address such that the query doesn't work. 
    # As such, there are mulitple files resulting from this script, which have all been stored 
    # individually, in the directory ("thesis_files/locations/crude")
    
      # library(googleway) # this library allows for pulling detailed information from Google's 
      #                    # Geocoding API, using a specific API key, which gets around daily limits by 
      #                    # paying a nominal fee ($.50 per extra 1000 in excess of 2,500 per day)
      # 
      # key <- "my_key_here"  # My unique Google project API key; not gonna post this on github
      # geocoded_addresses<-list() # Making an empty list for the geocoded addresses to be stored in
      # 
      # for (i in 1:nrow(unique_facilities)){ # For each of the unique facilities...
      #     print(i)
      #     google_results<-google_geocode(address = unique_facilities$full_address[i], key = key) # store the google results in an object
      # 
      #     if(google_results$status=="ZERO_RESULTS"){ # if the geocode request had no results...
      #       results<-data.table(prov_id=unique_facilities$prov_id[i],ADR=unique_facilities$full_address[i],LAT=NA,LON=NA,geocode=NA) # propogate with NAs
      #       geocoded_addresses[[i]]<-results #add to the list of results
      #     
      #     }else{ # if there were results from the geocode results...
      #       
      #       # Querying the results from the geocode; storing latitude and longitude. 
      #         latitude<-google_results$results$geometry$location[1][["lat"]]
      #         longitude<-google_results$results$geometry$location[2][["lng"]]
      #       type<-google_results$results$geometry$location_type
      # 
      #       # Making a data.table of the results, adding to the list.
      #         results<-data.table(prov_id=unique_facilities$prov_id[i],ADR=unique_facilities$full_address[i],LAT=latitude,LON=longitude,geocode=type)
      #         geocoded_addresses[[i]]<-results
      #       }
      #   } # closing loop of facilities
      # 
      # addresses<-rbindlist(geocoded_addresses,fill=TRUE) # bind together all of the results into a data table
      # fwrite(addresses,file=paste0(files_dir,"/locations/geocoded_facilities_2894_plus.csv")) # write the results to a file
      # 
    
      ## Aggregating facility geocoding information 
    
      # Loading unique facilities
        load(paste0(files_dir,"/locations/unique_facilities.rdata"))

      # Combine the raw data and store in one file
        # Load in all the addresses from the different files written
        file_1<-fread(file=paste0(files_dir,"/locations/CRUDE/geocoded_facilities_1900.csv"))
        file_2<-fread(file=paste0(files_dir,"/locations/CRUDE/geocoded_facilities_1901_plus.csv"))
        file_3<-fread(file=paste0(files_dir,"/locations/CRUDE/geocoded_facilities_2894_plus.csv"))
      
        # Combine different files
        addresses<-unique(rbindlist(list(file_1,file_2,file_3),use.names=TRUE,fill=TRUE))
        setnames(addresses,"ADR","full_address")
        rm(file_1,file_2,file_3)
        
        geocoded_facilities<-merge(unique_facilities,addresses,by=c("prov_id","full_address"))
        save(geocoded_facilities,file=paste0(files_dir,"locations/geocoded_facilities.rdata"))
        
        }else{
          load(paste0(files_dir,"locations/geocoded_facilities.rdata"))
        } # closing "if-geocode" statement
    
    
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
      geocoded_facilities<-unique(geocoded_facilities[,list(prov_id,LAT,LON,prov_state)])
      geocoded_facilities[,index:=1]
      geocoded_facilities[,by_id:=sum(index),by=c("prov_id")]
      #View(geocoded_facilities[by_id>2,])
      # For facilities with multiple buildings or lat/lons provided, the average lat/lon is taken.
      geocoded_facilities<-geocoded_facilities[!is.na(LAT)|!is.na(LON),list(lon_wgs84=mean(LON),lat_wgs84=mean(LAT)),by=c("prov_id","prov_state")]
      proper_order<-copy(geocoded_facilities)
      
# Discovering the County (and mcnty) of the facilities
      
      load(paste0(files_dir,"/locations/county.rdata"))
      load(paste0(files_dir,"/locations/county_carto.rdata"))

      # Making a SpatialPointsDataFrame
        coords <- cbind(geocoded_facilities$lon_wgs84, geocoded_facilities$lat_wgs84) # points from latitude/longitude taken from Google API
        facilities <- sp::SpatialPointsDataFrame(coords, geocoded_facilities) # make spatial data frame 
        facilities@data<-data.table(facilities@data)
        facilities@data[,point_order:=1:nrow(facilities@data)]
        proj4string(facilities)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" # define projection of the facilities as unprojected lat/lon
        plot(facilities,main="Facilities with WGS84 Projection")
        facilities <- spTransform(facilities, CRS(proj4string(county))) 
        albers_coords<-data.table(facilities@coords)[,list(lon_albers=coords.x1,lat_albers=coords.x2)]
        facilities@data<-cbind(facilities@data,albers_coords)
          
      # Figuring out what polygon the points are within using sp::over
        overlay<-sp::over(facilities,county)
        overlay<-overlay[,list(fips=GEOID,statefips=STATEFP,countyfips=COUNTYFP, cnty=as.numeric(as.character(GEOID)),mcnty,state,state_name,cnty_name,ihme_lc_id=location_id)]
        facilities@data<-cbind(facilities@data,overlay)
     
      # Adding and transforming coordinates for a spatial Points object that has Alaska and Hawaii as inset
        facilities <- spTransform(facilities, CRS(proj4string(county_carto))) # Transforming facilities to lay on top of the county map
        
      ## Move AK and HI in the facilities points layer for making points overlay onto cartographic maps 
        fac_carto <- copy(facilities)
        plot(fac_carto,main="Facilities Before transformation")
        
        # pull out and relocate AK and HI
          alaska <- fac_carto[fac_carto$state == 02,]
          alaska@data <- data.frame(alaska@data)
          rownames(alaska@data) <- as.character(alaska@data$prov_id)
          alaska <- maptools::elide(alaska, rotate=-50)
          alaska <- maptools::elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2)
          alaska <- maptools::elide(alaska, shift=c(-2500000, -2500000))
          proj4string(alaska) <- proj4string(fac_carto)
          
          hawaii <- fac_carto[fac_carto$state == 15,]
          hawaii@data <- data.frame(hawaii@data)
          rownames(hawaii@data) <- as.character(hawaii@data$prov_id)
          hawaii <- maptools::elide(hawaii, rotate=-35)
          hawaii <- maptools::elide(hawaii, shift=c(5400000, -1400000))
          proj4string(hawaii) <- proj4string(fac_carto)
          
        # recombine
          fac_carto <- fac_carto[!fac_carto$state %in% c(2, 15),]
          fac_carto@data <- data.frame(fac_carto@data)
          rownames(fac_carto@data) <- as.character(fac_carto@data$prov_id)
          fac_carto <- rbind(fac_carto, alaska, hawaii)
          fac_carto <- fac_carto[order(fac_carto$prov_id),]
          fac_carto@data <- data.table(fac_carto@data)
          plot(fac_carto,main="Facilities after transformation")
          rm(alaska,hawaii)
          
          carto_coords<-data.table(fac_carto@coords)[,list(lon_carto=coords.x1,lat_carto=coords.x2)]
          facilities@data<-cbind(facilities@data,carto_coords)
          
          save(facilities,file=paste0(files_dir,"/locations/facilities.rdata"))
          
          # Saving location identifiers of facilities
          loc<-copy(facilities@data)
          save(loc,file=paste0(files_dir,"/locations/loc.rdata"))
          
          
          #load("J:/Project/us_counties/covariates/counties/prepped_covariates.rdata")
          
          # 
          # # Mapping the facilities onto the continental US
          # basemap<-wmap(chloropleth_map = county_carto,
          #               geog_id = "mcnty",data=covar[year==2014],
          #               variable="ethn_hisp",
          #               return_map_object_only = TRUE)
          # 
          # map<-basemap+labs(title=paste0("Hospitals overlaid onto % hispanic, cartographic shapefile"))+
          #   geom_point(data = facilities@data, 
          #              aes(x = lon_carto, y= lat_carto), 
          #              size=1, alpha=.6)
          # map