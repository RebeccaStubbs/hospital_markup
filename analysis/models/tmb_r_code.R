############################################
# Author: Rebecca Stubbs
# Date: 1/30/2017
# Purpose: Prepare data for CPP TMB model
# cd /homes/stubbsrw/THESIS/
###########################################

library(MapSuite)
library(TMB)
library(data.table)

rm(list=ls())

# Main directory
files_dir<-"C:/Users/stubbsrw/Documents/thesis_files/"
code_dir<-"C:/Users/stubbsrw/Documents/us_counties_stubbs_gitrepo/hospital_markup/analysis/models/"
setwd(code_dir)
cpp_version<-"random_intercepts"

# Loading in clean hospital markup data
load(paste0(files_dir,"charges/data_clean/clean.rdata"))
load(paste0(files_dir,"/locations/cnty_crosswalk.rdata"))

########################
# Adding in Covariates #
########################

covars<-c("race_black","poverty","income_median","mx","edu_ba","ethn_hisp","rural","unemployed","uninsured")

# Load in Covariates
  load("J:/Project/us_counties/covariates/counties/prepped_covariates.rdata")
# Load in Mx (age-standardized mortality rate estimated from US Counties project)
  load(paste0(files_dir,"Mx.rdata"))
# Add in MX to Covars
  covar<-merge(covar, Mx,by=c("year","mcnty"))
  covar<-merge(covar,cnty_crosswalk,by="mcnty",allow.cartesian=T)
  # Add in information on % uninsured
  load(paste0(files_dir,"saihe.rdata"))
  covar<-merge(covar,saihe,by=c("cnty", "year"),all.x=T,all.y=F)
  covar<-covar[year %in% unique(clean$year),]
  covar<-covar[!is.na(uninsured),c("cnty","year",covars),with=F]
  
  # Converting covariate values to z-scores
  covar[, uninsured_hypothetical_z:=1-mean(uninsured)/sd(uninsured)]
  covar[, c(covars) := lapply(.SD, function(x) (x - mean(x))/sd(x)), .SDcols=covars]
  
# Add in covariate information to data.frame  
  data<-merge(clean[,list(markup,n_discharge,state_name,cnty,mcnty,prov_id,year)],covar,by=c("cnty", "year"),all.x=T,all.y=F)
  data<-data[!is.na(mx)]
  rm(covar,Mx,clean)

# recode year from 0
  data[, year := as.integer(year - min(unique(data$year)))]

# Adding an intercept column
  data[,Intercept:=1]
  
# Creating 0:n indexes for state, and county
  data[,cnty_i:=as.integer(factor(cnty,levels=unique(cnty)))-1]
  data[,state_i:=as.integer(factor(state_name,levels=unique(state_name)))-1]
  data[,prov_i:=as.integer(factor(prov_id,levels=unique(prov_id)))-1]
  re_crosswalk<-rbind(unique(data[,list(areaname=cnty,area=cnty_i,level="county_re")]),
                      unique(data[,list(areaname=state_name,area=state_i,level="state_re")]),
                      unique(data[,list(areaname=prov_id,area=prov_i,level="prov_re")]))

# Selecting out the covariates to pass to the model    
  covariates<-copy(data[,c("Intercept",covars), with=F])

# Generate log-transformed outcome variable
  data[,log_markup:=log(markup)]
  
# Restrict to 
###################################
# Set up for TMB
###################################
  
  ## Compiling CPP Code
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get rid of the .dll file if it already exists, that way we know the most recent version will go through...
  if (file.exists(paste0(cpp_version,".dll"))) file.remove(paste0(cpp_version,".dll"), paste0(cpp_version))
  compile(paste0(cpp_version,".cpp")) # Compile dat file!
  if (file.exists(paste0(cpp_version,".dll"))) print ("Yep, that seems to have worked!") else stop("Whoah, looks like your code didn't actually compile...")
 
  ## Generate named lists for TMB inputs
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
    Data <- list("outcome_i" = as.vector(data$log_markup),
                 "state_i" = as.vector(data$state_i),
                 "county_i" = as.vector(data$cnty_i),
                 "prov_i" = as.vector(data$prov_i),
                 "year_i" = as.vector(data$year),
                 "X_ij" = as.matrix(covariates), # Matrix of covars given specified variables
                 "n_i" = nrow(data),
                 "n_state" = length(unique(data$state_i)),
                 "n_county" = length(unique(data$cnty_i)),
                 "n_prov" = length(unique(data$prov_i)),
                 "n_year" =length(unique(data$year)),
                 "n_covars"=ncol(as.matrix(covariates))
    )
    
    Params<-list("betas_j"=rep(0,ncol(as.matrix(covariates))),
                 "state_re" = rep(0,length(unique(data$state_i))),
                 "county_re" = rep(0,length(unique(data$cnty_i))),
                 "prov_re" = rep(0,length(unique(data$prov_i))),
                 "year_re" = rep(0,length(unique(data$year))),
                 "log_sd_state_re"=.01,
                 "log_sd_county_re"=.01,
                 "log_sd_prov_re"=.01,
                 "log_sd_year_re"=.01,
                 "log_SDmod"=.01
    ) # This creates a list of betas as for as many columns as you give it
    
    Random_effects<-c("state_re","county_re","prov_re","year_re")


###################################
# Fit Model
###################################
    
## Fitting TMB Model
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Start the clock!
    ptm <- proc.time()
    
    # Load in the TMB objective function
    dyn.load(dynlib(cpp_version))
    
    # Make the objective function and gradient objects
    Obj <- MakeADFun( data=Data, 
                      parameters=Params,
                      random=Random_effects,
                      DLL=cpp_version)
    #if(silent) Obj$env$beSilent() # If you want to supress output, this will turn off 
    
    # Optimize the model
    Opt <- nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr )
    # Get report
    Report <- Obj$report()
    
    time_elapsed<-proc.time()-ptm
    print(time_elapsed)
    
    # Get precision matrix/standard errors for confidence intervals
    sd.out <- sdreport(Obj, getJointPrecision=TRUE)
    time_elapsed<-proc.time()-ptm
    print(time_elapsed)
    
    dyn.unload(dynlib(cpp_version)) # Apparently it's good practice to unload your library after you load it in, just in case!
    
###################################
# Explore Results
###################################
    
# Defining functions to Map and Explore Parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    fixed_effects<-function(sd.out,beta_list=names(covariates)){
      fixed<-summary(sd.out, "fixed", p.value=T)
      parameter<-rownames(fixed)
      fixed<-cbind(parameter,data.table(fixed))
      betas<-fixed[parameter=="betas_j"]
      betas[,parameter:=beta_list]
      betas[,type:="beta"]
      non_betas<-fixed[parameter!="betas_j"]
      non_betas[,type:="fit_param"]
      fixed<-rbind(betas,non_betas)
      fixed[,confint_low:=Estimate+(-1.96*fixed[["Std. Error"]])]
      fixed[,confint_high:=Estimate+(1.96*fixed[["Std. Error"]])]
      return(fixed)
    }
    
    random_effects<-function(sd.out,re_crosswalk=re_crosswalk){
      random<-summary(sd.out, "random")
      level<-rownames(random)
      random<-cbind(level,data.table(random))
      random[, area := (1:.N)-1, by = level]
      random<-merge(random,re_crosswalk,by=c("level","area"))
      return(random)
    }
    
# Reporting out Model Parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    data[,order:=seq(1,nrow(data))]
    fixef<-fixed_effects(sd.out)
    ranef<-random_effects(sd.out,re_crosswalk)
    beta_coefficients<-fixef[type=="beta"]$Estimate
    covariate_contribution<- rowSums(t(t(covariates)*beta_coefficients)) # Getting the covariate part of the linear prediction

    covariates_scenario<-copy(covariates)
    covariates_scenario[,uninsured:=data$uninsured_hypothetical_z]
    covariate_contribution_scenario<- rowSums(t(t(covariates_scenario)*beta_coefficients)) 
    data<-merge(data,ranef[level=="county_re",list(cnty_i=area,cnty_re=Estimate)],by="cnty_i",sort=F)
    data<-merge(data,ranef[level=="state_re",list(state_i=area,state_re=Estimate)],by="state_i",sort=F)
    data<-merge(data,ranef[level=="prov_re",list(prov_i=area,prov_re=Estimate)],by="prov_i",sort=F)
    data[,area_re:=state_re+cnty_re+prov_re]
    
    predictions<-cbind(data[,list(log_markup,area_re)],covariate_contribution, covariate_contribution_scenario)
    predictions[,pred:=covariate_contribution+area_re]
    predictions[,residual:=log_markup-pred]
    predictions[,scenario_pred:=covariate_contribution_scenario+area_re+residual]
    predictions[,markup:=exp(log_markup)]
    predictions[,scenario:=exp(scenario_pred)]
    predictions[,scenario_diff:=markup-scenario]
    data[,scenario:=predictions$scenario]
    data[,scenario_diff:=predictions$scenario_diff]
    
    
    rmse<-mean(predictions$residual^2)^.5
    
    save(data,fixef,ranef,beta_coefficients,covariate_contribution,data,predictions,file=paste0(files_dir,"/results/model_output.Rdata"))
    load(paste0(files_dir,"/results/model_output.Rdata"))
# Constructing residuals from model fit
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
beta_table<-fixef[type=="beta",]
beta_table[,Estimate:=format(Estimate,digits=3)]
beta_table[,ui_95:=paste0(" (",format(confint_low, digits=3,nsmall=2),
                          ",",format(confint_high, digits=3,nsmall=2),")")]

  
# Graphics Based on Results and Modeling
#qqnorm(log(data$markup),main="Normal Q-Q Plot of Markup (log-transformed)")
#qqnorm(predictions$residual,main="Normal Q-Q Plot of Model Residuals")


load(paste0(files_dir,"/charges/data_clean/clean.rdata"))

# Information on properties of the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
length(unique(clean$cnty))
length(unique(clean$prov_id))


# Summary Statistics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
by_prov_id<-clean[,list(min=min(markup),
                        p25=quantile(markup,0.25),
                        p50=quantile(markup,0.5),
                        p75=quantile(markup,0.75),
                        max=max(markup),
                        n=sum(n_discharge),
                        mean=mean(markup),
                        sd=sd(markup)),
                  by=prov_id]

# Standard Deviation in Facility vs. Median Markup
ggplot(data=by_prov_id, aes(p50,sd)) + geom_hex()+theme_tufte()+
  scale_fill_gradientn(colours=rev(wpal("bright_cool")),
                       guide = guide_colorbar(title = "N Facilities",
                                              title.position="top", 
                                              barheight=unit(.6,"snpc"), 
                                              barwidth=unit(.035,"snpc")))+
  xlab("Median Markup")+ylab("Standard Deviation")+
  ggtitle("Median and Standard Devation of Markup")



# By county
load(paste0(files_dir,"/locations/county_carto.rdata"))

# Maps of raw variables 
by_cnty<-clean[year==2014,list(markup=mean(markup),
                               n=sum(n_discharge)),
             by=c("cnty","state_name","cnty_name")]

MapSuite::PolygonMap(map=county_carto,id="cnty",data=by_cnty,variable="markup", 
                     outline=county_carto, outline_color = "grey",
                     map_colors=wpal("warm_darkfire"), histogram=T,
                     map_title="Average Markup in US Counties, 2014",
                     legend_title = "All DRGs")

# Maps of modeled variables 
county_re<-unique(data[,list(cnty,cnty_re,state_re,combined=state_re+cnty_re)])

MapSuite::PolygonMap(map=county_carto,id="cnty",data=county_re,variable="combined", 
                     outline=county_carto, outline_color = "grey",
                     map_colors=c(rev(wpal("cool_blue_bright")),wpal("warm_fire")), histogram=T,
                     map_title="Model Random Effect Values",
                     map_diverging_centervalue = 0,
                     legend_title="State and County Random Effects Combined")

MapSuite::PolygonMap(map=county_carto,id="cnty",data=county_re,variable="state_re", 
                     outline=county_carto, outline_color = "grey",
                     map_colors=c(rev(wpal("cool_blue_bright")),wpal("warm_fire")), histogram=T,
                     map_title="Model Random Effect Values",
                     map_diverging_centervalue = 0,
                     map_colors_limits = c(min(county_re$combined),max(county_re$combined)),
                     legend_title="State Random Effects")

MapSuite::PolygonMap(map=county_carto,id="cnty",data=county_re,variable="cnty_re", 
                     outline=county_carto, outline_color = "grey",
                     map_colors=c(rev(wpal("cool_blue_bright")),wpal("warm_fire")), histogram=T,
                     map_title="Model Random Effect Values",
                     map_diverging_centervalue = 0,
                     map_colors_limits = c(min(county_re$combined),max(county_re$combined)),
                     legend_title="County Random Effects")


