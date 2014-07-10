# This file has the R code used to derive R data files
library(RODBC)
source("S:/EDC/blair/connectioninfoBV.R")

#set up data, from edc report

load("data/dbids.RData")
load("data/vesselgroupings.RData")
load("data/vesselhomeportdata.RData")
load("data/deliveryPort.RData") # two boats from the steinerer.costs table are not in the EDCFISH_MV table: 603820 (Alutian Challenger)and 504299 (New Life)

# create fewer fisheries
load("S:/EDC/RData Files/fewerfisheriesfn.RData") #this is the function to rename fisheries

names(deliveryPort)[names(deliveryPort) %in% "REV"] <- "REVBYDPORT" #this is will differentiate rev from delivery port data from rev in revlbsdas table
names(deliveryPort)[names(deliveryPort) %in% "YEAR"] <- "SURVEY_YEAR" #consistent with other datasets
names(deliveryPort)[names(deliveryPort) %in% "RWT_LBS"] <- "LBSBYDPORT" #worst var name ever? constitent with rev var from same dataset

vesselhomeportdata$HOMEPT <- with(vesselhomeportdata, ifelse(HOMEPT == "Alaska", NA, 
                                                             ifelse(HOMEPT == "Monterey", "Morro Bay", HOMEPT))) # I recoding AK fisheries to NA. Run in conjucntion with rm.na to remove them from plot inputs
vesselhomeportdata$STATE <- with(vesselhomeportdata, ifelse(STATE == "AK", NA, STATE))

#################################
# Catcher Vessel Cost Data
#################################

costdat <- sqlQuery(ifqpub, "select * from steinerer.costs")
names(costdat)[names(costdat) %in% "COST"] <- "DISCOST" #this is the varname used in the subset coding

fisherycode <- sqlQuery(ifqpub, "select * from steinerer.fisherycodes")
# vesselgroupings from EDCreport 
# vesselhomeportdata from EDCreport
cost.code <- merge(costdat, fisherycode, all.x=TRUE)

#Join the subsetting variables to the costs data
#join costs + vessel length
costslng <- merge(cost.code, vesselgroupings, all.x=TRUE)

#... + homeports

fullcosts <- merge(costslng, vesselhomeportdata, all.x=TRUE)

# add cost categories and codes to costs data: method is from EDC report

fullcosts$COSTTYPCAT <- with(fullcosts ,
                         ifelse(grepl('CX', FULLCODE), 'Fixed costs', 
                                ifelse(grepl('QP', FULLCODE) | grepl('LEP', FULLCODE) | grepl('QS', FULLCODE) | FULLCODE == 'EXDEPRALL', 'other',  
                                       ifelse(grepl('WC', FULLCODE) & !grepl('FG', FULLCODE), 'Variable costs', 'Fixed costs'))))

fullcosts$COSTCAT <- with(fullcosts, 
                         ifelse(FULLCODE%in%c('EXONBQRMALL', 'EXFGRRMWC', 'EXFGRRMSHD', 'EXPQRMALL'), 'caplike',
                                ifelse(grepl('CX', FULLCODE), 'capex',
                                       ifelse(grepl('QP', FULLCODE) | grepl('LEP', FULLCODE) | grepl('QS', FULLCODE), 'prmtqta', 
                                              ifelse(COSTTYP == 'Variable costs', 'varcost', 
                                                     ifelse(COSTTYP == 'Fixed costs', 'fixedcost', 'depr'))))))

# FISHERIES to FEWERFISHERIES
fullcosts$FISHERIES <- sapply(as.character(fullcosts$FISHERY), fewerfisheriesfn)
# recode the "fixed-fixed" boats to "other" 
fullcosts$FISHERIES <- ifelse(fullcosts$FISHERIES == "Groundfish fixed gear with fixed gear endorsement", "Other fisheries", fullcosts$FISHERIES)

#convert some vars to factors
      fullcosts$SURVEY_YEAR <- factor(fullcosts$SURVEY_YEAR)
      fullcosts$VSSLNGCLASS <- factor(fullcosts$VSSLNGCLASS, levels= c("Small vessel ($<$ 60 ft)", "Medium vessel ($>$ 60 ft, $<=$ 80 ft)", "Large vessel ($>$ 80 ft)"))
      fullcosts$FISHERIES <- factor(fullcosts$FISHERIES)
      fullcosts$COSTTYP <- factor(fullcosts$COSTTYP)
      fullcosts$COSTTYPCAT <- factor(fullcosts$COSTTYPCAT)


#clean up a few vars that we are not going to use
fullcosts <- fullcosts[, !names(fullcosts) %in% c("EDCSURVEY_DBID", "FULLCODE", "FISHERYCODE", "FISHERY", "VSSLNG", "HOMEPTlat", "HOMEPTlong", "HOMECOUNTY")]

save(fullcosts, file="U:/vesselSim/vesselSim_app/data/fullcosts.RData")


###########################################################
#Catcher Vessel Revenue, MTS, LBS, DAS, delivery port data
###########################################################

revdat <- sqlQuery(ifqpub, "select * from steinerer.REVLBSDAS")

revlng <- merge(revdat, vesselgroupings, all.x=TRUE)

#deliveryports

revdelvr<- merge(revlng, deliveryPort, by = c("VESSEL_ID", "SURVEY_YEAR"), all.x=TRUE)

rev.code <- merge(revdelvr, fisherycode, all.x=TRUE)

fullrev <- merge(rev.code, vesselhomeportdata, all.x=TRUE)

# FISHERIES to FEWERFISHERIES
fullrev$FISHERIES <- sapply(as.character(fullrev$FISHERY), fewerfisheriesfn)

# recode the "fixed-fixed" boats to "other" 
fullrev$FISHERIES <- ifelse(fullrev$FISHERIES == "Groundfish fixed gear with fixed gear endorsement", "Other fisheries", fullrev$FISHERIES)


#convert to factors
fullrev$SURVEY_YEAR <- factor(fullrev$SURVEY_YEAR)
fullrev$VSSLNGCLASS <- factor(fullrev$VSSLNGCLASS, levels= c("Small vessel ($<$ 60 ft)", "Medium vessel ($>$ 60 ft, $<=$ 80 ft)", "Large vessel ($>$ 80 ft)"))
fullrev$FISHERIES <- factor(fullrev$FISHERIES)


dropvars <- names(fullrev) %in% c("EDCSURVEY_DBID", "FULLCODE", "FISHERYCODE", "FISHERY", "VSSLNG", "HOMEPTlat", "HOMEPTlong", "HOMECOUNTY")

fullrev <- fullrev[!dropvars]

save(fullrev, file="U:/vesselSim/vesselSim_app/data/fullrev.RData")
