# This file has the R code used to derive R data files

library(RODBC)
source("S:/EDC/blair/connectioninfoBV.R")

#set up data, from edc report

load("data/dbids.RData")
load("data/vesselgroupings.RData")
load("data/vesselhomeportdata.RData")
load("data/deliveryPortRev.RData")
#################################
# Catcher Vessel Cost Data
#################################

costdat <- sqlQuery(ifqpub, "select * from steinerer.costs")
#rename cost to discost to fit with old code
names(costdat)[4] <- "DISCOST"

fisherycode <- sqlQuery(ifqpub, "select * from steinerer.fisherycodes")
# vesselgroupings from EDCreport 
# vesselhomeportdata from EDCreport
cost.code <- merge(costdat, fisherycode, all.x=TRUE)
#deliveryports
costsdlvr <- merge(cost.code, deliveryPortRev, by= c("VESSEL_ID", "SURVEY_YEAR"), all.x=TRUE)

#Join the subsetting variables to the costs data
#join costs + vessel length
costslng <- join(costsdlvr, vesselgroupings)
#... + homeports

fullcosts <- join(costslng, vesselhomeportdata)

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

# create fewer fisheries
load("S:/EDC/RData Files/fewerfisheriesfn.RData") #this is the function to rename fisheries
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


###########################################
#Catcher Vessel Revenue, MTS, LBS, DAS data
###########################################


revdat <- sqlQuery(ifqpub, "select * from steinerer.REVLBSDAS")

revlng <- merge(revdat, vesselgroupings, all.x=TRUE)

revdelvr<- merge(revlng, deliveryPortRev, by = c("VESSEL_ID", "SURVEY_YEAR"), all.x=TRUE)

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
