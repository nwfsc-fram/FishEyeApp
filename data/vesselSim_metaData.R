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

# revdelvr<- merge(revlng, deliveryPort, by = c("VESSEL_ID", "SURVEY_YEAR"), all.x=TRUE)

rev.code <- merge(revlng, fisherycode, all.x=TRUE)

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


####################################################

#make some aggregated tables from the above datasets
#notes: for the following tables we will probably need a UI selection to include/drop AK fisheries

####################################################

# 1. REVENUE TABLES
fullrev.melt <- melt(fullrev, measure.vars = c("LBS", "REV", "MTS", "DAS", "CREW"))
# 1.1 SURVEY_YEAR by FISHERIES
rev.year.fishery.precast <- dcast(fullrev.melt, SURVEY_YEAR + FISHERIES + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == 'REV'))
rev.year.fishery.mean <- dcast(rev.year.fishery.precast, FISHERIES ~ SURVEY_YEAR, fun.aggregate = mean, value.var="REV")  
rev.year.fishery.mean <- melt(rev.year.fishery.mean, id.var="FISHERIES", value.name = "REV", variable.name = "SURVEY_YEAR") #do this during plotting or now?

# 1.2 SURVEY_YEAR by VESSEL LENGTH
rev.year.vsslng.precast <- dcast(fullrev.melt, SURVEY_YEAR + VSSLNGCLASS + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == 'REV'))
rev.year.vsslng.mean <- dcast(rev.year.vsslng.precast, VSSLNGCLASS ~ SURVEY_YEAR, fun.aggregate = mean, value.var="REV")  
rev.year.vsslng.mean <- melt(rev.year.vsslng.mean, id.var="VSSLNGCLASS", value.name = "REV", variable.name = "SURVEY_YEAR") #do this during plotting or now?

# 1.3 SURVEY_YEAR by HOMEPORT
rev.year.homept.precast <- dcast(fullrev.melt, SURVEY_YEAR + HOMEPT + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == 'REV'))
rev.year.homept.mean <- dcast(rev.year.homept.precast, HOMEPT ~ SURVEY_YEAR, fun.aggregate = mean, value.var="REV")  
rev.year.homept.mean <- melt(rev.year.homept.mean, id.var="HOMEPT", value.name = "REV", variable.name = "SURVEY_YEAR") #do this during plotting or now?


# 1.4 SURVEY_YEAR by DELIVERY PORT
deliveryPort.melt <- melt(deliveryPort, measure.vars = c("REVBYDPORT", "LBSBYDPORT"))
rev.year.delvpt.precast <- dcast(deliveryPort.melt, SURVEY_YEAR + DELIVERYPT + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == 'REVBYDPORT'))
rev.year.delvpt.mean <- dcast(rev.year.delvpt.precast, DELIVERYPT ~ SURVEY_YEAR, fun.aggregate = mean, value.var="REVBYDPORT")  
rev.year.delvpt.mean <- melt(rev.year.delvpt.mean, id.var="DELIVERYPT", value.name = "REVBYDPORT", variable.name = "SURVEY_YEAR") #do this during plotting or now?

# 2. COST TABLES
fullcost.melt <- melt(fullcosts, measure.vars = c("DISCOST"))
# 2.1 SURVEY_YEAR by FISHERIES
cost.year.fishery.precast <- dcast(fullcost.melt, SURVEY_YEAR + FISHERIES + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == 'DISCOST'))
cost.year.fishery.mean <- dcast(cost.year.fishery.precast, FISHERIES ~ SURVEY_YEAR, fun.aggregate = mean, value.var="DISCOST")  
cost.year.fishery.mean <- melt(cost.year.fishery.mean, id.var="FISHERIES", value.name = "DISCOST", variable.name = "SURVEY_YEAR") #do this during plotting or now?


# 2.2 SURVEY_YEAR by VESSEL LENGTH
cost.year.vsslng.precast <- dcast(fullcost.melt, SURVEY_YEAR + VSSLNGCLASS + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == 'DISCOST'))
cost.year.vsslng.mean <- dcast(cost.year.vsslng.precast, VSSLNGCLASS ~ SURVEY_YEAR, fun.aggregate = mean, value.var="DISCOST")  
cost.year.vsslng.mean <- melt(cost.year.vsslng.mean, id.var="VSSLNGCLASS", value.name = "DISCOST", variable.name = "SURVEY_YEAR") #do this during plotting or now?

# 2.3 SURVEY_YEAR by HOME PORT
cost.year.homept.precast <- dcast(fullcost.melt, SURVEY_YEAR + HOMEPT + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == 'DISCOST'))
cost.year.homept.mean <- dcast(cost.year.homept.precast, HOMEPT ~ SURVEY_YEAR, fun.aggregate = mean, value.var="DISCOST")  
cost.year.homept.mean <- melt(cost.year.homept.mean, id.var="HOMEPT", value.name = "DISCOST", variable.name = "SURVEY_YEAR") #do this during plotting or now?


# 2.4 SURVEY_YEAR by COST TYPE
cost.year.costtyp.precast <- dcast(fullcost.melt, SURVEY_YEAR + COSTTYPCAT + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == 'DISCOST'))
cost.year.costtyp.mean <- dcast(cost.year.costtyp.precast, COSTTYPCAT ~ SURVEY_YEAR, fun.aggregate = mean, value.var="DISCOST")  
cost.year.costtyp.mean <- melt(cost.year.costtyp.mean, id.var="COSTTYPCAT", value.name = "DISCOST", variable.name = "SURVEY_YEAR") #do this during plotting or now?

save(list = c("rev.year.fishery.mean", "rev.year.vsslng.mean", "rev.year.homept.mean", "rev.year.delvpt.mean", "cost.year.fishery.mean", "cost.year.vsslng.mean", "cost.year.homept.mean", "cost.year.costtyp.mean"), file= "U:/vesselSim/vesselSim_app/data/tables.RData")

# save a list of var names
dat.vars <- list(SURVEY_YEAR = unique(fullrev$SURVEY_YEAR), 
                 VSSLNGCLASS = unique(fullrev$VSSLNGCLASS), 
                 HOMEPT = unique(fullrev$HOMEPT), 
                 STATE = unique(fullrev$STATE), 
                 FISHERIES = unique(fullrev$FISHERIES),
                 COSTTYPCAT = unique(fullcosts$COSTTYPCAT),
                 DELIVERYPT = unique(deliveryPort$DELIVERYPT))

save(dat.vars, file = "U:/vesselSim/vesselSim_app/data/dat.vars.RData")
