# This file has the R code used to derive data files for the explorer app

#####################################################################

#load required libraries and import data

#####################################################################

# Set working directory...once we know where this will be ran from. I use getwd to use relative wd's in source calls. 
# setwd()

home <- "C:/Users/blair.vanderlugt.NMFS/My Documents/vesselSim"

library(RODBC)
library(reshape2)
library(plyr)
library(EDCReport)        
source(paste0(home, "/datasets/connectionInfo.R"))

# specify years for use in EDC table. If you want to change the output year for any table make changes to this list.
years = data.frame(years=c("2009", "2010", "2011", "2012"))

#set up data, from edc report
# these are things that need to be self contained or translated to their slq equivalent 
load(paste0(home,"/datasets/dbids.RData"))
load(paste0(home,"/datasets/vesselgroupings.RData"))
load(paste0(home,"/datasets/vesselhomeportdata.RData"))
load(paste0(home,"/datasets/deliveryPort.RData")) # two boats from the steinerer.costs table are not in the EDCFISH_MV table: 603820 (Alutian Challenger)and 504299 (New Life)

# create fewer fisheries
load("S:/EDC/RData Files/fewerfisheriesfn.RData") #this is the function to rename fisheries

#this is will differentiate vessel level rev from delivery port data.
# names(deliveryPort)[names(deliveryPort) %in% "REV"] <- "REVBYDPORT" 
names(deliveryPort)[names(deliveryPort) %in% "YEAR"] <- "SURVEY_YEAR" 
# names(deliveryPort)[names(deliveryPort) %in% "RWT_LBS"] <- "LBSBYDPORT" 

# some variable recoding to group locations with low frequencies
vesselhomeportdata$HOMEPT <- with(vesselhomeportdata, ifelse(HOMEPT == "Alaska", NA, 
                                                             ifelse(HOMEPT == "Monterey", "Morro Bay", HOMEPT))) # I recoding AK fisheries to NA. Run in conjucntion with rm.na to remove them from plot inputs
vesselhomeportdata$STATE <- with(vesselhomeportdata, ifelse(STATE == "AK", NA, STATE))

############################################################

# Catcher Vessel Cost Data

############################################################

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

fullcosts.melt <- melt(fullcosts, measure.vars = c("DISCOST"))


######################################################################

#Catcher Vessel Revenue, MTS, LBS, DAS

######################################################################

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


# convert to factors
fullrev$SURVEY_YEAR <- factor(fullrev$SURVEY_YEAR)
fullrev$VSSLNGCLASS <- factor(fullrev$VSSLNGCLASS, levels= c("Small vessel ($<$ 60 ft)", "Medium vessel ($>$ 60 ft, $<=$ 80 ft)", "Large vessel ($>$ 80 ft)"))
fullrev$FISHERIES <- factor(fullrev$FISHERIES)


keepvars <- names(fullrev) %in% c("SURVEY_YEAR", "VESSEL_ID", "VSSLNGCLASS", "HOMEPT", "STATE", "FISHERIES", "LBS", "REV", "MTS", "DAS", "CREW")

fullrev <- fullrev[keepvars]

fullrev.melt <- melt(fullrev, measure.vars = c("LBS", "REV", "MTS", "DAS", "CREW"))

###################################################

#Net revenue data

###################################################

# get fullcost and fullrev in the correct shape
# fullrev + fullcosts are create in above code, generated via IFQ db table's steinerer.COSTS and REVLBSDAS

rawcost <- subset(fullcosts, subset = FISHERIES != "Alaska" ,select= c('VESSEL_ID', 'SURVEY_YEAR', 'DISCOST', 'COSTTYPCAT', 'FISHERIES', 'VSSLNGCLASS', 'HOMEPT', 'STATE'))
#rawcost$FISHERIES <- factor(rawcost$FISHERIES)
rawcost <- droplevels(rawcost) # even better
rawcostMelt <- melt(rawcost, measure.var="DISCOST")
rawcostCast <- dcast(rawcostMelt, VESSEL_ID + SURVEY_YEAR + COSTTYPCAT + FISHERIES + VSSLNGCLASS + HOMEPT + STATE ~ variable, fun.aggregate=sum)

rawrev <- subset(fullrev, subset = FISHERIES != "Alaska",select= c('VESSEL_ID', 'SURVEY_YEAR', 'FISHERIES', 'REV', 'VSSLNGCLASS', 'HOMEPT', 'STATE'))
rawrev <- droplevels(rawrev)
rawrevMelt <- melt(rawrev, measure.var = "REV")
rawrevCast <- dcast(rawrevMelt, VESSEL_ID + SURVEY_YEAR + FISHERIES + VSSLNGCLASS + STATE + HOMEPT ~ variable, fun.aggregate=sum)

# var costs
rawcostVar <- rawcostCast[rawcostCast$COSTTYPCAT == 'Variable costs',]
rawcostVar <- dcast(rawcostVar, VESSEL_ID + FISHERIES + VSSLNGCLASS + STATE + HOMEPT + SURVEY_YEAR ~ ., 
                    value.var="DISCOST", sum, na.rm=TRUE)  
names(rawcostVar)[length(names(rawcostVar))] <- "VARCOST"

# fixed costs
rawcostFix <- rawcostCast[rawcostCast$COSTTYPCAT == 'Fixed costs',]
rawcostFix <- dcast(rawcostFix, VESSEL_ID + FISHERIES + VSSLNGCLASS + STATE + HOMEPT + SURVEY_YEAR ~ ., 
                    value.var="DISCOST", sum, na.rm=TRUE)
names(rawcostFix)[length(names(rawcostFix))] <- "FIXEDCOST"

# Other costs
rawcostOther <- rawcostCast[rawcostCast$COSTTYPCAT == 'other',]
rawcostOther <- dcast(rawcostOther, VESSEL_ID + FISHERIES + VSSLNGCLASS + STATE + HOMEPT + SURVEY_YEAR ~ ., 
                      value.var="DISCOST", sum, na.rm=TRUE)
names(rawcostOther)[length(names(rawcostOther))] <- "OTHERCOST"

netrevValues <- join(x=rawrevCast, join(x=rawcostFix, join(rawcostVar, rawcostOther)))

netrevValues <- with(netrevValues, netrevValues[VARCOST > 0,])

netrevValues$VARNETREV <- netrevValues$REV - netrevValues$VARCOST

netrevValues$TOTALNETREV <- netrevValues$VARNETREV - netrevValues$FIXEDCOST

netrev.melt <- melt(netrevValues, id.vars=c("VESSEL_ID", "SURVEY_YEAR", "FISHERIES", "VSSLNGCLASS", "STATE", "HOMEPT"))

 
# # creating an N table
# netrev.n <- dcast(netrevValuesMelt, SURVEY_YEAR + FISHERIES ~ variable, fun.aggregate = length, value.var = "value")
# # changing the name of the N tables
# names(netrev.n)[3:8] <- sapply(names(netrev.n)[3:8], function(x) paste(x, "N", sep="_"))
# netrev.n.melt <- melt(netrev.n, id.vars = c("SURVEY_YEAR", "FISHERIES"), value.name = "N")
# 
# # join the tables together
# netrev.final <- cbind(netrev.mean.melt, netrev.n.melt[4])

####################################################

# deliveryPort

####################################################

deliveryPort.melt <- melt(deliveryPort, measure.vars = c("REV", "RWT_LBS"))

############################################################################

# save a list of variable names and values 
# vesselSim_app uses these to populate selection panels and and to query the correct table

############################################################################

dat.vars <- list(SURVEY_YEAR = unique(years$years),
                 VSSLNGCLASS = levels(fullcosts$VSSLNGCLASS),
                 HOMEPT = unique(fullcosts$HOMEPT),
                 STATE = unique(fullcosts$STATE),
                 COSTTYPCAT = levels(fullcosts$COSTTYPCAT),
                 FISHERIES = levels(fullcosts$FISHERIES),
                 DELIVERYPT = unique(deliveryPort$DELIVERYPT))                    


###########################################################################

# These functions create many two-way tables and apply confidentiality rules to the data using EDCtables
# output is compiled into lists. This output as well as the factor names from dat.vars is the only thing saved to the server
# costRevDelTables handles the creation of cost, revenue and deliver port data tables
# netrevTables handles the creation of net revenue tables
# reference the source code to get more info on how these functions work

###########################################################################

source(paste0(getwd(), "/data/vesselSim_metaData_costRevDelTables.R"))
source(paste0(getwd(), "/data/vesselSim_metaData_netrevTables.R"))

costTabs <- costRevDelTables(data = fullcosts.melt, "DISCOST") 
revTabs <- costRevDelTables(data = fullrev.melt, "REV")
delTabs <- costRevDelTables(data = deliveryPort.melt, "REV")
netrevTabs <- netrevTables(data = netrev.melt)

tabs.out <- c(costTabs, revTabs, delTabs, netrevTabs)



############################################################################

# save the objects that we created

############################################################################

# save var names
save(dat.vars, file = paste0(getwd(),"/data/dat.vars.RData")) 

# save cost, rev, deliverport and netrev tables
save(tabs.out, file = paste0(getwd(),"/data/tabs.out.RData")) 


############################################################################

#last step is to delete everything from memory

############################################################################

# rm(list=ls())

############################################################################

#depricated! this is just an example of how to construct these tables without using the table function

#############################################################################

# # 1. REVENUE TABLES
# # 1.1 SURVEY_YEAR by FISHERIES
# rev.year.fishery.precast <- dcast(fullrev.melt, SURVEY_YEAR + FISHERIES + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == 'REV'))
# rev.year.fishery.mean <- EDCtable(rev.year.fishery.precast, theformula = FISHERIES ~ SURVEY_YEAR, valvar = "REV", functiontyp= "length", yeardf = data.frame(years=c("2009", "2010", "2011", "2012")),dataORtableORN = "data1conf")  
# rev.year.fishery.mean <- melt(rev.year.fishery.mean, id.var="FISHERIES", value.name = "REV", variable.name = "SURVEY_YEAR") #do this during plotting or now?
# 
# # 1.2 SURVEY_YEAR by VESSEL LENGTH
# rev.year.vsslng.precast <- dcast(fullrev.melt, SURVEY_YEAR + VSSLNGCLASS + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == 'REV'))
# rev.year.vsslng.mean <- EDCtable(rev.year.vsslng.precast, theformula = VSSLNGCLASS ~ SURVEY_YEAR, valvar = "REV", functiontyp= "mean", dataORtableORN = "data1conf")  
# rev.year.vsslng.mean <- melt(rev.year.vsslng.mean, id.var="VSSLNGCLASS", value.name = "REV", variable.name = "SURVEY_YEAR") #do this during plotting or now?
# 
# # 1.3 SURVEY_YEAR by HOMEPORT
# rev.year.homept.precast <- dcast(fullrev.melt, SURVEY_YEAR + HOMEPT + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == 'REV'))
# rev.year.homept.mean <- EDCtable(rev.year.homept.precast, theformula = VSSLNGCLASS ~ SURVEY_YEAR, valvar = "REV", functiontyp= "mean", dataORtableORN = "data1conf")
# rev.year.homept.mean <- melt(rev.year.homept.mean, id.var="HOMEPT", value.name = "REV", variable.name = "SURVEY_YEAR") #do this during plotting or now?
# 
# 
# # 1.4 SURVEY_YEAR by DELIVERY PORT
 
# rev.year.delvpt.precast <- dcast(deliveryPort.melt, SURVEY_YEAR + DELIVERYPT + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == 'REVBYDPORT'))
# rev.year.delvpt.mean <- EDCtable(rev.year.delvpt.precast, theformula = DELIVERYPT ~ SURVEY_YEAR, valvar = "REVBYDPORT", functiontyp= "mean", dataORtableORN = "data1conf")
# rev.year.delvpt.mean <- melt(rev.year.delvpt.mean, id.var="DELIVERYPT", value.name = "REVBYDPORT", variable.name = "SURVEY_YEAR") #do this during plotting or now?
# 
# # 2. COST TABLES
 
# # 2.1 SURVEY_YEAR by FISHERIES
# cost.year.fishery.precast <- dcast(fullcost.melt, SURVEY_YEAR + FISHERIES + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == 'DISCOST'))
# cost.year.fishery.mean <- EDCtable(cost.year.fishery.precast, theformula = FISHERIES ~ SURVEY_YEAR, valvar = "DISCOST", functiontyp= "mean", dataORtableORN = "data1conf")
# cost.year.fishery.mean <- melt(cost.year.fishery.mean, id.var="FISHERIES", value.name = "DISCOST", variable.name = "SURVEY_YEAR") #do this during plotting or now?
# 
# 
# # 2.2 SURVEY_YEAR by VESSEL LENGTH
# cost.year.vsslng.precast <- dcast(fullcost.melt, SURVEY_YEAR + VSSLNGCLASS + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == 'DISCOST'))
# cost.year.vsslng.mean <- EDCtable(cost.year.vsslng.precast, theformula = VSSLNGCLASS ~ SURVEY_YEAR, valvar = "DISCOST", functiontyp= "mean", dataORtableORN = "data1conf")  
# cost.year.vsslng.mean <- melt(cost.year.vsslng.mean, id.var="VSSLNGCLASS", value.name = "DISCOST", variable.name = "SURVEY_YEAR") #do this during plotting or now?
# 
# # 2.3 SURVEY_YEAR by HOME PORT
# cost.year.homept.precast <- dcast(fullcost.melt, SURVEY_YEAR + HOMEPT + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == 'DISCOST'))
# cost.year.homept.mean <- EDCtable(cost.year.homept.precast, theformula = HOMEPT ~ SURVEY_YEAR, valvar = "DISCOST", functiontyp= "mean", dataORtableORN = "data1conf")
# cost.year.homept.mean <- melt(cost.year.homept.mean, id.var="HOMEPT", value.name = "DISCOST", variable.name = "SURVEY_YEAR") #do this during plotting or now?
# 
# 
# # 2.4 SURVEY_YEAR by COST TYPE
# cost.year.costtyp.precast <- dcast(fullcost.melt, SURVEY_YEAR + COSTTYPCAT + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == 'DISCOST'))
# cost.year.costtyp.mean <- EDCtable(cost.year.costtyp.precast, theformula = COSTTYPCAT ~ SURVEY_YEAR, valvar = "DISCOST", functiontyp= "mean", dataORtableORN = "data1conf")
# cost.year.costtyp.mean <- melt(cost.year.costtyp.mean, id.var="COSTTYPCAT", value.name = "DISCOST", variable.name = "SURVEY_YEAR") #do this during plotting or now?
# 
# save(list = c("rev.year.fishery.mean", "rev.year.vsslng.mean", "rev.year.homept.mean", "rev.year.delvpt.mean", "cost.year.fishery.mean", "cost.year.vsslng.mean", "cost.year.homept.mean", "cost.year.costtyp.mean"), file= "U:/vesselSim/vesselSim_app/data/tables.RData")
 
# save(dat.vars, file = "U:/vesselSim/vesselSim_app/data/dat.vars.RData")

## more tables: exlude AK tables, delivery/homept states, netrev. 

