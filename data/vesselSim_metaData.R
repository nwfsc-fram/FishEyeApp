# This file has the R code used to derive R data files
library(RODBC)
library(reshape2)
library(plyr)
library(EDCReport)        
source("S:/EDC/blair/connectioninfoBV.R")
years = data.frame(years=c("2009", "2010", "2011", "2012"))

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

fullcost.melt <- melt(fullcosts, measure.vars = c("DISCOST"))


###########################################################

#Catcher Vessel Revenue, MTS, LBS, DAS

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


keepvars <- names(fullrev) %in% c("SURVEY_YEAR", "VESSEL_ID", "VSSLNGCLASS", "HOMEPT", "STATE", "FISHERIES", "LBS", "REV", "MTS", "DAS", "CREW")

fullrev <- fullrev[keepvars]

fullrev.melt <- melt(fullrev, measure.vars = c("LBS", "REV", "MTS", "DAS", "CREW"))

###################################################

#create data for net rev

###################################################

#get fullcost and fullrev in the correct shape
# these are the input datasets, they come from steinerer.COSTS and REVLBSDAS

rawcost <- subset(fullcosts, subset= FISHERIES != 'Alaska', select= c('VESSEL_ID', 'SURVEY_YEAR', 'DISCOST', 'COSTTYPCAT'))
rawcostMelt <- melt(rawcost, measure.var="DISCOST")
rawcostCast <- dcast(rawcostMelt, VESSEL_ID + SURVEY_YEAR + COSTTYPCAT ~ variable, fun.aggregate=sum)


rawrev <- subset(fullrev, subset= FISHERIES != 'Alaska', select= c('VESSEL_ID', 'SURVEY_YEAR', 'REV'))
rawrevMelt <- melt(rawrev, measure.var = "REV")
rawrevCast <- dcast(rawrevMelt, VESSEL_ID + SURVEY_YEAR ~ variable, fun.aggregate=sum)


#split the cost df into its three cost components. Me thinks there is a plyr solution here

#var costs
rawcostVar <- rawcostCast[rawcostCast$COSTTYPCAT == 'Variable costs',]
rawcostVar <- dcast(rawcostVar, VESSEL_ID + SURVEY_YEAR ~ ., 
                    value.var="DISCOST", sum, na.rm=TRUE)  
names(rawcostVar)[length(names(rawcostVar))] <- "VARCOST"

#fixed costs
rawcostFix <- rawcostCast[rawcostCast$COSTTYPCAT == 'Fixed costs',]
rawcostFix <- dcast(rawcostFix, VESSEL_ID + SURVEY_YEAR ~ ., 
                    value.var="DISCOST", sum, na.rm=TRUE)
names(rawcostFix)[length(names(rawcostFix))] <- "FIXEDCOST"

#Other costs
rawcostOther <- rawcostCast[rawcostCast$COSTTYPCAT == 'other',]
rawcostOther <- dcast(rawcostOther, VESSEL_ID + SURVEY_YEAR ~ ., 
                      value.var="DISCOST", sum, na.rm=TRUE)
names(rawcostOther)[length(names(rawcostOther))] <- "OTHERCOST"

netrevValues <- join(x=rawrevCast, join(x=rawcostFix, join(rawcostVar, rawcostOther)))

netrevValues <- with(netrevValues, netrevValues[VARCOST > 0,])

netrevValues$VARNETREV <- netrevValues$REV - netrevValues$VARCOST

netrevValues$TOTALNETREV <- netrevValues$VARNETREV - netrevValues$FIXEDCOST

netrevValuesMelt <- melt(netrevValues, id.vars=c("VESSEL_ID", "SURVEY_YEAR"))


####################################################

# deliveryPort

####################################################

deliveryPort.melt <- melt(deliveryPort, measure.vars = c("REVBYDPORT", "LBSBYDPORT"))

####################################################

#make some aggregated tables from the above datasets
#notes: for the following tables we will probably need a UI selection to include/drop AK fisheries
# 
#depricated! made that tables func to hand this
# ####################################################
# 
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

# save a list of var names, plz update as you add variables
 
# save(dat.vars, file = "U:/vesselSim/vesselSim_app/data/dat.vars.RData")

## more tables: exlude AK tables, delivery/homept states, netrev. 


###############################################################

# make a function to create lots of tables

###############################################################

tablesFunc <- function(data, measure.var, topic = c("FISHERIES", "VSSLNGCLASS", "HOMEPT", "STATE", "COSTTYPCAT", "DELIVERYPT"), stat = "mean"){
  
  #create an error for user entered measure.vars not in the input dataset
  if (!measure.var %in% levels(data$variable)) stop("measure.var not in data")
  
  # create some empty lists to be filled with tables and table names
  tables <- list()
  table.name <- list()
  
  # limit topic variables to those included in data
  topic.var <- topic[topic %in% names(data)]
  
  for (i in 1:length(topic.var)){
    # fist cast aggregates topic var to the observation level (Vessel level)
    dat <- dcast(data, SURVEY_YEAR + get(topic.var[i]) + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == measure.var))  
    
    # change the name of the topic.var because I can't figure out how to make reshape use the var name as the colname 
    # (ie things like get(topic.var[i] prints verbatim in the column instead of the var name))
    # 
    names(dat)[grep("get", names(dat))] <- "TOPIC"
    
    # confidentialiy rules are put in place (ie, n>3 & 90/10 rule) and data is aggregated to the topic/year level
    dat1 <- EDCtable(dat, theformula = TOPIC ~ SURVEY_YEAR, valvar = measure.var, functiontyp= stat, dataORtableORN = "data1conf")
    
    # This does the same as the previous step, but it computes the N's rather than the values
    datN1 <- EDCtable(dat, theformula = TOPIC ~ SURVEY_YEAR, valvar = measure.var, functiontyp= "length", dataORtableORN = "data1conf")
    
    # the next two steps melt the data so it is nice and tidy for ggplot to use later on
    dat2 <- melt(dat1, id.var= "TOPIC", value.name = measure.var, variable.name = "SURVEY_YEAR") #do this during plotting or now?
    
    # melt for N's table
    datN2 <- melt(datN1, id.var= "TOPIC", value.name = "N", variable.name = "SURVEY_YEAR")
    
    # merge the N's to the values data
    dat.final <- cbind(dat2, datN2[,3])
    
    # change the topic name back to the var name from the input data
    names(dat.final)[1] <- topic.var[i]
    
    # change the name of the N column to "N"
    names(dat.final)[length(dat.final)] <- "N"
    
    # pasting together a serialized table name. I use this to call specific tables in the Shiny app (general form is: data.year.measurevar.stat)
    table.name[[i]] <- tolower(paste(measure.var, "year",topic.var[i], stat, sep=".")) # name of the ith table
    # combining the different summary tables into a list
    tables[[i]] <- data.frame(dat.final)
    
    # naming each of the table's
    names(tables)[[i]] <- table.name[[i]]
    
    # Same operations as above, but this generates the tables that exclude Alaskan fisheries
    if (!"FISHERIES" %in% names(data)) next #I only create these tables for input data that includes a fisheries var (eg, does not work on data like deliveryport)
      
      datNoAK <- dcast(data, SURVEY_YEAR + get(topic.var[i]) + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == measure.var & FISHERIES != "Alaska"))
      # if (!is.null(datNoAK)) print(head(datNoAK)) #debugging
    
      names(datNoAK)[grep("get", names(dat))] <- "TOPIC"
    
      dat1NoAK <- EDCtable(datNoAK, theformula = TOPIC ~ SURVEY_YEAR, valvar = measure.var, functiontyp= stat, dataORtableORN = "data1conf")
    
      datN1NoAK <- EDCtable(datNoAK, theformula = TOPIC ~ SURVEY_YEAR, valvar = measure.var, functiontyp= "length", dataORtableORN = "data1conf")
    
      dat2NoAK <- melt(dat1NoAK, id.var= "TOPIC", value.name = measure.var, variable.name = "SURVEY_YEAR") #do this during plotting or now?
    
      datN2NoAK <- melt(datN1NoAK, id.var= "TOPIC", value.name = "N", variable.name = "SURVEY_YEAR")
    
      dat.final.NoAK <- cbind(dat2NoAK, datN2NoAK[,3])

      names(dat.final.NoAK)[1] <- topic.var[i]
    
      names(dat.final.NoAK)[length(dat.final.NoAK)] <- "N"
      
      # basically, I am starting to add NoAk tables after I finish the first run of tables wich is equal to length(topic.var). It should work, but can I break it? 
      tables[[i + length(topic.var)]] <- data.frame(dat.final.NoAK)
    
      names(tables)[[i + length(topic.var)]] <- paste(table.name[[i]], "noak", sep = ".")
          
  }
  tables
}

# for comparing output
# list(AK=tabs$COST.year.STATE.mean, NoAK=tabs$COST.year.STATE.mean.NoAK)
