# This file has the R code used to derive R data files

library(RODBC)
source("S:/EDC/blair/connectioninfoBV.R")

#set up data, from edc report

load("data/dbids.RData")
load("data/vesselgroupings.RData")
load("data/vesselhomeportdata.RData")

#################################
# Catcher Vessel Cost Data
#################################

costdat <- sqlQuery(ifqpub, "select * from steinerer.costs")
# vesselgroupings from EDCreport 
# vesselhomeportdata from EDCreport

#Join the subsetting variables to the costs data
#join costs + vessel length
costslng <- join(costdat, vesselgroupings, by = intersect(names(costdat), names(vesselgroupings)))
#... + homeports
fullcosts <- join(costslng, vesselhomeportdata, by = intersect(names(costslng), names(vesselhomeportdata)))

# add cost categories and codes to costs data: method is from EDC report

fullcosts$COSTTYP <- with(fullcosts ,
                         ifelse(grepl('CX', FULLCODE), 'Fixed costs', 
                                ifelse(grepl('QP', FULLCODE) | grepl('LEP', FULLCODE) | grepl('QS', FULLCODE) | FULLCODE == 'EXDEPRALL', 'other',  
                                       ifelse(grepl('WC', FULLCODE) & !grepl('FG', FULLCODE), 'Variable costs', 'Fixed costs'))))

fullcosts$COSTCAT <- with(fullcosts, 
                         ifelse(FULLCODE%in%c('EXONBQRMALL', 'EXFGRRMWC', 'EXFGRRMSHD', 'EXPQRMALL'), 'caplike',
                                ifelse(grepl('CX', FULLCODE), 'capex',
                                       ifelse(grepl('QP', FULLCODE) | grepl('LEP', FULLCODE) | grepl('QS', FULLCODE), 'prmtqta', 
                                              ifelse(COSTTYP == 'Variable costs', 'varcost', 
                                                     ifelse(COSTTYP == 'Fixed costs', 'fixedcost', 'depr'))))))


save(fullcosts, file="U:/vesselSim/vesselSim_app/data/fullcosts.RData")


###############################
#Catcher Vessel Revenue, MTS, LBS, DAS data
###############################


revdat <- sqlQuery(ifqpub, "select * from steinerer.REVLBSDAS")

revlng <- join(revdat, vesselgroupings, by = intersect(names(revdat), names(vesselgroupings)))

fullrev <- join(revlng, vesselhomeportdata, by = intersect(names(revlng), names(vesselhomeportdata)))

dropvars <- names(fullrev) %in% c("FISHERY", "FEWERFISHERIES")

fullrev <- fullrev[!dropvars]

save(fullrev, file="U:/vesselSim/vesselSim_app/data/fullrev.RData")
