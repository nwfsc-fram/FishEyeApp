#This code reads in the most uptodate data from oracle and then generates the tables needed for FISHEyE
#####################################################################################################
##############################################################################################

#!!!!!!READ THIS SECTION!!!!!!!**

# Information that needs to be changed
# First connect to oracle

#!!! Update this line of code to the username and password of the user and the directory to this file

source("C:/Users/melanie.harsch/Documents/FisheyeDataPrepHarsch/connectioninfo_shiny.R")

#fin.year is the final year of observations. We need this value for deflation. The NRfn function defaults to the maximum year in the revenue data
#However, to ensure that the correct year is being used (last year of complete dataset), set it here 

fin.year <- 2013

# If an error occurs, make sure directory to costdisvars (located in next section) is correct
#############################################################################################
############################################################################################

#! No other code needs to be run. Comments are provided to help user understand the script
######################################################################################################
library(sqldf) #for sql queries
library(RODBC) # to connect to oracle
library(EDCReport)      
library(plyr)
library(dplyr)
library(reshape2)
library(zipcode)
require(magrittr)

###############################################################################
# Generate the revenue data aggregated over fisheries, year, and vessel
# Read in the data
# Run two database queries to pull the data on 1) revenue by year, vesselID, and revenue component and 2) deflation
revfvarcosts <- sqlQuery(ifqpub, 'select * from STEINERER.REVFXVARCOSTS')
defl <- sqlQuery(ifqpub, 'select * from STEINERER.GDPDEFL')
revfvarcosts$EDCREPORTFISHERY <- ifelse(revfvarcosts$EDCREPORTFISHERY=="Non-whiting midwater trawl", "Non-whiting, non-DTS trawl with trawl endorsement",as.character(revfvarcosts$EDCREPORTFISHERY))
#revfvarcosts$FEWERFISHERIES <- fewerfisheriesfn(revfvarcosts$FEWERFISHERIES, breakoutmidwater=F)
revfvarcosts$FEWERFISHERIES <- revfvarcosts$EDCREPORTFISHERY
# Modified the NRfn function so that revenue data for each fisheries by  year and vessel returned
source("C:/Users/melanie.harsch/Documents/FisheyeDataPrepHarsch/NRfnFunction.R")
### !!!!!!!!Now apply the function
vessel.level <- NRfn.mod(revfvarcosts, defl, onlyCSvessels = F, 'sum', listoffisheries=listoffisheries,finalyear=fin.year)
#################################################################################

############################################################################
## Data to calculate revenue based on days at sea (DAS) and metric tons (MTS)
##ROUNDING ISSUE
mtsdas_raw <- sqlQuery(ifqpub, "SELECT VESSEL_ID, YEAR, FISHERYCODE, SUM(DAS) AS DAS, SUM(MTS) AS MTS, SUM(REV) AS REV
                       FROM STEINERER.REVLBSDAS
                       GROUP BY YEAR, VESSEL_ID, FISHERYCODE
                       ORDER BY VESSEL_ID, YEAR, FISHERYCODE;", dec=getOption("."))

mtsdas_raw$fishcode <-  FishConv(mtsdas_raw$FISHERYCODE)

mtsdas_raw <- sqldf("SELECT VESSEL_ID, YEAR, fishcode, SUM(DAS) AS DAS, SUM(MTS) AS MTS, SUM(REV) AS REV
                    FROM mtsdas_raw 
                    GROUP BY YEAR, VESSEL_ID, fishcode
                    ORDER BY VESSEL_ID, YEAR, fishcode;")

mtsdas_raw$FEWERFISHERIES <- fewerfisheriesfn(mtsdas_raw$fishcode, breakoutmidwater=F)
mtsdas_raw <- mtsdas_raw[,-c(3,6)]
mtsdas_raw$DAS <- ifelse(mtsdas_raw$DAS==0, 0.001, mtsdas_raw$DAS)
##################################################################################
# Data to determine if vessel fishing in AK for given year
fishak<-sqlQuery(ifqpub, "select VESSEL_ID, YEAR, 'TRUE' as FSHAK from steinerer.fshak")
###########################################################################


###############################################################################
# vessel length classes
# puts each vessel length into one of the vessel length bins created when using the
# 2009 data
vesselgroupings <- sqlQuery(ifqpub, "select VESSEL_ID, D_NUMBER_RESPONSE, SURVEY_YEAR from edcdata where fullcode in ('VSSLNG')")
vesselgroupings$VSSLNGCLASS <- with(vesselgroupings, ifelse(D_NUMBER_RESPONSE <= 60, "Small vessel (< 60 ft)", ifelse(D_NUMBER_RESPONSE > 60 & D_NUMBER_RESPONSE <= 80, "Medium vessel (> 60 ft, <= 80 ft)", ifelse(D_NUMBER_RESPONSE > 80, "Large vessel (> 80 ft)", "Other"))))
vesselgroupings <- unique(vesselgroupings[,-which(colnames(vesselgroupings)=="D_NUMBER_RESPONSE")])
###############################################################################################################
#
# homeportdata
data(zipcode)
names(zipcode) <- c("zip", "PORT", "STATE", "latitude", "longitude")

# grabbing unique observations from West Coast
zipcodeSub <- zipcode[!duplicated(paste(zipcode$PORT, zipcode$STATE)) & zipcode$STATE%in%c('OR', 'WA', 'CA', 'AK'),]



# converts home ports provided on EDCs into IO-PAC port groups
edchomeports <- sqlQuery(ifqpub, "select distinct text_response HOMPTEDC from edcsurvey.edcdata where fullcode = 'VSSPT' and edcsurvey_dbid in (select edcsurvey_dbid from edcsurvey.edcdata where fullcode = 'HARVEST' and text_response = 'YES')", stringsAsFactors = F)

edchomeports$HOMPTEDCINT <- with(edchomeports,
                                 ifelse(HOMPTEDC == 'BELLINGHAM', 'BELLINGHAM BAY',
                                        ifelse(HOMPTEDC%in%c('COOS BAY','CHARLESTON'), 'CHARLESTON OR COOS BAY',
                                               ifelse(HOMPTEDC == 'CONWAY', 'LA CONNER',
                                                      ifelse(HOMPTEDC%in%c('GARIBALDI', 'PORTLAND'), 'TILLAMOOK OR GARIBALDI',
                                                             ifelse(HOMPTEDC == 'HALF MOON BAY', 'PRINCETON OR HALF MOON BAY',
                                                                    ifelse(HOMPTEDC == 'HARBOR', 'BROOKINGS',
                                                                           ifelse(HOMPTEDC == 'ILWACO', 'ILWACO OR CHINOOK',
                                                                                  ifelse(HOMPTEDC%in%c('NOYO BAY', 'NOYO RIVER', 'NOYO BEACH', 'NOYO', 'NOYO FORT BRAGG'), 'FORT BRAGG',
                                                                                         ifelse(HOMPTEDC%in%c('PILLAR POINT HARBOR','PILLAR PT. HARBOR', 'PILLAR PT HARBOR'),
                                                                                                'PRINCETON OR HALF MOON BAY',
                                                                                                ifelse(HOMPTEDC%in%c('WARRENTON', 'HAMMOND'), 'ASTORIA',
                                                                                                       ifelse(HOMPTEDC == 'PORT SAN LUIS', 'SAN LUIS OBISPO',
                                                                                                              ifelse(HOMPTEDC == 'SOUTH BEND', 'WILLAPA BAY',
                                                                                                                     ifelse(HOMPTEDC%in%c('UNALASKA', 'PETERSBURG', 'KODIAK'), 'ALASKA',
                                                                                                                            ifelse(HOMPTEDC == 'HUMBOLDT BAY', 'EUREKA',
                                                                                                                                   ifelse(HOMPTEDC == 'GIG HARBOR', 'TACOMA',
                                                                                                                                          HOMPTEDC))))))))))))))))

ioports <- sqlQuery(ifqpub, "select STATE, IOPCID, PNAME, AGID, COUNTY from steinerer.pc_conv", stringsAsFactors = F)
ioports$HOMPTEDCINT <- toupper(ioports$PNAME)

distinctedchomeports <- merge(edchomeports, ioports, all.x= T)
names(distinctedchomeports) <- c("HOMPTEDCINT", "HOMPTEDC", "STATE", "PORT", "PNAME", "AGID", "COUNTY") # homeptedc = port that was provided on EDC form, homeptedcint = converts the port from the EDC form into a port group recognized by IO-PAC, state = state from IO-PAC table, port = mixed case version of homeptedcint and the IO-PAC port group, agid = one character agency code, county = county, pname = translation of pcid from pacfin tables

# cleans up the AK home ports
distinctedchomeports$STATE <- ifelse(distinctedchomeports$HOMPTEDCINT == 'ALASKA', 'AK', distinctedchomeports$STATE)
distinctedchomeports$PORT <- ifelse(distinctedchomeports$HOMPTEDCINT == 'ALASKA', 'Alaska', distinctedchomeports$PORT)
distinctedchomeports$PNAME <- ifelse(distinctedchomeports$HOMPTEDCINT == 'ALASKA', 'Alaska', distinctedchomeports$PNAME)
distinctedchomeports$AGID <- ifelse(distinctedchomeports$HOMPTEDCINT == 'ALASKA', 'A', distinctedchomeports$AGID)
distinctedchomeports$COUNTY <- ifelse(distinctedchomeports$HOMPTEDCINT == 'ALASKA', 'Kodiak Island', distinctedchomeports$COUNTY)

distinctedchomeports <- merge(data.frame(distinctedchomeports, PORTTYP = 'home port', stringsAsFactors = F), zipcodeSub, all.x = T)



#  <<deliveryportdata>>= #Focus on home port not delivery port
#   distinctdeliveryports <- data.frame(
#    sqldf("select distinct port, state, county from catchdata where fleet <> 'AS'"),
#    PORTTYP = 'DELIVERY', stringsAsFactors = F)#
#
#distinctportsDelivery <- merge(distinctdeliveryports, zipcodeSub, all.x = T)

#@

#  <<allports>>=
#  distinctports <- data.frame(rbind.fill(distinctedchomeports, distinctportsDelivery))
##for not just change distinctports to distincthomeports as not worried about distinctportsDelivery
distinctports <- distinctedchomeports
distinctports <- distinctports[,!(names(distinctports)%in%c('AGID', 'HOMPTEDCINT', 'PNAME'))]

distinctports <- distinctports[!is.na(distinctports$PORT),]

#manually fill in some missing data
#distinctports[104:110,1]<-c("OR","OR","WA","OR","OR","OR","WA")
#distinctports[104:110,2] <- c("Coos Bay","Astoria","South and central WA coast","Newport","Astoria","Newport","Puget Sound")

#distinctports <- merge(distinctedchomeports, zipcodeSub, all.x = T)
distinctports$zip <- ifelse(distinctports$PORT == 'Alaska', '99615', distinctports$zip)
distinctports$zip <- ifelse(distinctports$PORT == 'Puget Sound', '98121', distinctports$zip)
distinctports$zip <- ifelse(distinctports$PORT == 'South and central WA coast', '98614', distinctports$zip)
distinctports$zip <- ifelse(distinctports$PORT == 'North WA coast', '98225', distinctports$zip)

distinctports$latitude <- ifelse(distinctports$PORT == 'Alaska', 57.74663, distinctports$latitude)
distinctports$latitude <- ifelse(distinctports$PORT == 'Puget Sound', 47.43225, distinctports$latitude)
distinctports$latitude <- ifelse(distinctports$PORT == 'South and central WA coast', 46.2824, distinctports$latitude)
distinctports$latitude <- ifelse(distinctports$PORT == 'North WA coast', 48.74758, distinctports$latitude)

distinctports$longitude <- ifelse(distinctports$PORT == 'Alaska', -152.5114, distinctports$longitude)
distinctports$longitude <- ifelse(distinctports$PORT == 'Puget Sound', -121.8034, distinctports$longitude)
distinctports$longitude <- ifelse(distinctports$PORT == 'South and central WA coast', -123.9384, distinctports$longitude)
distinctports$longitude <- ifelse(distinctports$PORT == 'North WA coast', -122.4851, distinctports$longitude)

# inserting zipcodes for port groups that aren't associated with a specific city
distinctports[distinctports$PORT == 'North WA coast',-c(1:5)] <- zipcode[zipcode$PORT == 'Bellingham'&zipcode$STATE == 'WA',][1,c(1,4,5)]
distinctports[distinctports$PORT == 'Puget Sound',-(1:5)] <- zipcode[zipcode$PORT == 'Gig Harbor'& zipcode$STATE == 'WA',][1,c(1,4,5)]
distinctports[distinctports$PORT == 'South and central WA coast',-(1:5)] <- zipcode[zipcode$PORT == 'South Bend'& zipcode$STATE == 'WA',][1,c(1,4,5)]
distinctports[distinctports$PORT == 'Other Washington',-(1:5)] <- zipcode[zipcode$PORT == 'Chelan'& zipcode$STATE == 'WA',][1,c(1,4,5)]
distinctports[distinctports$PORT == 'Alaska',-(1:5)] <- zipcode[zipcode$PORT == 'Kodiak'& zipcode$STATE == 'AK',][1,c(1,4,5)]

#manually fill in missing data
#distinctports[c(105,108), 6:8] <- zipcode[zipcode$PORT == 'Astoria'& zipcode$STATE == 'OR',][1,c(1,4,5)]
#distinctports[c(107,109), 6:8] <- zipcode[zipcode$PORT == 'Newport'& zipcode$STATE == 'OR',][1,c(1,4,5)]
#distinctports[104, 6:8] <- zipcode[zipcode$PORT == 'Coos Bay'& zipcode$STATE == 'OR',][1,c(1,4,5)]

# home ports
vesselhomeportdata.int <- dcast(sqlQuery(ifqpub, "select * from edcsurvey.edcdata where fullcode = 'VSSPT' and edcsurvey_dbid in (select edcsurvey_dbid from edcsurvey.edcdata where fullcode = 'HARVEST' and text_response = 'YES' and survey_type = 'CATCHER VESSEL')", stringsAsFactors = F), 
                                EDCSURVEY_DBID + VESSEL_ID + ADJ_YEAR ~ FULLCODE, 
                                value.var = 'TEXT_RESPONSE')
names(vesselhomeportdata.int)[names(vesselhomeportdata.int) == 'VSSPT'] <- 'HOMPTEDC'
vesselhomeportdata <- merge(vesselhomeportdata.int, distinctports, all.x = T)

#vesselhomeportdata.int <- sqlQuery(ifqpub, "select VESSEL_ID, SURVEY_YEAR, TEXT_RESPONSE from edcsurvey.edcdata where fullcode = 'VSSPT' and edcsurvey_dbid in (select edcsurvey_dbid from edcsurvey.edcdata where
#                                           survey_type = 'CATCHER VESSEL')", stringsAsFactors = F)


#  vesselhomeportdata.int <- dcast(sqlQuery(ifqpub, "select * from edcsurvey.edcdata where fullcode = 'VSSPT' and edcsurvey_dbid in (select edcsurvey_dbid from edcsurvey.edcdata where
#                                           survey_type = 'CATCHER VESSEL')", stringsAsFactors = F), 
#                                  EDCSURVEY_DBID + VESSEL_ID + ADJ_YEAR ~ FULLCODE)

names(vesselhomeportdata.int)[names(vesselhomeportdata.int) == 'VSSPT'] <- 'HOMPTEDC'
#names(vesselhomeportdata.int)[names(vesselhomeportdata.int) == 'TEXT_RESPONSE'] <- 'HOMPTEDC'
vesselhomeportdata <- merge(vesselhomeportdata.int, distinctports, all.x = T)

vesselhomeportdata <- vesselhomeportdata[ ,c('VESSEL_ID', 'ADJ_YEAR', 'PORT', 'STATE')]
#vesselhomeportdata <- vesselhomeportdata[ ,c('VESSEL_ID', 'SURVEY_YEAR', 'PORT', 'STATE')]
vesselhomeportdata$PORT <- unique(ifelse(vesselhomeportdata$PORT=="Morro Bay", "Monterey", as.character(vesselhomeportdata$PORT)))
vesselhomeportdata[vesselhomeportdata$VESSEL_ID==610567&vesselhomeportdata$ADJ_YEAR==2013,3:4]=vesselhomeportdata[vesselhomeportdata$VESSEL_ID==610567&vesselhomeportdata$ADJ_YEAR==2012,3:4]
names(vesselhomeportdata) <- c('VESSEL_ID', 'SURVEY_YEAR', 'HOMEPT', 'STATE')
vesselhomeportdata <- unique(vesselhomeportdata)
###############################################################################################################
###############################################################################################################
# Now have five datasets
# Vessel size: vesselgroupings[,c(1,3)], 
# Vessel homeport and state: vesselhomeportdata
# Vessel revenue vessel.level
# metric tons and days at sea data:  mtsdas_raw 
# Fished in AK data: fishAkData 
###########################################################################


###############################################################################################################
#Merge into a single data file
dat <- unique(merge(vesselgroupings, vesselhomeportdata, by.x=c("VESSEL_ID","SURVEY_YEAR"), by.y=c("VESSEL_ID","SURVEY_YEAR"), all=T))
#missing vessel data for vessel 593809 in year 2013. Manually add this
dat[dat$VESSEL_ID==593809&dat$SURVEY_YEAR==2013,3:5]<- dat[dat$VESSEL_ID==593809&dat$SURVEY_YEAR==2012,3:5]
dat[dat$VESSEL_ID==594919&dat$SURVEY_YEAR==2013,4]<- "San Francisco"
dat[dat$VESSEL_ID==594919&dat$SURVEY_YEAR==2013,5]<- "CA"
dat[dat$VESSEL_ID==594919&dat$SURVEY_YEAR==2013,3]<- "Medium vessel (> 60 ft, <= 80 ft)"
dat <- unique(dat)

fishak$FSHAK<-as.character(fishak$FSHAK)
dat<-merge(fishak, dat, by.x=c("VESSEL_ID", "YEAR"), by.y=c("VESSEL_ID","SURVEY_YEAR"), all.y=T)

v.level <- merge(mtsdas_raw, vessel.level, by.x=c("VESSEL_ID","YEAR","FEWERFISHERIES"), by.y=c("VESSEL_ID","YEAR","FEWERFISHERIES"), all=T) 

dat <- unique(merge(dat, v.level, by.x=c("VESSEL_ID","YEAR"), by.y=c("VESSEL_ID","YEAR"), all=T))
dat$FSHAK[is.na(dat$FSHAK)]<-'FALSE'

# Beging cleaning up the data 
dat <- subset(dat, YEAR>2008&YEAR<=fin.year)
dat <- subset(dat, is.na(value)==F)
#dat[dat$VESSEL_ID==527718&dat$YEAR==2011,4:7] <-dat[dat$VESSEL_ID==527718&dat$YEAR==2012,4:7][1,] 
dat[dat$VESSEL_ID=='CF8629KY'&dat$YEAR==2010,4:6] <-dat[dat$VESSEL_ID=='CF8629KY'&dat$YEAR==2009,4:6][1,]

dat <- dcast(dat, VESSEL_ID+YEAR+FEWERFISHERIES+FSHAK+VSSLNGCLASS+HOMEPT+STATE+DAS+MTS~shortdescr, value.var ="value", fun.aggregate=sum, na.rm=T)
colnames(dat)[10:14] <- c("Fixed_costs","Revenue","Total_cost_net_revenue","Variable_cost_net_revenue","Variable_costs")

#Generate all catch share vs all non catch shares
allCSfish<-sqldf("select VESSEL_ID, YEAR, FSHAK, VSSLNGCLASS, HOMEPT, STATE, sum(DAS) DAS, sum(MTS) MTS, sum(Fixed_costs) Fixed_costs, sum(Revenue) Revenue, 
                 sum(Total_cost_net_revenue) Total_cost_net_revenue, sum(Variable_cost_net_revenue) Variable_cost_net_revenue, 
                 sum(Variable_costs) Variable_costs, 
                 case when FEWERFISHERIES in ('Other fisheries', 'Crab', 'Shrimp', 'Groundfish fixed gear with fixed gear endorsement') 
                 then 'All Non-Catch Share Fisheries' else 'All Catch Share Fisheries' end as FEWERFISHERIES
                 
                 from dat 
                 group by VESSEL_ID, YEAR, FSHAK, VSSLNGCLASS, HOMEPT, STATE,
                 case when FEWERFISHERIES in ('Other fisheries', 'Crab', 'Shrimp', 'Groundfish fixed gear with fixed gear endorsement') 
                 then 'All Non-Catch Share Fisheries' else 'All Catch Share Fisheries' end")

dat <- rbind(allCSfish[,c(1:2,14,3:13)], dat)


fishrates <- melt(
  ddply(dat, .(YEAR, VESSEL_ID, FEWERFISHERIES,FSHAK,VSSLNGCLASS,HOMEPT,STATE), transform,
        'REV.PERDAY' = Revenue/DAS,
        'REV.PERMTS' = Revenue/MTS,
        'VARCOSTS.PERDAY' = Variable_costs/DAS,
        'VARCOSTS.PERMTS' = Variable_costs/MTS,
        'VARCOSTNETREV.PERDAY' = Variable_cost_net_revenue/DAS,
        'VARCOSTNETREV.PERMTS' = Variable_cost_net_revenue/MTS,
        'FXCOSTS.PERDAY' = Fixed_costs/DAS,
        'FXCOSTS.PERMTS' = Fixed_costs/MTS,
        'TOTALCOSTNETREV.PERDAY' = Total_cost_net_revenue/DAS,
        'TOTALCOSTNETREV.PERMTS' = Total_cost_net_revenue/MTS),
  c('YEAR', 'VESSEL_ID', 'FEWERFISHERIES','FSHAK','VSSLNGCLASS','HOMEPT','STATE', 'MTS', 'DAS', 'Revenue', 'Variable_costs', 'Fixed_costs', 'Variable_cost_net_revenue', 'Total_cost_net_revenue'))

portlenakSUM <- subset(dat, FEWERFISHERIES=="All Catch Share Fisheries"| FEWERFISHERIES=="All Non-Catch Share Fisheries")
portlenakSUM <- as.data.frame(portlenakSUM %>% group_by(VESSEL_ID, STATE,FSHAK,HOMEPT,VSSLNGCLASS, YEAR, FEWERFISHERIES) %>% summarise(Fixed_costs=sum(Fixed_costs), Revenue=sum(Revenue), Total_cost_net_revenue=sum(Total_cost_net_revenue), Variable_cost_net_revenue=sum(Variable_cost_net_revenue), Variable_costs=sum(Variable_costs)))

#################################################################################################################
#Now calculate average and totals over vessels by a) fisheries, b) vessel length, c) homeport, and d) state
# Compute for total values first (not per day or per metric ton)  then compute per day/ per metric ton
fishlong<-melt(dat[, !names(dat) %in% c('MTS', 'DAS','VSSLNGCLASS','HOMEPT','STATE')], id.vars=c('VESSEL_ID', 'YEAR', 'FSHAK','FEWERFISHERIES'))
colnames(fishlong)[5]<-'SUMSTAT'
## Check confidentiality issues
trt1 <- PreTreat(fishlong, variables=.(FEWERFISHERIES, YEAR,SUMSTAT, FSHAK), valvar="value",individual="VESSEL_ID")
trt2 <- PreTreat(fishlong, variables=.(FEWERFISHERIES, YEAR,SUMSTAT), valvar="value",individual="VESSEL_ID")
fishlong.trt <-  cbind(trt1[,-which(colnames(trt1)=="aggvals")], trt2[,which(colnames(trt2)=="finalnum")])
colnames(fishlong.trt)[7]="finalnum2" 
fishlong.trt <- merge(fishlong, fishlong.trt, by=c("VESSEL_ID","FEWERFISHERIES","YEAR","SUMSTAT", "FSHAK"))
fishlong.trt$value <- with(fishlong.trt, ifelse(FSHAK=="FALSE", finalnum, finalnum2))

fisheriesall<-rbind(data.frame(DotDcast(fishlong.trt, YEAR+SUMSTAT+FEWERFISHERIES~., 'value', mean, 'value'), FISHAK='TRUE', SUMCAT='Fisheries', variable='Average'),
                    data.frame(DotDcast(fishlong.trt[fishlong.trt$FSHAK=='FALSE',], YEAR+SUMSTAT+FEWERFISHERIES~., 'value', mean, 'value'), FISHAK='FALSE', SUMCAT='Fisheries', variable='Average'),
                    data.frame(DotDcast(fishlong.trt, YEAR+SUMSTAT+FEWERFISHERIES~., 'value', sum, 'value'), FISHAK='TRUE', SUMCAT='Fisheries', variable='Total'),
                    data.frame(DotDcast(fishlong.trt[fishlong.trt$FSHAK=='FALSE',], YEAR+SUMSTAT+FEWERFISHERIES~., 'value', sum, 'value'), FISHAK='FALSE', SUMCAT='Fisheries', variable='Total'), 
                    data.frame(DotDcast(fishlong.trt, YEAR+SUMSTAT+FEWERFISHERIES~., 'value', length, 'value'), FISHAK='TRUE', SUMCAT='Fisheries', variable='N'),
                    data.frame(DotDcast(fishlong.trt[fishlong.trt$FSHAK=='FALSE',], YEAR+SUMSTAT+FEWERFISHERIES~., 'value', length, 'value'), FISHAK='FALSE', SUMCAT='Fisheries', variable='N'))

fisheriesall$variable<-with(fisheriesall, paste(variable, SUMSTAT, sep=" "))
fisheriesall<-fisheriesall[-2]
colnames(fisheriesall)[2]<-'SUMVAR'



portlenaklong<-melt(portlenakSUM[, !names(portlenakSUM) %in% c('MTS', 'DAS','HOMEPT','STATE')], id.vars=c('VESSEL_ID', 'YEAR', 'FSHAK','VSSLNGCLASS', 'FEWERFISHERIES'))
colnames(portlenaklong)[6]<-'SUMSTAT'
## Check confidentiality issues
trt1 <- PreTreat(portlenaklong, variables=.(VSSLNGCLASS, YEAR,SUMSTAT, FSHAK, FEWERFISHERIES), valvar="value",individual="VESSEL_ID")
trt2 <- PreTreat(portlenaklong, variables=.(VSSLNGCLASS, YEAR,SUMSTAT, FEWERFISHERIES), valvar="value",individual="VESSEL_ID")
portlenaklong.trt <-  cbind(trt1[,-which(colnames(trt1)=="aggvals")], trt2[,which(colnames(trt2)=="finalnum")])
colnames(portlenaklong.trt)[8]="finalnum2" 
portlenaklong.trt <- merge(portlenaklong, portlenaklong.trt, by=c("VESSEL_ID","VSSLNGCLASS","YEAR","SUMSTAT", "FSHAK", "FEWERFISHERIES"))
portlenaklong.trt$value <- with(portlenaklong.trt, ifelse(FSHAK=="FALSE", finalnum, finalnum2))

VSSLNGall<-rbind(data.frame(DotDcast(portlenaklong.trt, YEAR+SUMSTAT+VSSLNGCLASS+FEWERFISHERIES~., 'value', mean, 'value'), FISHAK='TRUE', SUMCAT='Vessel length class', variable='Average'),
                 data.frame(DotDcast(portlenaklong.trt[portlenaklong.trt$FSHAK=='FALSE',], YEAR+SUMSTAT+VSSLNGCLASS+FEWERFISHERIES~., 'value', mean, 'value'), FISHAK='FALSE', SUMCAT='Vessel length class', variable='Average'),
                 data.frame(DotDcast(portlenaklong.trt, YEAR+SUMSTAT+VSSLNGCLASS+FEWERFISHERIES~., 'value', sum, 'value'), FISHAK='TRUE', SUMCAT='Vessel length class', variable='Total'),
                 data.frame(DotDcast(portlenaklong.trt[portlenaklong.trt$FSHAK=='FALSE',], YEAR+SUMSTAT+VSSLNGCLASS+FEWERFISHERIES~., 'value', sum, 'value'), FISHAK='FALSE', SUMCAT='Vessel length class', variable='Total'), 
                 data.frame(DotDcast(portlenaklong.trt, YEAR+SUMSTAT+VSSLNGCLASS+FEWERFISHERIES~., 'value', length, 'value'), FISHAK='TRUE', SUMCAT='Vessel length class', variable='N'),
                 data.frame(DotDcast(portlenaklong.trt[portlenaklong.trt$FSHAK=='FALSE',], YEAR+SUMSTAT+VSSLNGCLASS+FEWERFISHERIES~., 'value', length, 'value'), FISHAK='FALSE', SUMCAT='Vessel length class', variable='N'))
VSSLNGall$variable<-with(VSSLNGall, paste(variable, SUMSTAT, sep=" "))
VSSLNGall<-VSSLNGall[-2]
colnames(VSSLNGall)[2]<-'SUMVAR'


portlenaklong<-melt(portlenakSUM[, !names(portlenakSUM) %in% c('MTS', 'DAS','VSSLNGCLASS','HOMEPT')], id.vars=c('VESSEL_ID', 'YEAR', 'FSHAK','STATE', 'FEWERFISHERIES'))
colnames(portlenaklong)[6]<-'SUMSTAT'
## Check confidentiality issues
trt1 <- PreTreat(portlenaklong, variables=.(STATE, YEAR,SUMSTAT, FSHAK, FEWERFISHERIES), valvar="value",individual="VESSEL_ID")
trt2 <- PreTreat(portlenaklong, variables=.(STATE, YEAR,SUMSTAT, FEWERFISHERIES), valvar="value",individual="VESSEL_ID")
portlenaklong.trt <-  cbind(trt1[,-which(colnames(trt1)=="aggvals")], trt2[,which(colnames(trt2)=="finalnum")])
colnames(portlenaklong.trt)[8]="finalnum2" 
portlenaklong.trt <- unique(merge(portlenaklong, portlenaklong.trt, by=c("VESSEL_ID","STATE","YEAR","SUMSTAT", "FSHAK","FEWERFISHERIES")))
portlenaklong.trt$value <- with(portlenaklong.trt, ifelse(FSHAK=="FALSE", finalnum, finalnum2))

STATEall<-rbind(data.frame(DotDcast(portlenaklong.trt, YEAR+SUMSTAT+STATE+FEWERFISHERIES~., 'value', mean, 'value'), FISHAK='TRUE', SUMCAT='State', variable='Average'),
                data.frame(DotDcast(portlenaklong.trt[portlenaklong.trt$FSHAK=='FALSE',], YEAR+SUMSTAT+STATE+FEWERFISHERIES~., 'value', mean, 'value'), FISHAK='FALSE', SUMCAT='State', variable='Average'),
                data.frame(DotDcast(portlenaklong.trt, YEAR+SUMSTAT+STATE+FEWERFISHERIES~., 'value', sum, 'value'), FISHAK='TRUE', SUMCAT='State', variable='Total'),
                data.frame(DotDcast(portlenaklong.trt[portlenaklong.trt$FSHAK=='FALSE',], YEAR+SUMSTAT+STATE+FEWERFISHERIES~., 'value', sum, 'value'), FISHAK='FALSE', SUMCAT='State', variable='Total'), 
                data.frame(DotDcast(portlenaklong.trt, YEAR+SUMSTAT+STATE+FEWERFISHERIES~., 'value', length, 'value'), FISHAK='TRUE', SUMCAT='State', variable='N'),
                data.frame(DotDcast(portlenaklong.trt[portlenaklong.trt$FSHAK=='FALSE',], YEAR+SUMSTAT+STATE+FEWERFISHERIES~., 'value', length, 'value'), FISHAK='FALSE', SUMCAT='State', variable='N'))
STATEall$variable<-with(STATEall, paste(variable, SUMSTAT, sep=" "))
STATEall<-STATEall[-2]
colnames(STATEall)[2]<-'SUMVAR'

####rates#####
trt1<-(PreTreat(fishrates, variables=.(FEWERFISHERIES, YEAR, FSHAK, variable), valvar="value",individual="VESSEL_ID"))
trt2<-(PreTreat(fishrates, variables=.(FEWERFISHERIES, YEAR, variable), valvar="value",individual="VESSEL_ID"))
fishrates.trt <-  cbind(trt1[,-which(colnames(trt1)=="aggvals")], trt2[,which(colnames(trt2)=="finalnum")])
colnames(fishrates.trt)[7]="finalnum2" 
fishrates.trt <- merge(fishrates, fishrates.trt, by=c("VESSEL_ID","FEWERFISHERIES","YEAR","variable", "FSHAK"))
fishrates.trt$value <- with(fishrates.trt, ifelse(FSHAK=="FALSE", finalnum, finalnum2))
fishrates.trt <- unique(fishrates.trt[,-which(colnames(fishrates.trt)==c("finalnum","finalnum2"))])

fisheriesrates<-rbind(data.frame(DotDcast(fishrates.trt, YEAR+variable+FEWERFISHERIES~., 'value', mean, 'value'), FISHAK='TRUE', SUMCAT='Fisheries'),
                      data.frame(DotDcast(fishrates.trt[fishrates.trt$FSHAK=='FALSE',], YEAR+variable+FEWERFISHERIES~., 'value', mean, 'value'), FISHAK='FALSE', SUMCAT='Fisheries'))
colnames(fisheriesrates)[3]<-'SUMVAR'

psv.fishrates <- subset(fishrates, FEWERFISHERIES=="All Catch Share Fisheries"|FEWERFISHERIES=="All Non-Catch Share Fisheries")
trt1<-(PreTreat(psv.fishrates, variables=.(VSSLNGCLASS, YEAR, FSHAK, FEWERFISHERIES,variable), valvar="value",individual="VESSEL_ID"))
trt2<-(PreTreat(psv.fishrates, variables=.(VSSLNGCLASS, YEAR, FEWERFISHERIES,variable), valvar="value",individual="VESSEL_ID"))
fishrates.trt <-  cbind(trt1[,-which(colnames(trt1)=="aggvals")], trt2[,which(colnames(trt2)=="finalnum")])
colnames(fishrates.trt)[8]="finalnum2" 
fishrates.trt <- merge(fishrates, fishrates.trt, by=c("VESSEL_ID","VSSLNGCLASS","YEAR","variable", "FSHAK","FEWERFISHERIES"))
fishrates.trt$value <- with(fishrates.trt, ifelse(FSHAK=="FALSE", finalnum, finalnum2))
fishrates.trt <- unique(fishrates.trt[,-which(colnames(fishrates.trt)==c("finalnum","finalnum2"))])
VSSLNGrates<-rbind(data.frame(DotDcast(fishrates.trt, YEAR+variable+VSSLNGCLASS+FEWERFISHERIES~., 'value', mean, 'value'), FISHAK='TRUE', SUMCAT='Vessel length class'),
                   data.frame(DotDcast(fishrates.trt[fishrates.trt$FSHAK=='FALSE',], YEAR+variable+VSSLNGCLASS+FEWERFISHERIES~., 'value', mean, 'value'), FISHAK='FALSE', SUMCAT='Vessel length class'))
colnames(VSSLNGrates)[3]<-'SUMVAR'

trt1<-(PreTreat(psv.fishrates, variables=.(HOMEPT, YEAR, FSHAK,FEWERFISHERIES, variable), valvar="value",individual="VESSEL_ID"))
trt2<-(PreTreat(psv.fishrates, variables=.(HOMEPT, YEAR, FEWERFISHERIES, variable), valvar="value",individual="VESSEL_ID"))
fishrates.trt <-  cbind(trt1[,-which(colnames(trt1)=="aggvals")], trt2[,which(colnames(trt2)=="finalnum")])
colnames(fishrates.trt)[8]="finalnum2" 
fishrates.trt <- merge(fishrates, fishrates.trt, by=c("VESSEL_ID","HOMEPT","YEAR","variable", "FSHAK","FEWERFISHERIES"))
fishrates.trt$value <- with(fishrates.trt, ifelse(FSHAK=="FALSE", finalnum, finalnum2))
fishrates.trt <- unique(fishrates.trt[,-which(colnames(fishrates.trt)==c("finalnum","finalnum2"))])
HOMEPTrates<-rbind(data.frame(DotDcast(fishrates.trt, YEAR+variable+HOMEPT+FEWERFISHERIES~., 'value', mean, 'value'), FISHAK='TRUE', SUMCAT='Homeport'),
                   data.frame(DotDcast(fishrates.trt[fishrates.trt$FSHAK=='FALSE',], YEAR+variable+HOMEPT+FEWERFISHERIES~., 'value', mean, 'value'), FISHAK='FALSE', SUMCAT='Homeport'))
colnames(HOMEPTrates)[3]<-'SUMVAR'

trt1<-(PreTreat(psv.fishrates, variables=.(STATE, YEAR,FEWERFISHERIES, FSHAK, variable), valvar="value",individual="VESSEL_ID"))
trt2<-(PreTreat(psv.fishrates, variables=.(STATE, YEAR,FEWERFISHERIES, variable), valvar="value",individual="VESSEL_ID"))
fishrates.trt <-  cbind(trt1[,-which(colnames(trt1)=="aggvals")], trt2[,which(colnames(trt2)=="finalnum")])
colnames(fishrates.trt)[8]="finalnum2" 
fishrates.trt <- merge(fishrates, fishrates.trt, by=c("VESSEL_ID","STATE","YEAR","variable", "FSHAK","FEWERFISHERIES"))
fishrates.trt$value <- with(fishrates.trt, ifelse(FSHAK=="FALSE", finalnum, finalnum2))
fishrates.trt <- unique(fishrates.trt[,-which(colnames(fishrates.trt)==c("finalnum","finalnum2"))])
STATErates<-rbind(data.frame(DotDcast(fishrates.trt, YEAR+variable+STATE+FEWERFISHERIES~., 'value', mean, 'value'), FISHAK='TRUE', SUMCAT='State'),
                  data.frame(DotDcast(fishrates.trt[fishrates.trt$FSHAK=='FALSE',], YEAR+variable+STATE+FEWERFISHERIES~., 'value', mean, 'value'), FISHAK='FALSE', SUMCAT='State'))
colnames(STATErates)[3]<-'SUMVAR'

full.dat<-rbind(fisheriesall, fisheriesrates[,c(1,3:6,2)])
full.dat$FEWERFISHERIES <- NA
full.dat <- rbind(full.dat,  VSSLNGall[,c(1:2,4:7,3)], HOMEPTall[,c(1:2,4:7,3)], STATEall[,c(1:2,4:7,3)],VSSLNGrates[,c(1,3,5:7,2,4)], HOMEPTrates[,c(1,3,5:7,2,4)], STATErates[,c(1,3,5:7,2,4)])
colnames(full.dat)=c("YEAR","VARIABLE","VALUE","FISHAK","CATEGORY","STAT_DESCR","CS")
###
#fisheriesall is the final dataset -- should rename

#######################################################################################
##
##Get data into correct format
##########################################################################################
fromElements <- vector(length = 25)
fromElements <- c(
  "Average Fixed_costs",
  "FXCOSTS.PERDAY", 
  "FXCOSTS.PERMTS",
  "N Fixed_costs",
  "Total Fixed_costs",
  "Average Revenue",
  "REV.PERDAY", 
  "REV.PERMTS", 
  "N Revenue",
  "Total Revenue", 
  "Average Total_cost_net_revenue",
  "TOTALCOSTNETREV.PERDAY", 
  "TOTALCOSTNETREV.PERMTS", 
  "N Total_cost_net_revenue", 
  "Total Total_cost_net_revenue",
  "Average Variable_cost_net_revenue",
  "VARCOSTNETREV.PERDAY", 
  "VARCOSTNETREV.PERMTS",
  "N Variable_cost_net_revenue", 
  "Total Variable_cost_net_revenue",
  "Average Variable_costs", 
  "VARCOSTS.PERDAY", 
  "VARCOSTS.PERMTS", 
  "N Variable_costs", 
  "Total Variable_costs")

toElements <- vector(length = 25)
toElements <- c(
  "Fixed costs, Average per vessel", 
  "Fixed costs, Average per vessel/day", 
  "Fixed costs, Average per vessel/metric-ton", 
  "Fixed costs, N", 
  "Fixed costs, Total",
  "Revenue, Average per vessel", 
  "Revenue, Average per vessel/day", 
  "Revenue, Average per vessel/metric-ton", 
  "Revenue, N", 
  "Revenue, Total",
  "Total cost net revenue, Average per vessel", 
  "Total cost net revenue, Average per vessel/day", 
  "Total cost net revenue, Average per vessel/metric-ton", 
  "Total cost net revenue, N", 
  "Total cost net revenue, Total",
  "Variable cost net revenue, Average per vessel", 
  "Variable cost net revenue, Average per vessel/day", 
  "Variable cost net revenue, Average per vessel/metric-ton", 
  "Variable cost net revenue, N", 
  "Variable cost net revenue, Total",
  "Variable costs, Average per vessel", 
  "Variable costs, Average per vessel/day", 
  "Variable costs, Average per vessel/metric-ton", 
  "Variable costs, N", 
  "Variable costs, Total")

#recode stat_descr variable
full.dat$STAT_DESCR <- plyr::mapvalues(full.dat$STAT_DESCR, fromElements, toElements, warn_missing = T) 
full.dat <- within(full.dat, var <- data.frame(do.call('rbind', strsplit(full.dat$STAT_DESCR, ', '))))
full.dat$SHORTDESC <- full.dat$var$X1
full.dat$STAT <- full.dat$var$X2
full.dat <- full.dat[,-c(6,8)]
# rename category --------------------------------------------------------------

#fromCategory <- c("FISHERY", "HOMEPT", "STATE", "VSSLNGCLASS")

#toCategory <- c("Fisheries", "Homeport", "State", "Vessel length class")

#full.dat$CATEGORY <- plyr::mapvalues(full.dat$CATEGORY, fromCategory, toCategory)

# rename variable -------------------------------------------------------------

fromVariable <- c("Monterey", 
                  "AK", "CA", "OR", "WA")

toVariable <- c("Morro Bay-Monterey",
                "Alaska", "California","Oregon", 
                "Washington")

full.dat$VARIABLE <- plyr::mapvalues(full.dat$VARIABLE, fromVariable, toVariable)
size <- subset(full.dat %>% group_by(YEAR,VARIABLE,FISHAK,CATEGORY,SHORTDESC,STAT,CS) %>% transmute(N=max(VALUE)), STAT=="N")
full.dat <- merge(full.dat, size[,-6], by=c("YEAR","VARIABLE","FISHAK","CATEGORY","SHORTDESC","CS"))
netrevTable <- subset(full.dat, STAT!="N")
netrevTable$VALUE <- ifelse(netrevTable$N<3, NA, netrevTable$VALUE)
netrevTable$SHORTDESCR <- netrevTable$SHORTDESC
netrevTable <- netrevTable[,-5]
netrevTable$YEAR <- as.character(netrevTable$YEAR)
netrevTable$FISHAK <- as.logical(netrevTable$FISHAK)
########
save(netrevTable, file="./netrevTable.RData")
