library(shiny)
library(RODBC)
library(ggplot2)
library(EDCReport)
library(reshape2)
library(sqldf)
library(reshape2)
source("data/connectionInfo.R", local=T)

##### yeardbids.RData - YEARS and DBIDS where the entity harvested fish with that vessel

years <- c("2009", "2010", "2011", "2012")

#dbids pull takes a while, lets see how far we can get without it
# dbids <- sqlQuery(ifqpub, "
#   select distinct edcsurvey_dbid from edcdata 
#   where survey_type = 'CATCHER VESSEL' 
#   and edcsurvey_dbid in (
#     select edcsurvey_dbid 
#   from edcsurvey.edcdata 
#   where fullcode = 'HARVEST' 
#     and text_response = 'YES'
#   )") # pulls the dbids where vessels fished


