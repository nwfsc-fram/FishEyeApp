# This file has the R code I used to derive R data files

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
