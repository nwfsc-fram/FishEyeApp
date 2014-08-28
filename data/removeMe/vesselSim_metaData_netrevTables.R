
netrevTables <- function(data, topic = c("FISHERIES", "VSSLNGCLASS", "STATE", "HOMEPT"), stat = "mean"){
  
  
  # create some empty lists to be filled with tables and table names
  tables <- list()
  tables.name <- list()
  factor.tables <- list()
  factor.tables.name <- list()
#   tables.out <- list()
  
  # limit topic variables to those included in data
  topic.var <- topic[topic %in% names(data)]
  
  for (i in 1:length(topic.var)){

    # first cast aggregates topic var to the observation level (Vessel level)
    dat <- dcast(data, SURVEY_YEAR + get(topic.var[i]) + VESSEL_ID + variable ~ ., fun.aggregate = sum, na.rm=T)  
    
    # change the name of the topic.var 
    names(dat)[grep("get", names(dat))] <- "TOPIC"
    names(dat)[length(dat)] <- "value"
    
    # second cast uses specified stat
    dat1 <- EDCtable(dat, variable ~ SURVEY_YEAR, valvar = "value", functiontyp = stat, dataORtableORN = "data1conf")
    
    # melt for vessel level data and "topic" var
    dat.melt <- melt(dat1, id.vars = c("variable"), variable.name = "SURVEY_YEAR") 
        
    tables[[i]] <- dat.melt
#     
    tables.name[[i]] <- tolower(paste("netrev", "year" ,topic.var[i] , stat, sep=".")) # name of the ith table  
    
    #naming each of the table's
    names(tables)[[i]] <- tables.name[[i]] 
  
  }
  tables
}
#         for (j in 1:length(tables)){
#   
#         for (b in 1:length(levels(tables[[i]][,2]))){ #this is grabbing the length of the first table TOPIC factor it comes across so it equals levels(FISHERIES) every iteration
# 
#           dat.melt.sub <- subset(tables[[i], TOPIC == levels(tables[[i]][,2])[b])
# 
#           dat.by.cat <- EDCtable(dat.melt.sub, variable ~ SURVEY_YEAR, valvar = 'value', functiontyp=stat,dataORtableORN = "data1conf")
# 
# #         print(str(dat.by.cat))
# 
#           factor.tables[[b]] <- dat.by.cat
#           factor.tables.name[[b]] <- sapply(strsplit(levels(tables[[i]][,2])[b], split = " "), function(x) paste(x[1:length(x)], collapse="_"))
#           names(factor.tables)[[b]] <- factor.tables.name[[b]]
#           factor.tables 
#         } 
   




#1) take a list of "topics" which are factor variables for each variable of interest
#2) for each topic variable create a summary tables for each individual factor for year and netrev category
# ex. 1) topic == "FISHERIES" 2) levels("FISHERIES") == c("at-sea whiting", "crab", "shrimp") 3) summary table...crab(year by netrev), shrimp(year by netrev)

#     print(head(tables))
#   }
# #     tables.out[[j]] <- factor.tables
#     names(tables.out)[[j]] <- names(tables.name)[[i]]

#     # This does the same as the previous step, but it computes the N's rather than the values
#     #     datN1 <- EDCtable(dat, theformula = TOPIC ~ SURVEY_YEAR, valvar = measure.var, functiontyp= "length", dataORtableORN = "data1conf")
#     #     
#     #     # the next two steps melt the data so it is nice and tidy for ggplot to use later on
#     #     dat2 <- melt(dat1, id.var= "TOPIC", value.name = measure.var, variable.name = "SURVEY_YEAR") #do this during plotting or now?
#     #     
#     #     # melt for N's tables
#     #     datN2 <- melt(datN1, id.var= "TOPIC", value.name = "N", variable.name = "SURVEY_YEAR")
#     #     
#     #     # merge the N's to the values data
#     #     dat.final <- cbind(dat2, datN2[,3])
#     #     
#     #     # change the topic name back to the var name from the input data
#     #     names(dat.final)[1] <- topic.var[i]
#     #     
#     #     # change the name of the N column to "N"
#     #     names(dat.final)[length(dat.final)] <- "N"
#     #     
#     #     
#     #     # combining the different summary tables into a list
#     #     tables[[i]] <- data.frame(dat.final)  
#     
# tables.out
# }
# #     print(topic.var[i])
#     # create a list containing the each subset netrev table as a sub-list  
#       
#     # pasting together a serialized table name. I use this to call specific tables in the Shiny app (general form is: data.year.measurevar.stat)
#     
#     
#     #     tables[[i]] <- dat.tabs
#     
#
#     #     
#     #     
#     
#   

# my toy list data.framedat1 <- EDCtable(dat, variable ~ SURVEY_YEAR, valvar = value, functiontyp = stat, dataORtableORN = "data1conf")
#tmp <- list(a=data.frame(one=runif(10), two=factor(rep(c("A", "B"),length.out=10), levels = c("A", "B"))), b=data.frame(three=runif(10), four=factor(rep(c("C","D"), length.out=10))))

