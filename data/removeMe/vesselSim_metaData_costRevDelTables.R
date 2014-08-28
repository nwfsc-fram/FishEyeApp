

###############################################################

# make a function to create lots of tables
# works for rev, cost and delivery data, or any other datset that uses a single "measure.var". This does not apply to net rev (measure.var = varcostnetrev, totalnetrev, rev, fixed, ect)

###############################################################


costRevDelTables <- function(data, measure.var, topic = c("FISHERIES", "VSSLNGCLASS", "HOMEPT", "STATE", "DELIVERYPT", "COSTTYPCAT", "DELIVERYST"), stat = "mean"){
  
  #create an error for user entered measure.vars not in the input dataset
  if (!all(measure.var %in% levels(data$variable))) stop("measure.var not in data")
  
  # create some empty lists to be filled with tables and table names
  tables <- list()
  table.name <- list()
  
  # limit topic variables to those included in data
  topic.var <- topic[topic %in% names(data)]
  
  for (i in 1:length(topic.var)){
    # fist cast aggregates topic var to the observation level (Vessel level)
    dat <- dcast(data, SURVEY_YEAR + get(topic.var[i]) + VESSEL_ID ~ variable, fun.aggregate = sum, subset = .(variable == measure.var), na.rm=T)  
    
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
    
    names(datNoAK)[grep("get", names(datNoAK))] <- "TOPIC"
    
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