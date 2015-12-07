listoffisheries <- factor(c(
  'At-sea Pacific whiting',
  'Shoreside Pacific whiting',
  'DTS trawl with trawl endorsement',
  'Non-whiting, non-DTS trawl with trawl endorsement',
  'Groundfish fixed gear with trawl endorsement',
  'Groundfish fixed gear with fixed gear endorsement',
  'Crab',
  'Shrimp',
  'Other fisheries'), ordered = T)

#csfisheries <- listoffisheries[grepl('trawl', listoffisheries) | grepl('whiting', listoffisheries)]

# This is the function parred down to just what is needed and returning fishery
NRfn.mod <- function(data, defl, listoffisheries = listoffisheries, sumORmean = 'sum', scale = 'thousands', 
                 onlyCSvessels = F, finalyear = NULL) { # note that scale only corresponds to the table, not the raw data output
  
  defl <- subset(defl, select = c('YEAR', paste0('DEFL', finalyear))) 
  names(defl) <- gsub(finalyear, "", names(defl)) 
  
  years <- data.frame(year = seq(2009, finalyear))
  
  # pulls the data for the fisheries of interest
  
  if(is.null(finalyear)) finalyear <- max(data$YEAR)
  rawdataNR.int <- data[data$FEWERFISHERIES %in% listoffisheries,]
  rawdataNR <- rawdataNR.int
  rawdataNR <- rawdataNR[rawdataNR$YEAR <= finalyear,]
  
  # summarizes over the fishery
  
  aggdataNR <- ddply(rawdataNR, .(FEWERFISHERIES, YEAR, VESSEL_ID), plyr::summarise,
                     REV = sum(REV, na.rm = T),
                     VARCOSTS = sum(VARCOSTS, na.rm = T),
                     FXCOSTS = sum(FXCOSTS, na.rm = T))
  
  
  # calculating var and tot net rev
  
  aggdataNR$varnetrev <- aggdataNR$REV - aggdataNR$VARCOSTS
  aggdataNR$totalnetrev <- aggdataNR$varnetrev - aggdataNR$FXCOSTS
  
  #===============================================================================  
  # generating the table
  #===============================================================================
  
  # reshaping the data
  mdataNR <- melt(aggdataNR, c('YEAR', 'VESSEL_ID',"FEWERFISHERIES"))
  # putting the parentheses and bolding in for the table
  mdataNR$shortdescr <- with(mdataNR, ifelse(variable == 'REV', 'Revenue', 
                                             ifelse(variable == 'FXCOSTS', '(Fixed costs)', 
                                                    ifelse(variable == 'varnetrev', FormatArow('Variable cost net revenue', typ = 'bold'), 
                                                           ifelse(variable == 'totalnetrev', FormatArow('Total cost net revenue', typ = 'bold'), 
                                                                  ifelse(variable == 'VARCOSTS', '(Variable costs)', ''))))))
  
  mdataNR$shortdescr <- factor(mdataNR$shortdescr, 
                               levels = c('Revenue', '(Variable costs)', FormatArow('Variable cost net revenue', typ = 'bold'),
                                          '(Fixed costs)', FormatArow('Total cost net revenue', typ = 'bold')))
  mdataNR <- mdataNR %>%
    transform(shortdescr = gsub("[(]", "", gsub("[)]", "", gsub("[}]", "", gsub("\\\\textbf[{]", "", shortdescr)))))
  
 # return(mdataNR)
}

