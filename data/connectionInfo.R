library(RODBC)
pacfin <- odbcConnect("pacfin", "bvanderl", "vab$pac1", believeNRows=FALSE )
ifqpub <- odbcConnect("ifq", "vanderlugtbl", "#o7r*3@wTSJj", believeNRows=FALSE)

#ifq pass updated 5/21/2014