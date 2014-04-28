library(RODBC)
pacfin <- odbcConnect("pacfin", "bvanderl", "vab$pac1", believeNRows=FALSE )
ifqpub <- odbcConnect("ifq", "vanderlugtbl", "Mountain5530!", believeNRows=FALSE)

