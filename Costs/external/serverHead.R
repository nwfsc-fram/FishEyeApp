
load("data/factorOrder.RData")
load("data/CVcosts.RData")
load("data/FRcosts.RData")
load("data/CPcosts.RData")
load("data/MScosts.RData")


#======================function to help create table with date===================================#
rbindCommonCols<-function(x, y){
  
  commonColNames = intersect(colnames(x), colnames(y))
  x = x[,commonColNames]
  y = y[,commonColNames]
  
  colClassesX = sapply(x, class)
  colClassesY = sapply(y, class)
  classMatch = paste( colClassesX, colClassesY, sep = "-" )
  factorColIdx = grep("factor", classMatch)
  
  for(n in factorColIdx){ 
    x[,n] = as.factor(x[,n])
    y[,n] = as.factor(y[,n])
  }
  
  for(n in factorColIdx){ 
    x[,n] = factor(x[,n], levels = unique(c( levels(x[,n]), levels(y[,n]) )))
    y[,n] = factor(y[,n], levels = unique(c( levels(y[,n]), levels(x[,n]) )))  
  } 
  
  res = rbind(x,y)
  res
}

