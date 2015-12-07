years.list <- c("2009", "2010", "2011", "2012")

# load("data/tabs.out.RData") ## this is now the following datatables
load("data/netrevTable.RData")
load("data/factorOrder.RData")
load("data/netrevThirds.RData")

# custom palettes
# for net rev figures
pal.netrev <- c("Revenue" = "#66a61e", "Fixed costs" = "#e41a1c",
  "Total cost net revenue" = "#7570b3", 
  "Variable costs" = "#d95f02", "Variable cost net revenue" = "#1b9e77")

pal.thirds <- c("#1b9e77", "#d95f02", "#7570b3")


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

