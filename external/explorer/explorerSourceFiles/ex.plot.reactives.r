plotBase <- reactive({
  if(!is.null(dat.cast())){
  dat.plot <- dat.cast()
  plot.out <- ggplot(dat.plot, aes_string(x=byvar(), y=dat.measure.var(), fill=groupvar())) + geom_bar(stat="identity")
  plot.out
  }
})