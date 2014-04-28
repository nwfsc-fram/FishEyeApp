dat <- reactive({
  if(input$dat.name == "Catcher Vessel Cost Data"){
    dat <- sqlQuery(ifqpub, "select * from  steinerer.COSTS") 
  }else dat <- NULL
  dat
})

#staging plots and subsetting
dataGo <- reactive({
  if(!is.null(input$dat.name) & !is.null(input$fishery)){
    x <- TRUE
  } else x <- FALSE
  x
})