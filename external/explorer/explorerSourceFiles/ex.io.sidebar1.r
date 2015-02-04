#======================================

# this page handles all of the reactive 
  # expressions for the dynamic user interface

#======================================


output$YearSelect <- renderUI({
  checkboxGroupInput("YearSelect", "Years:", 
    choices = DatVars()$SURVEY_YEAR, selected="")
})


output$ShortdescrSelect <- renderUI({ 
  checkboxGroupInput("ShortdescrSelect", "Revenue/Cost type:", 
    choices=DatVars()$SHORTDESCR, 
    selected = DatVars()$SHORTDESCR)
})


output$CategorySelect <- renderUI({ 
  selectInput("CategorySelect", "Summarize by:",
    choices = DatVars()$CATEGORY, selected = "")
})


output$VariableSelect <- renderUI({
  if(!is.null(input$CategorySelect)){
      checkboxGroupInput("VariableSelect", "", 
        choices = Variable(), selected = "")
  } else return()
})


output$FishAkSelect <- renderUI({
  checkboxInput("FishAkSelect", "Include vessels that fished in AK", 
    value = TRUE)
})


output$StatSelect <- renderUI({
  selectInput("StatSelect", "Summary statistc:", 
    choices = DatVars()$STAT, multiple = FALSE)
})


#======================================

# Plot options

#======================================


output$PlotSelect <- renderUI({
  selectInput("PlotSelect", "Plot type:", choices= c("Bar", "Point", "Line"))
})


output$DodgeSelect <- renderUI({
  if(!is.null(input$PlotSelect)) {
    if(input$PlotSelect == "Bar") {
      radioButtons("DodgeSelect", "", choices= c("Grouped position", "Stacked position"))
    } else return()
  } else return()
})


#==============Data subsetting action button ==================#


output$DataButton <- renderUI({
  if(PermitPlot()) {
    actionButton("DataButton", label=" Plot Data", icon=icon("bar-chart-o"))
  }
})

