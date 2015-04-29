#======================================

# this page handles all of the reactive 
  # expressions for the dynamic user interface

#======================================


output$YearSelect <- renderUI({
  checkboxGroupInput("YearSelect", "Years:", 
    choices = DatVars()$YEAR, selected = DatVars()$YEAR)
})


output$ShortdescrSelect <- renderUI({ 
  checkboxGroupInput("ShortdescrSelect", "Economic measures:", 
    choices = DatVars()$SHORTDESCR, selected = DatVars()$SHORTDESCR)
})


output$CategorySelect <- renderUI({
#   tags$div(title="Hi, I am a sample hover tip",
    radioButtons("CategorySelect", "Summary variable:",
      choices = DatVars()$CATEGORY)
#   )
})


output$VariableSelect <- renderUI({
  if(!is.null(input$CategorySelect)){
      checkboxGroupInput("VariableSelect", "", 
        choices = Variable(), selected = "")
  } else return()
})

output$FisherySubsetSelect <- renderUI({
  if(is.null(input$CategorySelect)) return()
  if(input$CategorySelect != "Fisheries"){
      radioButtons("fisherySubsetSelect", "", choices = "Include All fisheries")
  }
})


output$FishAkSelect <- renderUI({
  checkboxInput("FishAkSelect", "Include vessels that fished in AK", 
    value = TRUE)
})


output$StatSelect <- renderUI({
  radioButtons("StatSelect", "Summary statistic:", 
    choices = c(DatVars()$STAT))
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


# output$DataButton <- renderUI({
#   if(PermitPlot()) {
#     actionButton("DataButton", label=" Plot Data")
#   }
# })

