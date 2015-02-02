#======================================

# this page handles all of the reactive 
  # expressions for the dynamic user interface

#======================================


#labeled "Years"
output$YearSelect <- renderUI({
  checkboxGroupInput("YearSelect", "Years:", 
    choices = DatVars()$SURVEY_YEAR, selected="")
})


# labeled "Topic"
output$ShortdescrSelect <- renderUI({ #data selection. there are two reactives that are dependent on these names in ex.reactives
  checkboxGroupInput("ShortdescrSelect", "Revenue/Cost type:", 
    choices=DatVars()$SHORTDESCR[!is.na(DatVars()$SHORTDESCR)], 
    selected = "")
})


# labeled "Category
# these topic names are used all over the rest of the scripts, careful when you change them..
# in javascript control on ex.fluidPage, reactives in both dat.var and subsetting, plot.reactives
output$CategorySelect <- renderUI({ 
  selectInput("CategorySelect", "Summarize by:",
    choices = DatVars()$CATEGORY, selected = " ")
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
    choices = DatVars()$STAT, selected = "", multiple = FALSE)
})


#======================================

# Plot options

#======================================

output$PlotSelect <- renderUI({
    selectInput("PlotSelect", "Plot type:", choices= c("", "Bar", "Point", "Line"), selected=c(""))
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
