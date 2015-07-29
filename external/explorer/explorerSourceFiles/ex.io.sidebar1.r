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

    radioButtons("CategorySelect", "Summarize variable by:",
      
      choices = DatVars()$CATEGORY)
#   )
})



output$VariableSelect <- renderUI({
  if(!is.null(input$CategorySelect)){
    checkboxGroupInput("VariableSelect", "", 
                      # choices = c("All Catch Share Fisheries","At-sea Pacific whiting","Shoreside Pacific whiting","DTS trawl with trawl endorsement","Non-whiting, non-DTS trawl with trawl endorsement",
                      #             "Groundfish fixed gear with trawl endorsement","Groundfish fixed gear with fixed gear endorsement",
                      #             "All Non-Catch Share Fisheries", "Crab","Shrimp","Other fisheries"),#
                      choices=Variable(),
                       selected = "")
  } else return()
})

output$FisherySubsetSelect <- renderUI({
  if(is.null(input$CategorySelect)) return()
  if(input$CategorySelect != "Fisheries"){
     return(em("All fisheries included"))# radioButtons("fisherySubsetSelect", "", choices = "Include all fisheries")
  }
})


output$FishAkSelect <- renderUI({
  checkboxInput("FishAkSelect", "Include vessels that fished in AK", 
    value = TRUE)
})


output$StatSelect <- renderUI({
  radioButtons("StatSelect", "Statistic:", 
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
      radioButtons("DodgeSelect", "", choices= c("Compare economic measures side-by-side", "Total cost revenue figure","Variable cost revenue figure"))
    } else return()
  } else return()
})


#==============Data subsetting action button ==================#


# output$DataButton <- renderUI({
#   if(PermitPlot()) {
#     actionButton("DataButton", label=" Plot Data")
#   }
# })

