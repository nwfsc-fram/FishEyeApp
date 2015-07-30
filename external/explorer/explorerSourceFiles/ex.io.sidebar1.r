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

fish.var <- c("All Catch Share Fisheries","At-sea Pacific whiting","Shoreside Pacific whiting","DTS trawl with trawl endorsement","Non-whiting, non-DTS trawl with trawl endorsement",
  "Groundfish fixed gear with trawl endorsement","Groundfish fixed gear with fixed gear endorsement",
  "All Non-Catch Share Fisheries", "Crab","Shrimp","Other fisheries")

Variable <- reactive({
  dat <- DatMain()
  if(input$CategorySelect == "Homeport"){
    variable = factorOrder$port
  } else if(input$CategorySelect == "State"){
    variable = factorOrder$state
  } else if(input$CategorySelect == "Fisheries"){
    variable = fish.var
  } else {
    variable = factorOrder$lengths
    #       subByCategory <- dat[dat$CATEGORY == input$CategorySelect,] 
  }
  return(variable)
})


output$VariableSelect <- renderUI({
  if(!is.null(input$CategorySelect)){

      if(input$CategorySelect == "State"){
       checkboxGroupInput("VariableSelect", "State", choices = factorOrder$state, selected="")
      } else if(input$CategorySelect == "Vessel length class"){
        checkboxGroupInput("VariableSelect",  "Vessel length class", choices=factorOrder$lengths, selected="")
      } else if(input$CategorySelect == "Homeport"){
         tagList( 
        actionButton("selectall", "Select all"),
         
        checkboxGroupInput("VariableSelect", "Homeport", choices=factorOrder$port, selected="")
         )
      } else if(input$CategorySelect=="Fisheries"){
        tagList(
        selectInput("fishCatSelect","", c("Catch share fisheries"="CSF", "Non-catch shares fisheries"="NSF", "All fisheries"="AF"), selected="AF"),
        conditionalPanel(
          condition="input.fishCatSelect==AF", 
         actionButton("selectall2", "Select all")),
        checkboxGroupInput("VariableSelect", "Select one or more fishery", choices=c("All Catch Share Fisheries","At-sea Pacific whiting","Shoreside Pacific whiting","DTS trawl with trawl endorsement","Non-whiting, non-DTS trawl with trawl endorsement",
                                                                    "Groundfish fixed gear with trawl endorsement","Groundfish fixed gear with fixed gear endorsement",
                                                                    "All Non-Catch Share Fisheries", "Crab","Shrimp","Other fisheries"), selected="")
        
        )
        }
      }
        
    #   
        
#        if(input$CategorySelect == "Fisheries"){
#      checkboxGroupInput("VariableSelect", "", 
                      # choices = c("All Catch Share Fisheries","At-sea Pacific whiting","Shoreside Pacific whiting","DTS trawl with trawl endorsement","Non-whiting, non-DTS trawl with trawl endorsement",
                      #             "Groundfish fixed gear with trawl endorsement","Groundfish fixed gear with fixed gear endorsement",
                      #             "All Non-Catch Share Fisheries", "Crab","Shrimp","Other fisheries"),#
 #                     choices=Variable()#,
                       #selected = ""
#                    )
#    
#    )
#  }
   else return()
})

observe({
  if (is.null(input$fishCatSelect)) return()
    else if(input$fishCatSelect=="CSF"){
      updateCheckboxGroupInput(session,"VariableSelect", choices=fish.var[1:7], selected=NULL)
    } else  if(input$fishCatSelect=="NSF"){
      updateCheckboxGroupInput(session,"VariableSelect", choices=fish.var[8:11], selected=NULL)
     } else { 
       tagList(
      actionButton("selectall2", "Select all"),
      updateCheckboxGroupInput(session,"VariableSelect", choices=fish.var, selected=NULL)
  )
       }
#  else return ()
})

observe({
   if (is.null(input$selectall) || input$selectall == 0) return() 
  else if (input$selectall%%2 == 0) {
     updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0)) } 
  else {
    updateCheckboxGroupInput(session,"VariableSelect", selected=factorOrder$port)
  }
})

observe({
  if (is.null(input$selectall2) || input$selectall2 == 0) return() 
  else if (input$selectall2%%2 == 0) {
    updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0)) } 
  else {
    updateCheckboxGroupInput(session,"VariableSelect", selected=fish.var)
  }
})



output$FisherySubsetSelect <- renderUI({
  if(is.null(input$CategorySelect)) return()
  if(input$CategorySelect != "Fisheries"){
     return(em("All fisheries included"))# radioButtons("fisherySubsetSelect", "", choices = "Include all fisheries")
}
 } )

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

