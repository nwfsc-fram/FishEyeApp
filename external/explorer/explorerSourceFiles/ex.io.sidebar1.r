#======================================

# this page handles all of the reactive 
  # expressions for the dynamic user interface

#======================================
observe({
  if(is.null(input$send) || input$send==0) return(NULL)
  from <- isolate(input$from)
  to <- isolate(input$to)
  subject <- isolate(input$subject)
  msg <- paste(isolate(input$from), isolate(input$message))
  sendmail(to, subject, msg, password="rmail")
})



output$ShortdescrSelect <- renderUI({ 
  checkboxGroupInput("ShortdescrSelect", "Economic measures:", 
    choices = DatVars()$SHORTDESCR, selected = DatVars()$SHORTDESCR)
})

output$YearSelect <- renderUI({
  checkboxGroupInput("YearSelect", "Years (note that the Catch Share Program began in 2011):", 
    choices = DatVars()$YEAR, selected = DatVars()$YEAR)
})

output$CategorySelect <- renderUI({
#   tags$div(title="Hi, I am a sample hover tip",

    radioButtons("CategorySelect", "Group vessels according to:", #"Summarize variable by:",
      
      choices = DatVars()$CATEGORY)
#   )
})

fish.var <- c("The aggregate of all catch share fisheries"="All Catch Share Fisheries","At-sea Pacific whiting","Shoreside Pacific whiting","DTS trawl with trawl endorsement","Non-whiting, non-DTS trawl with trawl endorsement",
  "Groundfish fixed gear with trawl endorsement",
  "The aggregate of all non-catch share fisheries"="All Non-Catch Share Fisheries", "Groundfish fixed gear with fixed gear endorsement","Crab","Shrimp","Other fisheries")

Variable <- reactive({
  dat <- DatMain()
  if(input$CategorySelect == "Fisheries"){
    variable = fish.var
  } else if(input$CategorySelect == "Homeport"){
    variable = factorOrder$port
  } else if(input$CategorySelect == "State"){
    variable = factorOrder$state
  } else {
    variable = factorOrder$lengths
    #       subByCategory <- dat[dat$CATEGORY == input$CategorySelect,] 
  }
  return(variable)
})


    

output$VariableSelect <- renderUI({  
    if(input$tabs=="Panel1"){
      if(!is.null(input$CategorySelect)){
        if(input$CategorySelect == "State"){
          tagList(           
            tags$div(class="select", selectInput("inSelect","",
                        c("All fisheries" = "All Fisheries",
                          "All catch share fisheries" = "All Catch Share Fisheries",
                          "All non-catch shares fisheries" = "All Non-Catch Share Fisheries"))),
            checkboxGroupInput("VariableSelect", "Select one or more:", choices = factorOrder$state, selected="")
          )
        } else if(input$CategorySelect == "Vessel length class"){
          tagList(           
            tags$div(class="select", selectInput("inSelect","",
                        c("All fisheries" = "All Fisheries",
                          "All catch share fisheries" = "All Catch Share Fisheries",
                          "All non-catch shares fisheries" = "All Non-Catch Share Fisheries"))),
            checkboxGroupInput("VariableSelect",  "Select one or more:", choices=factorOrder$lengths, selected="")
          )
        } else if(input$CategorySelect == "Homeport"){
            tagList(           
              tags$div(class="select", selectInput("inSelect","",
                          c("All fisheries" = "All Fisheries",
                            "All catch share fisheries" = "All Catch Share Fisheries",
                            "All non-catch shares fisheries" = "All Non-Catch Share Fisheries"))),
              #$bsButton("selectall", "Select all", style="primary", size="extra-small",block=F, type="action"),
              checkboxGroupInput("VariableSelect", "Select one or more:", choices=factorOrder$port, selected="")
            )
        } else if(input$CategorySelect=="Fisheries"){
            tagList(
#              selectInput("fishCatSelect","", c("Catch share fisheries"="CSF", "Non-catch shares fisheries"="NSF", "All fisheries"="AF"), selected="AF"),
                 bsButton("selectall2", "All fisheries", style="default", size="extra-small",block=F, type="action"),
                 bsButton("selectallcs", "All catch share fisheries", style="default",size="extra-small", block=F, type="action"),
                 bsButton("selectallncs", "All non-catch shares fisheries", style="default", size="extra-small", block=F, type="action"),
           #  conditionalPanel("input.fishCatSelect==AF", 
              checkboxGroupInput("VariableSelect", tags$h5(tags$i("Select fishery individually:")), choices=fish.var, selected="")
              
            )
          } # end fisheries
      } else return ()
    
   } else if(input$tabs=="Panel2") {
      if(!is.null(input$CategorySelect)){
       
        if(input$CategorySelect == "State"){
           radioButtons("VariableSelect", "Select ONE state", choices = c("None selected"="","Washington"="Washington", "Oregon"="Oregon","California"="California"), selected="None selected") 
        } else if(input$CategorySelect == "Vessel length class"){
           radioButtons("VariableSelect",  "Select ONE vessel length class", choices=c("None selected"="",factorOrder$lengths), selected="None selected")
        } else if(input$CategorySelect == "Homeport"){
            radioButtons("VariableSelect", "Select ONE homeport", choices=c("None selected"="",factorOrder$port), selected="None selected")
        } else if(input$CategorySelect=="Fisheries"){
           # tagList(
              # selectInput("fishCatSelect2","", c("Catch share fisheries"="CSF", "Non-catch share fisheries"="NSF", "All fisheries"="AF"), selected="AF"),
              # conditionalPanel(
             #    condition="input.fishCatSelect2==AF", 
                 radioButtons("VariableSelect", "Select ONE fishery", choices=c("None selected"="",fish.var), selected="None selected")#)
                   #        )            
            }#end fisheries
          } #else return ()
   } # end Panel 2
  })
   

#observe({
#  if (is.null(input$fishCatSelect)) return()
#    else if(input$fishCatSelect=="CSF"){
#      updateCheckboxGroupInput(session,"VariableSelect", choices=fish.var[1:6], selected="")
#    } else  if(input$fishCatSelect=="NSF"){
#      updateCheckboxGroupInput(session,"VariableSelect", choices=fish.var[7:11], selected="")
#     } else { 
#       tagList(
#         actionButton("selectall2", "Select all"),
#      updateCheckboxGroupInput(session,"VariableSelect", choices=fish.var, selected="")
#  )
#       }
#  else return ()
#})


#observe({
#  if (is.null(input$fishCatSelect2)) return()
#  else if(input$fishCatSelect2=="CSF"){
#    updateRadioButtons(session,"VariableSelect",  choices=c("None selected"="",fish.var[1:6]), selected="None selected")
#  } else  if(input$fishCatSelect2=="NSF"){
#    updateRadioButtons(session,"VariableSelect", choices=c("None selected"="",fish.var[7:11]), selected="None selected")
#  } else { 
#    updateRadioButtons(session,"VariableSelect",  choices=c("None selected"="",fish.var), selected="None selected")
#  }
  #  else return ()
#})


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

observe({
  if (is.null(input$selectallcs) || input$selectallcs == 0) return() 
  else if (input$selectallcs%%2 == 0) {
    updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0)) } 
  else {
    updateCheckboxGroupInput(session,"VariableSelect", selected=fish.var[1:6])
  }
})

observe({
  if (is.null(input$selectallncs) || input$selectallncs == 0) return() 
  else if (input$selectallncs%%2 == 0) {
    updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0)) } 
  else {
    updateCheckboxGroupInput(session,"VariableSelect", selected=fish.var[7:11])
  }
})


#output$FisherySubsetSelect <- renderUI({
#  if(is.null(input$CategorySelect)) return()
#  if(input$CategorySelect != "Fisheries"){
#     return(em("Economic data from all fisheries are included"))# data from all fisheries
#}
# } )

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
output$DodgeSelect <- renderUI({
  if(input$tabs!="Panel2"){
  radioButtons("DodgeSelect", "Plot Options:", 
               choices= c("Compare economic measures side-by-side","Derivation of total cost net revenue", "Derivation of variable cost net revenue"))
} else return()
  })


output$PlotSelect <- renderUI({
  if(input$tabs!="Panel2"){
    if(input$DodgeSelect == "Compare economic measures side-by-side") {
  selectInput("PlotSelect", "", choices= c("Bar", "Point", "Line"))
    } else return()
  } else return()
})


#output$PlotSelect <- renderUI({
#  selectInput("PlotSelect", "Plot Options:", choices= c("Bar", "Point", "Line"))
#})


#output$DodgeSelect <- renderUI({
#    if(!is.null(input$PlotSelect)) {
#    if(input$PlotSelect == "Bar") {
#      radioButtons("DodgeSelect", "", choices= c("Derivation of total cost revenue", "Derivation of variable cost revenue","Compare economic measures side-by-side"))
#    } else return()
#  } else return()
#})


#==============Data subsetting action button ==================#


# output$DataButton <- renderUI({
#   if(PermitPlot()) {
#     actionButton("DataButton", label=" Plot Data")
#   }
# })

#===============text ==========================================#
output$SelectText <- renderText ({ 
  if(input$tabs!="Panel2"){
  if(input$CategorySelect!="Fisheries"){
HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
                                       <b>Show data summed across these fisheries:</b></div>")
} else  if(input$CategorySelect=="Fisheries"){
  HTML("<div style='display:inline-block;width:100%; margin-top:11px'>
                                       <b>Select data for these fisheries:</b> <br>
                                       <h5><i>Select fishery groups:</i></h5></div>")#,
 # HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
 #                                      <i>Select fishery groups:</i></div>") 

}
  } else {
    return()
  }
})

#output$SelectTextYear <- renderText({
#  HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
#                                       <i>The catch shares program was implemented after 2010</i></div>")
#})

