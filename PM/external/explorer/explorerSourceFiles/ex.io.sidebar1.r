#======================================

# this page handles all of the reactive 
  # expressions for the dynamic user interface

#======================================

observeEvent(input$reset_input, {
  if(input$tabs=="Panel2") {
  updateRadioButtons(session, "VariableSelect", selected="")  
  } else if(input$tabs=="Panel1"){
  updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0))
  }  
})

output$ShortdescrSelect <- renderUI({ 
  tags$div(class="ckbox", checkboxGroupInput("ShortdescrSelect", HTML("<div> Economic measures:<button id='iem' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i> 
                                                                      </button></div>"), 
                                             choices = DatVars()$SHORTDESCR, selected = DatVars()$SHORTDESCR))
})
#HTML("<div> Statistic:<i class='fa fa-info fa-fw' style='font-size:12px; color:blue'></i></div>")
output$YearSelect <- renderUI({
  tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:",# p("Years:", # p(HTML("<div title=This is a tooltip> Years:<i class='fa fa-info-circle fa-fw' style='font-size:12px; color:blue'></i></div>"), 
                                                              #span(tags$br(), "(The Catch Share program began in 2011)", style="font-style:italic;font-size:10.5pt; font-weight:normal;"), style="margin-bottom:-2px"), 
                                              choices = DatVars()$YEAR, selected = DatVars()$YEAR))
})

output$CategorySelect <- renderUI({
  tags$div(class="ckbox", radioButtons("CategorySelect", "Group vessels according to:", 
      choices = DatVars()$CATEGORY))
})

fish.var <- c("All fisheries combined"="All Fisheries"," All Catch Share fisheries combined"="All Catch Share Fisheries","At-sea Pacific whiting","Shoreside Pacific whiting",
              "DTS trawl with trawl endorsement","Non-whiting, non-DTS trawl with trawl endorsement",  "Groundfish fixed gear with trawl endorsement",
  "All non-Catch Share fisheries combined"="All Non-Catch Share Fisheries", "Groundfish fixed gear with fixed gear endorsement","Crab","Shrimp","Other fisheries")

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
                          "All non-catch shares fisheries" = "All Non-Catch Share Fisheries")), style="margin-bottom:-10px"),
            checkboxGroupInput("VariableSelect", "Select one or more state:", choices = factorOrder$state, selected="")
          )
        } else if(input$CategorySelect == "Vessel length class"){
          tagList(           
            tags$div(class="select", selectInput("inSelect","",
                        c("All fisheries" = "All Fisheries",
                          "All catch share fisheries" = "All Catch Share Fisheries",
                          "All non-catch shares fisheries" = "All Non-Catch Share Fisheries")), style="margin-bottom:-10px"),
            checkboxGroupInput("VariableSelect",  "Select one or more vessel length class:", choices=factorOrder$lengths, selected="")
          )
        } else if(input$CategorySelect == "Homeport"){
            tagList(           
              tags$div(class="select", selectInput("inSelect","",
                          c("All fisheries" = "All Fisheries",
                            "All catch share fisheries" = "All Catch Share Fisheries",
                            "All non-catch shares fisheries" = "All Non-Catch Share Fisheries")), style="margin-bottom:-10px"),
              tags$div(checkboxGroupInput("VariableSelect", div("Select one or more homeport:", style="margin-top:0; padding:-10px"), choices=factorOrder$port, selected=""))
            )
        } else if(input$CategorySelect=="Fisheries"){
            tagList(
                 actionButton("selectall2", "All fisheries", style="default", size="extra-small",block=F, type="action"),
                 actionButton("selectallcs", "All catch share fisheries", style="default",size="extra-small", block=F, type="action"),
                 actionButton("selectallncs", "All non-catch shares fisheries", style="default", size="extra-small", block=F, type="action"),
           tags$div(class="ckbox2", checkboxGroupInput("VariableSelect", HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                                                                 or select fisheries individually:  <button id='ivs' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
                                                       choices=c("All fisheries combined"="All Fisheries"," All catch share fisheries combined"="All Catch Share Fisheries",fish.var[3:12])#div(fish.var, stlye="font-style:bold")#fish.var
                                                       , selected=""))
            )
          } # end fisheries
      } else return ()
    
   } else if(input$tabs=="Panel2") {
      if(!is.null(input$CategorySelect)){
       
        if(input$CategorySelect == "State"){
          tagList(           
            tags$div(class="select", selectInput("inSelect","",
                                                 c("All fisheries" = "All Fisheries",
                                                   "All catch share fisheries" = "All Catch Share Fisheries",
                                                   "All non-catch shares fisheries" = "All Non-Catch Share Fisheries")), style="margin-bottom:-10px"),
            tags$div(class="rbutton2",  radioButtons("VariableSelect", "Select ONE state", choices = c("No state selected"="","Washington"="Washington", "Oregon"="Oregon","California"="California"), selected="")) 
          )
            } else if(input$CategorySelect == "Vessel length class"){
          tagList(           
            tags$div(class="select", selectInput("inSelect","",
                                                 c("All fisheries" = "All Fisheries",
                                                   "All catch share fisheries" = "All Catch Share Fisheries",
                                                   "All non-catch shares fisheries" = "All Non-Catch Share Fisheries")), style="margin-bottom:-10px"),
            tags$div(class="rbutton2", radioButtons("VariableSelect",  "Select ONE vessel length class", choices=c("No vessel length selected"="",factorOrder$lengths), selected=""))
          )
            } else if(input$CategorySelect == "Homeport"){
          tagList(           
            tags$div(class="select", selectInput("inSelect","",
                                                 c("All fisheries" = "All Fisheries",
                                                   "All catch share fisheries" = "All Catch Share Fisheries",
                                                   "All non-catch shares fisheries" = "All Non-Catch Share Fisheries")), style="margin-bottom:-10px"),
            tags$div(class="rbutton2", radioButtons("VariableSelect", "Select ONE homeport", choices=c("No homeport selected"="",factorOrder$port), selected=""))
          )
            } else if(input$CategorySelect=="Fisheries"){
              tags$div(class="rbutton", radioButtons("VariableSelect", HTML("<div> Select ONE fishery <button id='iof' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
                                                     choices=c("No fishery selected"="",fish.var), selected=""))
            }#end fisheries
          } #else return ()
   } # end Panel 2
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
    updateCheckboxGroupInput(session,"VariableSelect", selected=fish.var)  }
})

observe({
  if (is.null(input$selectallcs) || input$selectallcs == 0) return() 
  else if (input$selectallcs%%2 == 0) {
    updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0)) } 
  else {
    updateCheckboxGroupInput(session,"VariableSelect", selected=fish.var[2:7])
  }
})

observe({
  if (is.null(input$selectallncs) || input$selectallncs == 0) return() 
  else if (input$selectallncs%%2 == 0) {
    updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0)) } 
  else {
    updateCheckboxGroupInput(session,"VariableSelect", selected=fish.var[8:12])
  }
})


output$FishAkSelect <- renderUI({
  tags$div(class="ckbox", checkboxInput("FishAkSelect", p("Include vessels that fished in Alaska: ", 
                                                          span("By selecting this, you will include vessels that also participated in Alaskan fisheries. Data from their activities in Alaska are not included.", style="font-style:italic;font-size:10pt")), value = TRUE))
})

output$FishWhitingSelect <- renderUI({
  tags$div(class="ckbox", checkboxInput("FishWhitingSelect", p("Include vessels that fished for Pacific whiting: ", 
                                                               span("By selecting this, you will include vessels that also fished for Pacific whiting. Data from their activities are included if this box is selected.", style="font-style:italic;font-size:10pt")), value = TRUE))
})

output$IndicatorSelect <- renderUI({
  tagList(
    selectInput("Ind_sel", "Select an indicator category:", c('Demographic'="Demographic", 'Economic'="Economic", 'Regional'="Regional"), selectize=T),
    tags$div(class="statbox", radioButtons("MetricSelect","", choices=c(DatVars()$METRIC[1:8])))
  )
})

observe({
  if (is.null(input$Ind_sel)) return()
 # if (input$Ind_sel=="Economic") return()
 else if(input$Ind_sel=="Economic") {
    updateRadioButtons(session, "MetricSelect", choices ="Select an economic measure and statistic below")
  }
  else  if(input$Ind_sel=="Demographic"){
    updateRadioButtons(session, "MetricSelect", choices = c(DatVars()$METRIC[1:8]))
  } else  if(input$Ind_sel=="Regional"){
    updateRadioButtons(session, "MetricSelect", choices = c(DatVars()$METRIC[9:11]))
  } 
})

output$SectorSelect <- renderUI({
  tags$div(class="sectselect", selectInput("Sect_sel", span("West Coast Trawl Catch Share Program:", style="font-size:110%;font-style:italic; padding:5px; display:inline-block;vertical-align:middle"), c('Catcher Vessels'="CV", 'Mothership Vessels'="M", 'Catcher Processor Vessels'="CP", 'First Receivers and Shorebased Processors'="FR"), width='35%')
)
  })


output$VesSumSelect <- renderUI({
  if(PermitPlot()){
  if(input$VariableSelect!="All Fisheries"&input$VariableSelect!="All Catch Share Fisheries"&input$VariableSelect!="All Non-Catch Share Fisheries") {
 
  tagList(
    tags$div(class="ckbox", radioButtons("VesSum", HTML("<div> Show data summed: <button id='iVesSum' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                                           choices=c("within selected fisheries"="within","across all Catch Share fisheries"="acrossCS","across all West Coast fisheries"="acrossWC")))
  )} else {return()}
    } else {return()}
})
#span("For all vessels that fished within selected fisheries, show data for activities:", style="font-size:11pt; font-weight:bold;"), #font-style:italic;

output$StatSelect <- renderUI({
  tagList(
    selectInput("AVE_MED", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                c('Median, Average, or Total values'="", Average="A", Median="M", Total="T"), selectize=F),
    tags$div(class="statbox", radioButtons("StatSelect","",  choices = c(DatVars()$STAT[4:6]))))
})

observe({
  if (is.null(input$AVE_MED)) return()
 else  if(input$AVE_MED=="M"){
    updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[4:6]))
  }  else if(input$AVE_MED=="A"){
    updateRadioButtons(session,"StatSelect",   choices = c(DatVars()$STAT[1:3]))
  } else  if(input$AVE_MED=="T"){
    updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[7]))
  } 
})

#======================================

# Plot options

#======================================
#output$DodgeSelect <- renderUI({
#  if(input$tabs!="Panel2"){
#    radioButtons("DodgeSelect", HTML("<div> Plot Options: <button id='ipo' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></div>"), 
#                 choices= c("Economic measures side-by-side", "Composition of variable cost net revenue","Composition of total cost net revenue"))
#  } else return()
#})


output$PlotSelect <- renderUI({
  if(input$tabs!="Panel2"){
#    if(input$DodgeSelect == "Economic measures side-by-side") {
      tags$div(class="ckbox", selectInput("PlotSelect", "Plot options:", choices= c("Bar", "Point", "Line")))
#    } else return()
  } else return()
})


#===============text ==========================================#
output$SelectText <- renderText ({ 
  if(input$CategorySelect!="Fisheries"){
    HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
         <b>Show data summed across these fisheries: </b><button id='isummed' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></div>")
  } else  if(input$tabs!="Panel2" & input$CategorySelect=="Fisheries"){
    HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:20px; margin-bottom:-25px;'>
         <b>Select fisheries:</b> 
         <h5><i>Select a fishery group,  </i>
         <button id='ifg' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></h5></div>")#,
   }
})

#attr(input, "readonly") <- FALSE
#input$ActionButtonMemory <- 0
#observe({
#  if(length(input$data)>0){
#    if((input$data-input$ActionButtonMemory)>0){
#      input$ActionButtonMemory<- input$data # Equalize
#    }}
#})
