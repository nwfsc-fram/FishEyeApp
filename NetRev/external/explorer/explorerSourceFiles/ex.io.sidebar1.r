#======================================

# this page handles all of the reactive 
# expressions for the dynamic user interface

#======================================
#observe({
#  if(is.null(input$send) || input$send==0) return(NULL)
#  from <- isolate(input$from)
#  to <- "nwfsc.fisheye@noaa.gov"
#  subject <- isolate(input$subject)
#  msg <- paste(isolate(input$from), isolate(input$message))
#  sendmail("melanie.harsch@noaa.gov", subject, msg, password="rmail")
#})

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
                                             choices = c("Revenue" = "Revenue", "Variable costs" = "Variable costs", 
                                             "Fixed costs" = "Fixed costs", "Variable Cost Net Revenue"= "Variable cost net revenue",
                                             "Total Cost Net Revenue" = "Total cost net revenue"), selected = c("Revenue", "Variable costs", 
                                                                                                                "Fixed costs", "Variable cost net revenue","Total cost net revenue")))
})
#HTML("<div> Statistic:<i class='fa fa-info fa-fw' style='font-size:12px; color:blue'></i></div>")
output$YearSelect <- renderUI({
  tags$div(class="ckbox", checkboxGroupInput( "YearSelect", "Years:",#p("Years:", # p(HTML("<div title=This is a tooltip> Years:<i class='fa fa-info-circle fa-fw' style='font-size:12px; color:blue'></i></div>"), 
                                                            #  span(tags$br(), "(The Catch Share program began in 2011)", style="font-style:italic;font-size:10.5pt; font-weight:normal;"), style="margin-bottom:-2px"), 
                                              choices = DatVars()$YEAR, selected = DatVars()$YEAR))
})

output$CategorySelect <- renderUI({
  #   tags$div(title="Hi, I am a sample hover tip",
  
  tags$div(class="ckbox", radioButtons("CategorySelect", "Group vessels according to:", 
                                       
                                       choices = DatVars()$CATEGORY))
  #   )
})

fish.var <- c("All fisheries combined"="All Fisheries"," All catch share fisheries combined"="All Catch Share Fisheries","At-sea Pacific whiting","Shoreside Pacific whiting",
              "DTS trawl with trawl endorsement","Non-whiting, non-DTS trawl with trawl endorsement",  "Groundfish fixed gear with trawl endorsement",
              "All non-catch share fisheries combined"="All Non-Catch Share Fisheries", "Groundfish fixed gear with fixed gear endorsement","Crab","Shrimp","Other fisheries")

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
                                                 "All non-catch share fisheries" = "All Non-Catch Share Fisheries")), style="margin-bottom:-10px"),
          checkboxGroupInput("VariableSelect", "Select one or more state:", choices = factorOrder$state, selected="")
        )
      } else if(input$CategorySelect == "Vessel length class"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",
                                               c("All fisheries" = "All Fisheries",
                                                 "All catch share fisheries" = "All Catch Share Fisheries",
                                                 "All non-catch share fisheries" = "All Non-Catch Share Fisheries")), style="margin-bottom:-10px"),
          checkboxGroupInput("VariableSelect",  "Select one or more vessel length class:", choices=factorOrder$lengths, selected="")
        )
      } else if(input$CategorySelect == "Homeport"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",
                                               c("All fisheries" = "All Fisheries",
                                                 "All catch share fisheries" = "All Catch Share Fisheries",
                                                 "All non-catch share fisheries" = "All Non-Catch Share Fisheries")), style="margin-bottom:-10px"),
          #$bsButton("selectall", "Select all", style="primary", size="extra-small",block=F, type="action"),
          tags$div(checkboxGroupInput("VariableSelect", div("Select one or more homeport:", style="margin-top:0; padding:-10px"), choices=factorOrder$port, selected=""))
        )
      } else if(input$CategorySelect=="Fisheries"){
        tagList(
          #              selectInput("fishCatSelect","", c("Catch share fisheries"="CSF", "Non-catch shares fisheries"="NSF", "All fisheries"="AF"), selected="AF"),
          actionButton("selectall2", "All fisheries", style="default", size="extra-small",block=F, type="action"),
          actionButton("selectallcs", "All catch share fisheries", style="default",size="extra-small", block=F, type="action"),
          actionButton("selectallncs", "All non-catch share fisheries", style="default", size="extra-small", block=F, type="action"),
          #  conditionalPanel("input.fishCatSelect==AF", 
          tags$div(class="ckbox2", checkboxGroupInput("VariableSelect", HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                                                             or select fisheries individually:  <button id='ivs' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
                                                      choices=c("All fisheries combined"="All Fisheries"," All catch share fisheries combined"="All Catch Share Fisheries",fish.var[3:12])#div(fish.var, stlye="font-style:bold")#fish.var
                                                      , selected=""))
          #tags$div(class="ckbox", checkboxGroupInput("ShortdescrSelect", HTML("<div> Economic measures:<button id='iem' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i> 
          
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
                                                 "All non-catch share fisheries" = "All Non-Catch Share Fisheries")), style="margin-bottom:-10px"),
          tags$div(class="rbutton2",  radioButtons("VariableSelect", "Select ONE state", choices = c("No state selected"="","Washington"="Washington", "Oregon"="Oregon","California"="California"), selected="")) 
        )
      } else if(input$CategorySelect == "Vessel length class"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",
                                               c("All fisheries" = "All Fisheries",
                                                 "All catch share fisheries" = "All Catch Share Fisheries",
                                                 "All non-catch share fisheries" = "All Non-Catch Share Fisheries")), style="margin-bottom:-10px"),
          tags$div(class="rbutton2", radioButtons("VariableSelect",  "Select ONE vessel length class", choices=c("No vessel length selected"="",factorOrder$lengths), selected=""))
        )
      } else if(input$CategorySelect == "Homeport"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",
                                               c("All fisheries" = "All Fisheries",
                                                 "All catch share fisheries" = "All Catch Share Fisheries",
                                                 "All non-catch share fisheries" = "All Non-Catch Share Fisheries")), style="margin-bottom:-10px"),
          tags$div(class="rbutton2", radioButtons("VariableSelect", "Select ONE homeport", choices=c("No homeport selected"="",factorOrder$port), selected=""))
        )
      } else if(input$CategorySelect=="Fisheries"){
        # tagList(
        # selectInput("fishCatSelect2","", c("Catch share fisheries"="CSF", "Non-catch share fisheries"="NSF", "All fisheries"="AF"), selected="AF"),
        # conditionalPanel(
        #    condition="input.fishCatSelect2==AF", 
        tags$div(class="rbutton", radioButtons("VariableSelect", HTML("<div> Select ONE fishery <button id='iof' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
                                               choices=c("No fishery selected"="",fish.var), selected=""))
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


#output$FisherySubsetSelect <- renderUI({
#  if(is.null(input$CategorySelect)) return()
#  if(input$CategorySelect != "Fisheries"){
#     return(em("Economic data from all fisheries are included"))# data from all fisheries
#}
# } )

output$FishAkSelect <- renderUI({
  tags$div(class="ckbox", checkboxInput("FishAkSelect", p("Include vessels that fished in Alaska: ", 
                                                          span("By selecting this, you will include vessels that also participated in Alaskan fisheries. 
                                                               Data from their activities in Alaska are not included.", style="font-style:italic;font-size:10pt")), 
                                        value = TRUE))
})

output$FishWhitingSelect <- renderUI({
  tags$div(class="ckbox", checkboxInput("FishWhitingSelect", p("Include vessels that fished for Pacific whiting: ", 
                                                               span("By selecting this, you will include vessels that also fished for Pacific whiting. 
                                                                    Data from their activities are included if this box is selected.", style="font-style:italic;font-size:10pt")), 
                                        value = TRUE))
})


output$StatSelect <- renderUI({
  tagList(
    selectInput("AVE_MED", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                c('Choose Median, Average, or Total'="", Average="A", Median="M", Total="T"), selectize=F),
    #    actionButton("MED", "Median", style="default",size="extra-small", block=F, type="action"),
    #    if(input$AVE_MED!="A"){
    tags$div(class="statbox", radioButtons("StatSelect","",  choices = c(DatVars()$STAT[4:6])))#"Statistic:",
    #    }
    #   else #if(input$AVE_MED=="M")
    #     {
    #    tags$div(class="ckbox", radioButtons("StatSelect", "", 
    #                                         choices = c(DatVars()$STAT[4:7])))
    #  }
  )
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
output$DodgeSelect <- renderUI({
  if(input$tabs!="Panel2"){
    radioButtons("DodgeSelect", HTML("<div> Plot Options: <button id='ipo' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></div>"), 
                 choices= c("Economic measures side-by-side", "Composition of Variable Cost Net Revenue","Composition of Total Cost Net Revenue"))
  } else return()
})


output$PlotSelect <- renderUI({
    if(input$DodgeSelect == "Economic measures side-by-side") {
      tags$div(class="ckbox", selectInput("PlotSelect", "", choices= c("Bar", "Point", "Line")))
    } else return()
})

#output$PlotSelect <- renderUI({
#  selectInput("PlotSelect", "Plot Options:", choices= c("Bar", "Point", "Line"))
#})


#output$DodgeSelect <- renderUI({
#    if(!is.null(input$PlotSelect)) {
#    if(input$PlotSelect == "Bar") {
#      radioButtons("DodgeSelect", "", choices= c("Composition of total cost revenue", "Composition of variable cost revenue","Economic measures side-by-side"))
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
  if(input$CategorySelect!="Fisheries"){
    HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
                                       <b>Show data summed across these fisheries: </b><button id='isummed' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></div>")
  } else  if(input$tabs!="Panel2" & input$CategorySelect=="Fisheries"){
    HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:20px; margin-bottom:-25px;'>
                                       <b>Select fisheries:</b> 
                                       <h5><i>Select a fishery group,  </i>
       <button id='ifg' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></h5></div>")#,
    # HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
    #                                      <i>Select fishery groups:</i></div>") 
    
  }
  
})

#output$SelectTextYear <- renderText({
#  HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
#                                       <i>The catch share program was implemented after 2010</i></div>")
#})
#attr(input, "readonly") <- FALSE
#input$ActionButtonMemory <- 0
#observe({
#  if(length(input$data)>0){
#    if((input$data-input$ActionButtonMemory)>0){
#      input$ActionButtonMemory<- input$data # Equalize
#    }}
#})

