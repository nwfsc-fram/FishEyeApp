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
  tags$div(class="ckbox", checkboxGroupInput("ShortdescrSelect", "Economic measures:", 
    choices = DatVars()$SHORTDESCR, selected = DatVars()$SHORTDESCR))
})

output$YearSelect <- renderUI({
  tags$div(class="ckbox", radioButtons( "YearSelect", p("Years:", span(tags$br(), "(The Catch Share program began in 2011)", style="font-style:italic;font-size:10.5pt; font-weight:normal;"), style="margin-bottom:-2px"), 
    choices=c("Show averages for pre- and post- catch shares"="years_ave", "Show individual years"="years_ind"), selected="years_ind"),
  checkboxGroupInput("", "", choices = DatVars()$YEAR, selected = DatVars()$YEAR))
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
              #$bsButton("selectall", "Select all", style="primary", size="extra-small",block=F, type="action"),
              tags$div(checkboxGroupInput("VariableSelect", div("Select one or more homeport:", style="margin-top:0; padding:-10px"), choices=factorOrder$port, selected=""))
            )
        } else if(input$CategorySelect=="Fisheries"){
            tagList(
#              selectInput("fishCatSelect","", c("Catch share fisheries"="CSF", "Non-catch shares fisheries"="NSF", "All fisheries"="AF"), selected="AF"),
                 actionButton("selectall2", "All fisheries", style="default", size="extra-small",block=F, type="action"),
                 actionButton("selectallcs", "All catch share fisheries", style="default",size="extra-small", block=F, type="action"),
                 actionButton("selectallncs", "All non-catch shares fisheries", style="default", size="extra-small", block=F, type="action"),
           #  conditionalPanel("input.fishCatSelect==AF", 
              tags$div(class="ckbox2", checkboxGroupInput("VariableSelect", div("or select fisheries individually:", style="font-style:italic; font-size:10.87pt; font-weight:normal; margin-bottom:-7.5pt"), 
                                                         choices=c("All fisheries combined"="All Fisheries"," All catch share fisheries combined"="All Catch Share Fisheries",fish.var[3:7],fish.var[8],fish.var[9:12])#div(fish.var, stlye="font-style:bold")#fish.var
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
           # tagList(
              # selectInput("fishCatSelect2","", c("Catch share fisheries"="CSF", "Non-catch share fisheries"="NSF", "All fisheries"="AF"), selected="AF"),
              # conditionalPanel(
             #    condition="input.fishCatSelect2==AF", 
                 tags$div(class="rbutton", radioButtons("VariableSelect", "Select ONE fishery", choices=c("No fishery selected"="",fish.var), selected=""))
                   #        )            
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
  tags$div(class="ckbox", checkboxInput("FishAkSelect", p("Include vessels that fished for whiting", 
                                                          span("By selecting this, you will include activities from  whiting fisheries. ", style="font-style:italic;font-size:10pt"))), 
                         checkboxInput("FishAkSelect", p("Include vessels that fished in AK", 
                                                          span("By selecting this, you will include vessels that also participated in Alaskan fisheries. 
                                                               Data from their activities in Alaska are not included.", style="font-style:italic;font-size:10pt"))), 
    value = TRUE)
})


output$IndicatorSelect <- renderUI({
  tagList(
    selectInput("Ind_sel", "Select an indicator category", c('Demographic'="Demographic", 'Economic'="Economic", 'Regional'="Regional"), selectize=T),
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
  selectInput("Sect_sel", "", c('Catcher Vessel'="CV", 'Mothership vessels'="M", 'Catcher Processor vessels'="CP", 'First Receivers and Shorebased processors'="FR"), selectize=T)
})


output$VesSumSelect <- renderUI({
  tagList(
    tags$div(class="ckbox", radioButtons("VesSum",span("For all vessels that fished within selected fisheries, show data for activities:", style="font-size:11pt; font-weight:bold;"), #font-style:italic;
                                           choices=c("only within selected fisheries"="within","across all catch share fisheries"="acrossCS","across all West Coast fisheries"="acrossWC")))
  )
})

output$StatSelect <- renderUI({
  tagList(
    selectInput("AVE_MED", "Statistic:", c('Median, Average, or Total values'="", Average="A", Median="M", Total="T"), selectize=F),
      tags$div(class="statbox", radioButtons("StatSelect","",  choices = c(DatVars()$STAT[4:6])))#"Statistic:",
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
  radioButtons("DodgeSelect", "Plot Options:", 
               choices= c("Economic measures side-by-side", "Composition of variable cost net revenue","Composition of total cost net revenue"))
} else return()
  })


output$PlotSelect <- renderUI({
  if(input$tabs!="Panel2"){
    if(input$DodgeSelect == "Economic measures side-by-side") {
      tags$div(class="ckbox", selectInput("PlotSelect", "", choices= c("Bar", "Point", "Line")))
    } else return()
  } else return()
})


#===============text ==========================================#
output$SelectText <- renderText ({ 
  if(input$CategorySelect!="Fisheries"){
HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
                                       <b>Show data summed across these fisheries:</b></div>")
} else  if(input$tabs!="Panel2" & input$CategorySelect=="Fisheries"){
  HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:20px; margin-bottom:-25px;'>
                                       <b>Select fisheries:</b> 
                                       <h5><i>Select a fishery group,</i></h5></div>")#,

}
  })