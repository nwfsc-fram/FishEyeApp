#======================================

# this page handles all of the reactive 
  # expressions for the dynamic user interface

#======================================
output$LayoutSelect <- renderUI({
  if(input$Sect_sel=='CV'){
  radioButtons("LayoutSelect", "Compare:", choices = c('Catcher Vessels','Metrics'), inline=T)
  } else if(input$Sect_sel=='M'){
    radioButtons("LayoutSelect", "Compare:", choices = c('Motherships','Metrics'), inline=T)
  }else if(input$Sect_sel=='CP'){
    radioButtons("LayoutSelect", "Compare:", choices = c('Vessels','Metrics'), inline=T)
  }else if(input$Sect_sel=='FR'){
    radioButtons("LayoutSelect", "Compare:", choices = c('Vessels','Metrics'), inline=T)
  }
})


  output$ShortdescrSelect <- renderUI({ 
  if(input$LayoutSelect=='Metrics'){
  tags$div(class="ckbox", checkboxGroupInput("ShortdescrSelect", HTML("<div> Economic measures:<button id='iem' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i> 
                                                                      </button></div>"), 
                                             choices = DatVars()$SHORTDESCR, selected = DatVars()$SHORTDESCR))
  } else {
    tags$div(class="ckbox", radioButtons("ShortdescrSelect", HTML("<div> Economic measures:<button id='iem' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i> 
                                                                      </button></div>"), 
                                               choices = DatVars()$SHORTDESCR, selected =''))
  }
})


output$moreOptions <- renderUI({
  tags$div(class="ckbox", checkboxInput("moreOptions","Click to select additional years", value = FALSE))
})
output$YearSelect <- renderUI({
  if(input$Ind_sel!='Economic'){
  if(input$MetricSelect=='Gini coefficient'|input$MetricSelect=='Herfindahl-Hirschman Index'|input$MetricSelect=='Number of vessels'| input$MetricSelect=='Vessel length'|input$MetricSelect=='Seasonality'|
         input$MetricSelect=='Share of landings by state'){
     if(input$moreOptions=="FALSE"){
        tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:",choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
        } else{
        tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR, selected = DatVars()$YEAR[6:11], inline=T))
        }
  }   else {
    tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
  } }
   else  if(input$Ind_sel=='Economic'){
     if(input$ShortdescrSelect[1]!="Revenue"){
        tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
  } 
   else if(input$moreOptions=="FALSE"){
        tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:",choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
      } else{
        tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR, selected = DatVars()$YEAR[6:11], inline=T))
      }
    }
    
   
})

fish.var <- c("All fisheries combined"="All fisheries"," All Catch Share fisheries combined"="All Catch Share fisheries","At-sea Pacific whiting","Shoreside Pacific whiting",
              "DTS trawl with trawl endorsement","Non-whiting midwater trawl","Non-whiting, non-DTS trawl with trawl endorsement",  "Groundfish fixed gear with trawl endorsement",
  "All non-Catch Share fisheries combined"="All non-Catch Share fisheries", "Groundfish fixed gear with fixed gear endorsement","Crab","Shrimp","Other fisheries")

output$CategorySelect <- renderUI({
  tags$div(class="ckbox", radioButtons("CategorySelect", "Group vessels according to:", 
      choices = DatVars()$CATEGORY))
})

output$IndicatorSelect <- renderUI({
  tagList(
    selectInput("Ind_sel", HTML("<div> Select an indicator category: <button id='ipo' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), c('Demographic',"Economic","Social and Regional"), selectize=T),
    if(input$Sect_sel=="CV"){
      tags$div(class="statbox", radioButtons("MetricSelect","", choices=c(DatVars()$METRIC[1:8])))
      } else {
         tags$div(class="statbox", radioButtons("MetricSelect","", choices=c(DatVars()$METRIC[1:6])))
      }
  )
})


output$VariableSelect <- renderUI({  
  if(input$Sect_sel=="M"){
    tags$div(class="ckbox2", checkboxGroupInput("VariableSelect","",choices=c("At-sea Pacific whiting")), selected="")
  } else {
  if(!is.null(input$CategorySelect)){  
    if(input$LayoutSelect=="Metrics"){
    if(input$CategorySelect == "State"){
      tagList(           
        tags$div(class="select", selectInput("inSelect","",c("All fisheries","All Catch Share fisheries","All non-Catch Share fisheries")), style="margin-bottom:-10px"),
        tags$div(class="rbutton2",  radioButtons("VariableSelect", "Select ONE state", choices = c("No state selected"="","Washington"="Washington", "Oregon"="Oregon","California"="California"), selected="")) 
      )} else if(input$CategorySelect == "Vessel length class"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",c("All fisheries","All Catch Share fisheries","All non-Catch Share fisheries")), style="margin-bottom:-10px"),
          tags$div(class="rbutton2", radioButtons("VariableSelect",  "Select ONE vessel length class", choices=c("No vessel length selected"="",factorOrder$lengths), selected=""))
        )} else if(input$CategorySelect == "Homeport"){
          tagList(           
            tags$div(class="select", selectInput("inSelect","", c("All fisheries","All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
            tags$div(class="rbutton2", radioButtons("VariableSelect", "Select ONE homeport", choices=c("No homeport selected"="",factorOrder$port), selected=""))
          )} else if(input$CategorySelect=="Fisheries"){
            tags$div(class="rbutton", radioButtons("VariableSelect", HTML("<div> Select ONE fishery <button id='iof' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
                                                   choices=c("No fishery selected"="",fish.var), selected=""))}
  } else {
      if(input$CategorySelect == "State"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",c("All fisheries",  "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
          checkboxGroupInput("VariableSelect", "Select one or more state:", choices = factorOrder$state, selected="")
        ) } else if(input$CategorySelect == "Vessel length class"){
          tagList(           
            tags$div(class="select", selectInput("inSelect","",c("All fisheries", "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
            checkboxGroupInput("VariableSelect",  "Select one or more vessel length class:", choices=factorOrder$lengths, selected="")
          ) } else if(input$CategorySelect == "Homeport"){
            tagList(           
              tags$div(class="select", selectInput("inSelect","", c("All fisheries","All Catch Share fisheries","All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              tags$div(checkboxGroupInput("VariableSelect", div("Select one or more homeport:", style="margin-top:0; padding:-10px"), choices=factorOrder$port, selected=""))
            ) } else if(input$CategorySelect=="Fisheries"){
              tagList(
            #    actionButton("selectall2", "All fisheries", style="default", size="extra-small",block=F, type="action"),
            #    actionButton("selectallcs", "All Catch Share fisheries", style="default",size="extra-small", block=F, type="action"),
            #    actionButton("selectallncs", "All non-Catch Share fisheries", style="default", size="extra-small", block=F, type="action"),
                tags$div(class="ckbox2", checkboxGroupInput("VariableSelect", div("Select one or more fisheries:", style="margin-top:0; padding:-10px"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                                                              #     or select fisheries individually:  <button id='ivs' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
                                                            choices=c("All fisheries combined"="All fisheries"," All Catch Share fisheries combined"="All Catch Share fisheries",fish.var[3:13]), selected=""))
                )} 
       } #else if(input$Ind_sel=="Social and Regional"){
#         if(input$MetricSelect=="Share of landings by state"){
#    if(input$CategorySelect == "State"){
#      tagList(           
#        tags$div(class="select", selectInput("inSelect","",c("All fisheries","All Catch Share fisheries","All non-Catch Share fisheries")), style="margin-bottom:-10px"),
#        tags$div(class="rbutton2",  radioButtons("VariableSelect", "Select ONE state", choices = c("No state selected"="","Washington"="Washington", "Oregon"="Oregon","California"="California"), selected="")) 
#      )} else if(input$CategorySelect == "Vessel length class"){
#        tagList(           
#          tags$div(class="select", selectInput("inSelect","",c("All fisheries","All Catch Share fisheries","All non-Catch Share fisheries")), style="margin-bottom:-10px"),
#          tags$div(class="rbutton2", radioButtons("VariableSelect",  "Select ONE vessel length class", choices=c("No vessel length selected"="",factorOrder$lengths), selected=""))
#        )} else if(input$CategorySelect == "Homeport"){
#          tagList(           
#            tags$div(class="select", selectInput("inSelect","", c("All fisheries","All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
#            tags$div(class="rbutton2", radioButtons("VariableSelect", "Select ONE homeport", choices=c("No homeport selected"="",factorOrder$port), selected=""))
#          )} else if(input$CategorySelect=="Fisheries"){
#            tags$div(class="rbutton", radioButtons("VariableSelect", HTML("<div> Select ONE fishery <button id='iof' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
#                                                   choices=c("No fishery selected"="",fish.var), selected=""))}
#  }
#  else {
#        if(input$CategorySelect == "State"){
#          tagList(           
#            tags$div(class="select", selectInput("inSelect","",c("All fisheries",  "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
#            checkboxGroupInput("VariableSelect", "Select one or more state:", choices = factorOrder$state, selected="")
#          ) } else if(input$CategorySelect == "Vessel length class"){
#          tagList(           
#            tags$div(class="select", selectInput("inSelect","",c("All fisheries", "All Catch Share fisheries", "All non-Catch Share fsheries")), style="margin-bottom:-10px"),
#            checkboxGroupInput("VariableSelect",  "Select one or more vessel length class:", choices=factorOrder$lengths, selected="")
#          ) } else if(input$CategorySelect == "Homeport"){
#            tagList(           
#              tags$div(class="select", selectInput("inSelect","", c("All fisheries","All Catch Share fisheries","All non-Catch Share fisheries")), style="margin-bottom:-10px"),
#              tags$div(checkboxGroupInput("VariableSelect", div("Select one or more homeport:", style="margin-top:0; padding:-10px"), choices=factorOrder$port, selected=""))
#            ) } else if(input$CategorySelect=="Fisheries"){
#            tagList(
#          tags$div(class="ckbox2", checkboxGroupInput("VariableSelect", div("Select one or more fisheries:", style="margin-top:0; padding:-10px"),
                      #HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                      #or select fisheries individually:  <button id='ivs' type='button' 
                      #class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
#                      choices=c("All fisheries combined"="All fisheries"," All Catch Share fisheries combined"="All Catch Share fisheries",fish.var[3:13]), selected=""))
#            )} 
# } }
    } else return ()}
  })
   

#observe({
#   if (is.null(input$selectall) || input$selectall == 0) return() 
#  else if (input$selectall%%2 == 0) {
#     updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0)) } 
#  else {
#    updateCheckboxGroupInput(session,"VariableSelect", selected=factorOrder$port)
#  }
#})

#observe({
#  if (is.null(input$selectall2) || input$selectall2 == 0) return() 
#  else if (input$selectall2%%2 == 0) {
#    updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0)) } 
#  else {
#    updateCheckboxGroupInput(session,"VariableSelect", selected=fish.var)  }
#})

#observe({
#  if (is.null(input$selectallcs) || input$selectallcs == 0) return() 
#  else if (input$selectallcs%%2 == 0) {
#    updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0)) } 
#  else {
#    updateCheckboxGroupInput(session,"VariableSelect", selected=fish.var[2:8])
#  }
#})

#observe({
#  if (is.null(input$selectallncs) || input$selectallncs == 0) return() 
#  else if (input$selectallncs%%2 == 0) {
#    updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0)) } 
#  else {
#    updateCheckboxGroupInput(session,"VariableSelect", selected=fish.var[9:13])
#  }
#})


output$FishAkSelect <- renderUI({
  tags$div(class="ckbox", checkboxInput("FishAkSelect", p("Include Alaskan fisheries activities: ", 
                                                          span("By selecting this, you will include data from their activities in Alaska.", style="font-style:italic;font-size:10pt")), value = TRUE))
})

output$FishWhitingSelect <- renderUI({
  tags$div(class="ckbox", checkboxGroupInput("FishWhitingSelect", "Show data summed across:", choices=DatVars()$whitingv, selected=DatVars()$whitingv[1]))
})

observe({
  if (is.null(input$Ind_sel)) return()
#  if (input$Ind_sel=="Economic") return()
 else if(input$Ind_sel=="Economic") {
    updateRadioButtons(session, "MetricSelect", choices ="Select an economic measure and statistic below")
  }
  else  if(input$Ind_sel=="Demographic"){
    if(input$Sect_sel=="CV"){
      if(input$LayoutSelect!="Metrics"){
    updateRadioButtons(session, "MetricSelect", choices = c(DatVars()$METRIC[1:8]))
      } else {
        updateCheckboxGroupInput(session, "MetricSelect", choices = c(DatVars()$METRIC[1:8]))
      }
} else {
  if(input$LayoutSelect!="Metrics"){
  updateRadioButtons(session, "MetricSelect", choices = c(DatVars()$METRIC[1:6]))
  } else {
    updateCheckboxGroupInput(session, "MetricSelect", choices = c(DatVars()$METRIC[1:6]))
    
  }
}
  } else  if(input$Ind_sel=="Social and Regional"){
    if(input$Sect_sel=="CV"){
      if(input$LayoutSelect!="Metrics"){
    updateRadioButtons(session, "MetricSelect", choices = c(DatVars()$METRIC[9:13]))
      } else {
        updateCheckboxGroupInput(session, "MetricSelect", choices = c(DatVars()$METRIC[9:13]))
      }
    } else {
      if(input$LayoutSelect!="Metrics"){
      updateRadioButtons(session, "MetricSelect", choices = c(DatVars()$METRIC[7:10]))
      } else {
       updateCheckboxGroupInput(session, "MetricSelect", choices = c(DatVars()$METRIC[7:10]))
      }
    }
  } 
})

output$SectorSelect <- renderUI({
  tags$div(class="sectselect", selectInput("Sect_sel", span("West Coast Trawl Catch Share Program:", style="font-size:110%;font-style:italic; padding:5px; display:inline-block;vertical-align:middle"), c('Catcher Vessels'="CV", 'Mothership Vessels'="M", 'Catcher Processor Vessels'="CP", 'First Receivers and Shorebased Processors'="FR"), width='35%')
)
  })


output$VesSumSelect <- renderUI({
  if(PermitPlot()){
  if(input$VariableSelect!="All Fisheries"&input$VariableSelect!="All Catch Share fisheries"&input$VariableSelect!="All non-Catch Share fisheries") {
 
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
    updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[7:9]))
  } 
})

output$StatSelect2 <- renderUI({
  tagList(
    radioButtons("AVE_MED2", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
             if(input$MetricSelect=="Exponential Shannon Index"|input$MetricSelect=="Proportion of revenue from CS fishery"|input$MetricSelect=="Days at sea"|input$MetricSelect=="Vessel length"|input$MetricSelect=="Fishery participation"|
                input$MetricSelect=="Crew wage per day"|input$MetricSelect=="Revenue per crew day") { choices=c("Average", "Median")
             } else if(input$MetricSelect=="Number of vessels") {  choices=c("Total")
             } else if(input$MetricSelect=="Gini coefficient"|input$MetricSelect=="Herfindahl-Hirschman Index") {  choices=c("")
             }else if(input$MetricSelect=="Share of landings by state"|input$MetricSelect=="Seasonality") {  choices=c("")
             }else {choices=c("Average", "Median", "Total")
             }
  , selected=choices[1])
  )
})

#======================================

# Plot options

#======================================
output$PlotSelect <- renderUI({
      tags$div(class="ckbox", checkboxInput("PlotSelect", p(span("Plot options: ", style="font-weight:bold;font-size:12pt"),span("Show variance (standard deviation or median average deviation).", style="font-style:italic;font-size:10pt")), value=TRUE))
})


#===============text ==========================================#
output$SelectText <- renderText ({ 
  if(input$CategorySelect!="Fisheries"){
    HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
         <b>Show data summed across these fisheries: </b><button id='isummed' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></div>")
  } #else  if(input$CategorySelect=="Fisheries"){
#    HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:20px; margin-bottom:-25px;'>
#         <b>Select fisheries:</b></div>") 
#         <h5><i>Select a fishery group,  </i>
#         <button id='ifg' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></h5></div>")#,
#   }
})

observeEvent(input$reset_input, {
  if(input$LayoutSelect=="Metrics") {
    updateRadioButtons(session, "VariableSelect", selected="")  
  } else{
    updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0))
  }
})
