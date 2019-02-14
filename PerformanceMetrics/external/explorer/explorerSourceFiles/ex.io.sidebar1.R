#======================================

# this page handles all of the reactive 
# expressions for the dynamic user interface

#======================================
###################################################
## --------------------------- Sector Select ------------------------------------------##
###################################################
output$SectorSelect <- renderUI({
  #  tags$div(class="sectselect", selectInput("Sect_sel", span("West Coast Trawl Catch Share Program:", style="font-size:110%;font-style:italic; padding:10px; display:inline-block;vertical-align:middle"), 
  tags$div(class="sectselect", selectInput("Sect_sel", span("Select a sector:", style="font-style:strong;display:inline-block;vertical-align:middle"), 
                                           c('Catcher Vessels'="CV", 'Mothership Vessels'="M", 'Catcher-Processor Vessels'="CP", 'First Receivers and Shorebased Processors'="FR"
                                             ))
  )
})

output$SectPrint <- renderUI({
  tags$div(style="font-size:210%;font-style:italic; padding:10px; padding-left:15px;display:inline-block;vertical-align:middle",
           if(input$Sect_sel=="CV") {
             'West Coast Trawl Catch Share Program:  Catcher Vessels'
           } else if(input$Sect_sel=="M"){
             'West Coast Trawl Catch Share Program:  Mothership Vessels'
           } else if(input$Sect_sel=="CP"){
             'West Coast Trawl Catch Share Program:  Catcher-Processor Vessels'
           } else if(input$Sect_sel=="FR"){
             'West Coast Trawl Catch Share Program:  First Receivers and Shorebased Processors'
           }
  )
})
###################################################
#End
###################################################

#output$moreOptions <- renderUI({
  #tags$div(class="ckbox", checkboxInput("moreOptions", "I can't figure out how to drop this box - Erin", value = FALSE))
#})

########################################################
##YEAR
########################################################

output$Yearselect <- renderUI({
  if(input$Ind_sel=="Vessel characteristics"){
  if(input$Sect_sel=="CV" & input$CategorySelect=='Fisheries' & input$demSelect %in% c('Number of vessels', 'Vessel length', 'Gini coefficient')){
    tags$div(class="ckbox", sliderInput( "YearSelect","Years:", min = 2004, max = max(DatVars()$YEAR), 
                                         value = c(2009, max(DatVars()$YEAR)), step = 1, sep ='', ticks = F)) 
  } else {
    tags$div(class="ckbox", sliderInput( "YearSelect","Years:", min = 2009, max = max(DatVars()$YEAR), 
                                         value = c(2009, max(DatVars()$YEAR)), step = 1, sep ='', ticks = F))
  }}
  else if(input$Ind_sel == 'Other') {
    if(input$Sect_sel=="CV" & input$CategorySelect=='Fisheries'& input$socSelect %in% c('Seasonality','Share of landings by state')){
      tags$div(class="ckbox", sliderInput( "YearSelect","Years:", min = 2004, max = max(DatVars()$YEAR), 
                                           value = c(2009, max(DatVars()$YEAR)), step = 1, sep ='', ticks = F))
    }
    else {
      tags$div(class="ckbox", sliderInput( "YearSelect","Years:", min = 2009, max = max(DatVars()$YEAR), 
                                           value = c(2009, max(DatVars()$YEAR)), step = 1, sep ='', ticks = F))
    }
  }
  else if(input$Ind_sel == 'Economic') {
    if(input$Sect_sel == 'CV' & input$CategorySelect == 'Fisheries' & input$ShortdescrSelect[1] %in% 'Revenue') {
      tags$div(class="ckbox", sliderInput( "YearSelect","Years:", min = 2004, max = max(DatVars()$YEAR), 
                                           value = c(2009, max(DatVars()$YEAR)), step = 1, sep ='', ticks = F))
    }
    else {tags$div(class="ckbox", sliderInput( "YearSelect","Years:", min = 2009, max = max(DatVars()$YEAR), 
                                               value = c(2009, max(DatVars()$YEAR)), step = 1, sep ='', ticks = F))
    }
  }
  else if(input$Ind_sel == 'Crew') {
    tags$div(class="ckbox", sliderInput( "YearSelect","Years:", min = 2009, max = max(DatVars()$YEAR), 
                                               value = c(2009, max(DatVars()$YEAR)), step = 1, sep ='', ticks = F))
      }
  })
     
#END YEAR
##############################################

fish.var <- c("All fisheries combined"="All fisheries"," All catch share fisheries combined"="All catch share fisheries","Pacific whiting",
              "At-sea Pacific whiting","Shoreside Pacific whiting",
              "Groundfish with trawl gear","DTS trawl with trawl endorsement","Non-whiting midwater trawl","Non-whiting, non-DTS trawl with trawl endorsement", 
              "Groundfish fixed gear with trawl endorsement",
              "All non-catch share fisheries combined"="All non-catch share fisheries", "Crab","Shrimp",
              "Other fisheries")
###################################################
#CATEGORY SELECT
###################################################
output$Categoryselect <- renderUI({
  if(input$Sect_sel!="FR"){
  tags$div(class="ckbox", radioButtons("CategorySelect", "Group vessels according to:", choices = DatVars()$CATEGORY, selected=DatVars()$CATEGORY[1]))
} else {  tags$div(class="ckbox", radioButtons("CategorySelect", "Group processors according to:", choices = DatVars()$CATEGORY, selected=DatVars()$CATEGORY[1]))
}
  })
###################################################
# End Category selecto
###################################################

###################################################
#INDICATOR SELECT - Vessel characteristics, ECONOMIC, SOCIAL, and CREW (for all except FR)
###################################################
output$IndicatorSelect <- renderUI({
  if(input$Sect_sel != 'FR') {
#    tagList(
#  if(input$tabs=='Panel2'){
#  selectInput("Ind_sel", HTML("<div> Select an indicator category: <button id='ipo' type='button' class='btn btn-default action-button shiny-bound-input'> 
#                              <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), c('Demographic',"Economic","Social and Regional"), selected='Economic',selectize=T)#,
#  } else {
    selectInput("Ind_sel", HTML("<div> Select an indicator category: <button id='ipo' type='button' class='btn btn-default action-button shiny-bound-input'> 
                              <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), c('Vessel characteristics',"Economic","Crew", "Other"), selected='Vessel characteristics',selectize=T)
  } else { 
    selectInput("Ind_sel", HTML("<div> Select an indicator category: <button id='ipo' type='button' class='btn btn-default action-button shiny-bound-input'> 
                              <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), c('Vessel characteristics',"Economic","Other"), selected='Vessel characteristics',selectize=T)}
    
#  }
    })  #END INDICATOR SELECT
###################################################
#End indicator select
###################################################

###################################################
##METRIC SELECT
###################################################
output$Metricselect <- renderUI({ 
  if(input$LayoutSelect!="Metrics"){
    if(input$Ind_sel=="Economic") {
      tags$div(class='met_mod', radioButtons("MetricSelect","", choices ="Select an economic measure and statistic below"), style="font-style:italic;margin-bottom:20px;margin-top:-32px;margin-left:-15px;padding-top:0;")
    } #end economic
    else# if(input$Ind_sel=="Demographic")
      {
        tags$div(class='met_mod', radioButtons("MetricSelect","", choices ="Select a metric below"), style="font-style:italic;margin-bottom:20px;margin-top:-32px;margin-left:-15px;padding-top:0;")
#      if(input$Sect_sel=="CV"){
#        tags$div(class="statbox",radioButtons("MetricSelect","", choices = c(DatVars()$METRIC[1:8]), selected=DatVars()$METRIC[1]))
#      } else if(input$Sect_sel=="FR"){
#        tags$div(class="statbox",radioButtons("MetricSelect","", choices = c(DatVars()$METRIC[1:6]), selected=DatVars()$METRIC[1]))
 #     }else {
#        tags$div(class="statbox",radioButtons("MetricSelect","", choices = c(DatVars()$METRIC[1:6]), selected=DatVars()$METRIC[1]))
#      }
      } # end demographic
 #   else if(input$Ind_sel=="Social and Regional"){
#      radioButtons("MetricSelect","",choices="Select a metric below")
 #     if(input$Sect_sel=="CV"){
#        tags$div(class="statbox",radioButtons("MetricSelect","", choices = c(DatVars()$METRIC[9:13]), selected=DatVars()$METRIC[9]))
#      } else if(input$Sect_sel=="FR"){
#        tags$div(class="statbox",radioButtons("MetricSelect","", choices = c(DatVars()$METRIC[7:8]), selected=DatVars()$METRIC[7]))
#      }else {
#        tags$div(class="statbox",radioButtons("MetricSelect","", choices = c(DatVars()$METRIC[7:10]), selected=DatVars()$METRIC[7]))
#      }
#    } #end social and regional 
  } else {
    ##Metric Select
    if(input$Ind_sel=="Economic") {
     tags$div(class='met_mod', radioButtons("MetricSelect","", choices ="Select an economic measure and statistic below"), style="font-style:italic;margin-bottom:20px;margin-top:-32px;margin-left:-15px;padding-top:0;")
    }
    else #if(input$Ind_sel=="Demographic")
      {
      # -------> MODIFY THIS PART <--------#
      tags$div(class='met_mod', radioButtons("MetricSelect","", choices ="Select a metric below"), style="font-style:italic;margin-bottom:-20px;margin-top:-32px;margin-left:-15px;padding-top:0;") #NEED TO MODIFY THIS!!!!!!!!!!!!!!
      # -------> MODIFY THIS PART <--------#
#      if(input$Sect_sel=="CV"){
 #       tags$div(class='statbox',checkboxGroupInput("MetricSelect","", choices = c(DatVars()$METRIC[1:8])),selected=DatVars()$METRIC[1])
 #     } else if(input$Sect_sel=="FR"){
 #       tags$div(class='statbox',checkboxGroupInput("MetricSelect","", choices = c(DatVars()$METRIC[1:6]), selected=DatVars()$METRIC[1]))
 #     }else {
 #       tags$div(class="statbox",checkboxGroupInput("MetricSelect","", choices = c(DatVars()$METRIC[1:6]), selected=DatVars()$METRIC[1]))
 #     }
      } #end demographic
 #   else if(input$Ind_sel=="Social and Regional"){
#      radioButtons("MetricSelect","",choices="Select a metric below")
#        if(input$Sect_sel=="CV"){
#        tags$div(class="statbox",checkboxGroupInput("MetricSelect","", choices = c(DatVars()$METRIC[9:12]), selected=DatVars()$METRIC[9]))
#      } else if(input$Sect_sel=="FR"){
#        tags$div(class="statbox",checkboxGroupInput("MetricSelect","", choices = c(DatVars()$METRIC[7]), selected=DatVars()$METRIC[7]))
#      }else {
#        tags$div(class="statbox",checkboxGroupInput("MetricSelect","", choices = c(DatVars()$METRIC[7:10]), selected=DatVars()$METRIC[7]))
#      }
#    } #end social and regional
  }
}) #END METRIC SELECT
###################################################
#End Metric Select
###################################################

###################################################
##VARIABLE SELECT
###################################################
output$Variableselect <- renderUI({  
  if(!is.null(input$CategorySelect)){
    if(input$Sect_sel=="M"|input$Sect_sel=="CP"){
      tags$div(class="ckbox2", checkboxGroupInput("VariableSelect","",choices="At-sea Pacific whiting", selected=""))
    } 
    else if(input$Sect_sel=="CV"){
      if(input$CategorySelect == "State"){
        if(input$LayoutSelect!='Metrics'){
          #if(input$Ind_sel=="Social and Regional"){
             if(input$Ind_sel=='Vessel characteristics'||input$Ind_sel=='Economic'||input$Ind_sel == 'Crew' || input$Ind_sel=='Other'&input$socSelect!="Share of landings by state"){
                       tagList(           
                        tags$div(class="select", selectInput("inSelect","",c("All fisheries",  "All catch share fisheries", "All non-catch share fisheries")), style="margin-bottom:-10px"),
                        checkboxGroupInput("VariableSelect", "Select one or more state:", choices = factorOrder$state, selected=""))
                    }
          else{
              tagList(           
              tags$div(class="select", selectInput("inSelect","",c("All fisheries",  "All catch share fisheries", "All non-catch share fisheries")), style="margin-bottom:-10px"),
              tags$div(radioButtons("VariableSelect", "Select one state:", choices = c(#"No state selected"="",
                factorOrder$state), selected="")))
       }}#} #end not metrics
        else {
            tagList(           
              tags$div(class="select", selectInput("inSelect","",c("All fisheries",  "All catch share fisheries", "All non-catch share fisheries")), style="margin-bottom:-10px"),
              tags$div(class="rbutton3",radioButtons("VariableSelect", "Select one state:", choices = c("No state selected"="",factorOrder$state), selected="")))
          }
      } else if(input$CategorySelect == "Vessel length class"){
        if(input$LayoutSelect!='Metrics'){
          if(input$Ind_sel=="Vessel characteristics"||input$Ind_sel=="Economic"||input$Ind_sel == 'Crew' || input$Ind_sel=="Other"&input$socSelect!="Share of landings by state"){
            tagList(           
            tags$div(class="select", selectInput("inSelect","",c("All fisheries", "All catch share fisheries", "All non-catch share fisheries")), style="margin-bottom:-10px"),
            checkboxGroupInput("VariableSelect",  "Select one vessel length class:", 
                               choices=c("Large vessel (> 80 ft)","Medium vessel (> 60ft, <= 80ft)","Small vessel (<= 60 ft)"), selected="")) 
          }  
          else {
           tagList(           
              tags$div(class="select", selectInput("inSelect","",c("All fisheries", "All catch share fisheries", "All non-catch share fisheries")), style="margin-bottom:-10px"),
              radioButtons("VariableSelect",  "Select one vessel length class:", choices=c("Large vessel (> 80 ft)","Medium vessel (> 60ft, <= 80ft)",
                                                                                           "Small vessel (<= 60 ft)"), selected="")) 
          }}else {
            tagList(           
              tags$div(class="select", selectInput("inSelect","",c("All fisheries", "All catch share fisheries", "All non-catch share fisheries")), style="margin-bottom:-10px"),
              tags$div(class="rbutton3", radioButtons("VariableSelect",  "Select one vessel length class:", 
                                                      choices=c("No vessel length selected"="","Large vessel (> 80 ft)","Medium vessel (> 60ft, <= 80ft)","Small vessel (<= 60 ft)"), selected=""))) 
          }
      } else if(input$CategorySelect == "Homeport"){
        if(input$LayoutSelect!="Metrics"){
          if(input$Ind_sel=="Vessel characteristics"||input$Ind_sel=="Economic"||input$Ind_sel == 'Crew' || input$Ind_sel=="Other"&input$socSelect!="Share of landings by state"){
            tagList(           
              tags$div(class="select", selectInput("inSelect","", c("All fisheries","All catch share fisheries","All non-catch share fisheries")), style="margin-bottom:-10px"),
              tags$div(checkboxGroupInput("VariableSelect", div("Select one or more homeport:", style="margin-top:0; padding:-10px"), choices=factorOrder$port, selected="")))  
          }
          else {
            tagList(           
              tags$div(class="select", selectInput("inSelect","", c("All fisheries","All catch share fisheries","All non-catch share fisheries")), style="margin-bottom:-10px"),
              tags$div(radioButtons("VariableSelect", div("Select one homeport:", style="margin-top:0; padding:-10px"), choices=factorOrder$port, selected=""))) 
          } 
          }else {
            tagList(           
              tags$div(class="select", selectInput("inSelect","", c("All fisheries","All catch share fisheries","All non-catch share fisheries")), style="margin-bottom:-10px"),
              tags$div(class="rbutton3",radioButtons("VariableSelect", div("Select one homeport:", style="margin-top:0; padding:-10px"), 
                                                     choices=c("No homeport selected"="",factorOrder$port), selected=""))) 
          }
      } #end homeport
      else if(input$CategorySelect=="Fisheries"){
        if(input$LayoutSelect!='Metrics'){
          if(input$Ind_sel=="Economic"||input$Ind_sel=="Vessel characteristics"||input$Ind_sel == 'Crew' || input$Ind_sel=="Other" &input$socSelect[1]!="Share of landings by state"){
            if(input$tabs=='Panel1'){
           tags$div(class="ckbox2", checkboxGroupInput("VariableSelect", div("Select one or more fisheries:", style="margin-top:0; padding:-10px"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                                        choices=c("All fisheries combined"="All fisheries"," All catch share fisheries combined"="All catch share fisheries",fish.var[3:14])))
            } else {
              tags$div(class="ckbox2", checkboxGroupInput("VariableSelect", div("Select one or more fisheries:", style="margin-top:0; padding:-10px"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                                          choices=c("All fisheries combined"="All fisheries"," All catch share fisheries combined"="All catch share fisheries",fish.var[3:14]), selected=c('Groundfish with trawl gear','Groundfish fixed gear with trawl endorsement')))
          }}
          else {
            tagList(
             tags$div(class="ckbox3", radioButtons("VariableSelect", div("Select one fisheries:", style="margin-top:0; padding:-10px"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                                  choices=c("No fishery selected"="","All fisheries combined"="All fisheries"," All catch share fisheries combined"="All catch share fisheries",fish.var[3:14]), selected="")))
          }  
          }else {
            tags$div(class="ckbox3", radioButtons("VariableSelect", div("Select one fisheries:", style="margin-top:0; padding:-10px"), 
                                                  choices=c("No fishery selected"="","All fisheries combined"="All fisheries"," All catch share fisheries combined"="All catch share fisheries",fish.var[3:14]), selected=""))
          } 
      }#end fisheries
    } ##END Catcher Vessels
    else if(input$Sect_sel=="FR"){
      if(input$CategorySelect == "Region"){
        if(input$LayoutSelect!='Metrics'){
          if(input$Ind_sel=="Vessel characteristics"){
            if(input$demSelect!="Proportion of revenue from catch share species"){
            tagList(
              tags$div(class="select", selectInput("inSelect","",c("All production",  "Groundfish production", "Other species production")), style="margin-bottom:-10px"),
              tags$div(class="ckbox", checkboxGroupInput("VariableSelect", HTML("<div> Select one or more region:<button id='FRr' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"),
                                                         choices = c('Washington and Oregon','California'), selected="")))
          } else {
            tagList(
              tags$div(class="select", selectInput("inSelect","",c("All production")), style="margin-bottom:-10px"),
              tags$div(class="ckbox", checkboxGroupInput("VariableSelect", HTML("<div> Select one or more region:<button id='FRr' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"),
                                                       choices = c('Washington and Oregon','California'), selected=""))) 
          }  
        } else if(input$Ind_sel=="Economic"){
            tagList(
              tags$div(class="select", selectInput("inSelect","",c("All production",  "Groundfish production", "Other species production")), style="margin-bottom:-10px"),
              tags$div(class="ckbox", checkboxGroupInput("VariableSelect", HTML("<div> Select one or more region:<button id='FRr' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"),
                                                       choices = c('Washington and Oregon','California'), selected="")))
            } else {
              tagList(
                tags$div(class="select", selectInput("inSelect","",c("All production")), style="margin-bottom:-10px"),
                tags$div(class="ckbox", checkboxGroupInput("VariableSelect", HTML("<div> Select one or more region:<button id='FRr' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"),
                                                         choices = c('Washington and Oregon','California'), selected="")) )
            }
          } #ENd compare processors
        else {
          if(input$Ind_sel!="Other"){
            tagList(
                  tags$div(class="select", selectInput("inSelect","",c("All production",  "Groundfish production", "Other species production")), style="margin-bottom:-10px"),
                  tags$div(class="rbutton3",radioButtons("VariableSelect", HTML("<div> Select one region:<button id='FRr' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"), choices = c("No region selected"="",
                                                        'Washington and Oregon','California'), selected=""))
                )
          } else {
            tagList(
              tags$div(class="select", selectInput("inSelect","",c("All production")), style="margin-bottom:-10px"),
              tags$div(class="rbutton3",radioButtons("VariableSelect", HTML("<div> Select one region:<button id='FRr' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"), choices = c("No region selected"="",
                                                                                                                             'Washington and Oregon','California'), selected=""))) 
          }
          }
      } else if(input$CategorySelect =="Processor size"){
        if(input$LayoutSelect!='Metrics'){
          if(input$Ind_sel=="Vessel characteristics"){
            if(input$demSelect!="Proportion of revenue from catch share species"){
            tagList(
              tags$div(class="select", selectInput("inSelect","",c("All production",  "Groundfish production", "Other species production")), style="margin-bottom:-10px"),
              tags$div(checkboxGroupInput("VariableSelect",  HTML("<div> Select one size class:<button id='FRs' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"), choices=c("Large",'Medium','Small'), selected=""))
            )
          } else {
            tagList(
              tags$div(class="select", selectInput("inSelect","",c("All production")), style="margin-bottom:-10px"),
              tags$div(checkboxGroupInput("VariableSelect",  HTML("<div> Select one size class:<button id='FRs' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"), choices=c("Large",'Medium','Small'), selected="")))
          }
          } else if(input$Ind_sel=='Economic'){
            tagList(
              tags$div(class="select", selectInput("inSelect","",c("All production",  "Groundfish production", "Other species production")), style="margin-bottom:-10px"),
              tags$div(checkboxGroupInput("VariableSelect",  HTML("<div> Select one size class:<button id='FRs' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"), choices=c("Large",'Medium','Small'), selected=""))
            )  
          } else {
            tagList(
              tags$div(class="select", selectInput("inSelect","",c("All production")), style="margin-bottom:-10px"),
              tags$div(checkboxGroupInput("VariableSelect",  HTML("<div> Select one size class:<button id='FRs' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"), choices=c("Large",'Medium','Small'), selected="")))
          }
          }else {
            if(input$Ind_sel!="Other"){
                  tagList(
                    tags$div(class="select", selectInput("inSelect","",c("All production",  "Groundfish production", "Other species production")), style="margin-bottom:-10px"),
                    tags$div(class="rbutton3",radioButtons("VariableSelect",  HTML("<div> Select one size class:<button id='FRs' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"), choices=c("No processor size selected"="","Large",'Medium','Small'), selected=""))
                  )
            } else {                   
              tagList(
                tags$div(class="select", selectInput("inSelect","",c("All production")), style="margin-bottom:-10px"),
                tags$div(class="rbutton3",radioButtons("VariableSelect",  HTML("<div> Select one size class:<button id='FRs' type='button' class='btn btn-default action-button shiny-bound-input'> 
                               <i class='fa fa-info-circle fa-fw'></i></button></div>"), choices=c("No processor size selected"="","Large",'Medium','Small'), selected="")))
            }
                  }
      } #End processor class
      else {
        if(input$LayoutSelect!='Metrics'){
          if(input$Ind_sel=="Vessel characteristics"|input$Ind_sel=="Economic"){
            tags$div(class='FRprod',checkboxGroupInput("VariableSelect", HTML("<div> Select one or more production activities:<button id='FRi' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button> <style margin-top:0; padding:-10px> </style></div>"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                        choices=c("All production","Groundfish production","Pacific whiting production",'Non-whiting groundfish production','Other species production'), selected=""))
          }
          else if(input$Ind_sel=="Other"){
            tags$div(class='StatGrey3', checkboxGroupInput("VariableSelect", HTML("<div> Select one production activity:<button id='FRi' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                         <i class='fa fa-info-circle fa-fw'></i></button> <style margin-top:0; padding:-10px> </style></div>"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                  choices=c("All production","Groundfish production","Pacific whiting production",'Non-whiting groundfish production','Other species production'), selected=""))
         }}else {
           if(input$Ind_sel=="Other"){
             tags$div(class='StatGrey4', radioButtons("VariableSelect", HTML("<div> Select one production activity:<button id='FRi' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                                               <i class='fa fa-info-circle fa-fw'></i></button> <style margin-top:0; padding:-10px> </style></div>"),
                                                        choices=c("No production activities selected"="","All production","Groundfish production","Pacific whiting production",'Non-whiting groundfish production','Other species production'), selected=""))
             } else{
            tags$div(class='rbutton2', radioButtons("VariableSelect", HTML("<div> Select one production activity:<button id='FRi' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button> <style margin-top:0; padding:-10px> </style></div>"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                  choices=c("No production activities selected"="","All production","Groundfish production","Pacific whiting production",'Non-whiting groundfish production','Other species production'), selected=""))
          }} #End metrics
      } #end fisheries All production; Pacific whiting production; Non-whiting groundfish production; Other species production
    } #End FR
  }#end not null 
  else return ()
}) #END VARIABLE SELECT
###################################################
#End Variable select
###################################################

###################################################
#Select FISHAK
###################################################
output$FishAkselect <- renderUI({
  if(input$Sect_sel=='CV'){
  tags$div(class="ckbox", checkboxInput("FishAkSelect", p("Include Alaskan fisheries activities: ", 
                                                          span("By selecting this, you will include data from their activities in Alaska.", style="font-style:italic;font-size:10pt")), value = FALSE))
  } else {
    tags$div(class="ckbox", checkboxInput("FishAkSelect", p("Include Alaskan fisheries activities: ", 
                                                            span("By selecting this, you will include data from their activities in Alaska.", style="font-style:italic;font-size:10pt")), value = FALSE))
}
    })
###################################################
#end fishak
###################################################

###################################################
#Select whiting (data summed across category)
###################################################

output$FishWhitingselect <- renderUI({tags$div(class='ckbox', checkboxGroupInput('FishWhitingSelect', HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
                                                                               </button></div>"), choices=DatVars()$whitingv, selected=DatVars()$whitingv[1]))
})
##grey out the Non-whiting and Whiting vessel type selections for the Days at Sea metric for Level A CS fisheries and Level B non-CS fisheries due to confidentiality
##All economic metrics
##Social and Regional: Number of positions, crew wage per day, revenue per position day
#whitingy <- c('All fisheries','All catch share fisheries','Pacific whiting','Groundfish with trawl gear','Groundfish fixed gear with trawl endorsement','All non-catch share fisheries')
#whitingn <- c('At-sea Pacific whiting','Shoreside Pacific whiting','DTS trawl with trawl endorsement','Non-whiting midwater trawl',
                #'Non-whiting, non-DTS trawl with trawl endorsement','Other fisheries','Crab','Shrimp')

#output$FishWhitingselect <- renderUI({
  #if(input$Sect_sel == 'CV') {
    #if(input$Ind_sel=='Demographic') {
      #if(input$demSelect == 'Days at sea') {
        #if(is.null(input$VariableSelect)){
         # tags$div(class="ckbox", checkboxGroupInput("FishWhitingSelect", HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
                                                                               #</button></div>"), choices=DatVars()$whitingv, selected=DatVars()$whitingv[1]))
       
        #}else if(input$VariableSelect %in% whitingn) {tags$div(class='StatGrey1',checkboxGroupInput("FishWhitingSelect", HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
                                                                       #</button> <style margin-top:0; padding:-10px> </style></div>"), choices=c("All vessels","Non-whiting vessels", "Whiting vessels"), selected= "All vessels"))
          #}else if(input$VariableSelect %in% whitingy) {tags$div(class="ckbox", checkboxGroupInput("FishWhitingSelect", HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
                                                                       #</button></div>"), choices=DatVars()$whitingv, selected=DatVars()$whitingv[1]))
        
         # }}else{tags$div(class="ckbox", checkboxGroupInput("FishWhitingSelect", HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
                                                                       #</button></div>"), choices=DatVars()$whitingv, selected=DatVars()$whitingv[1]))
          
       # }}else if(input$Ind_sel == 'Economic') {
        #if(is.null(input$VariableSelect)){
          #tags$div(class="ckbox", checkboxGroupInput("FishWhitingSelect", HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
                                                                               #</button></div>"), choices=DatVars()$whitingv, selected=DatVars()$whitingv[1]))
        #}else if(input$VariableSelect %in% whitingn){tags$div(class='StatGrey1',checkboxGroupInput("FishWhitingSelect", HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
                                                                                                                                                                                                              #</button> <style margin-top:0; padding:-10px> </style></div>"), choices=c("All vessels","Non-whiting vessels", "Whiting vessels"), selected= "All vessels"))
          
        #}else if(input$VariableSelect %in% whitingy) {tags$div(class="ckbox", checkboxGroupInput("FishWhitingSelect", HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
                                                                               #</button></div>"), choices=DatVars()$whitingv, selected=DatVars()$whitingv[1]))
        #}else{tags$div(class="ckbox", checkboxGroupInput("FishWhitingSelect", HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
                                                                       #</button></div>"), choices=DatVars()$whitingv, selected=DatVars()$whitingv[1]))
        #}}else if(input$Ind_sel == 'Social and Regional') {
        #if(!input$socSelect %in% c('Seasonality','Share of landings by state')){
          #if(is.null(input$VariableSelect)){
            #tags$div(class="ckbox", checkboxGroupInput("FishWhitingSelect", HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
                                                                                 #</button></div>"), choices=DatVars()$whitingv, selected=DatVars()$whitingv[1]))
          #}else if(input$VariableSelect %in% whitingn) {tags$div(class='StatGrey1',checkboxGroupInput("FishWhitingSelect", HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
                                                                                                                                                                                                               # </button> <style margin-top:0; padding:-10px> </style></div>"), choices=c("All vessels","Non-whiting vessels", "Whiting vessels"), selected= "All vessels"))
            
          #}else if(input$VariableSelect %in% whitingy) {tags$div(class="ckbox", checkboxGroupInput("FishWhitingSelect", HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
                                                                                # </button></div>"), choices=DatVars()$whitingv, selected=DatVars()$whitingv[1]))
        
        #}}else{tags$div(class="ckbox", checkboxGroupInput("FishWhitingSelect", HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
                                                                       #</button></div>"), choices=DatVars()$whitingv, selected=DatVars()$whitingv[1]))
          #}}
  #else{tags$div(class="ckbox", checkboxGroupInput("FishWhitingSelect", HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
                                                                       #</button></div>"), choices=DatVars()$whitingv, selected=DatVars()$whitingv[1]))
#}}})
###################################################
#end whiting
###################################################

###################################################
#Economic measure select
###################################################
output$Shortdescrselect <- renderUI({ 
  if(input$LayoutSelect=='Metrics'){
    tags$div(class="ckbox", checkboxGroupInput("ShortdescrSelect", HTML("<div> Economic measures:<button id='iem' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i> 
                                                                        </button></div>"), 
                                               choices = DatVars()$SHORTDESCR, selected = DatVars()$SHORTDESCR))
  } else {
    if(input$tabs=='Panel1'){
    tags$div(class="ckbox", radioButtons("ShortdescrSelect", HTML("<div> Economic measures:<button id='iem' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i> 
                                                                  </button></div>"), 
                                         choices = DatVars()$SHORTDESCR, selected =DatVars()$SHORTDESCR[1]))
    } else {
      tags$div(class="ckbox", radioButtons("ShortdescrSelect", HTML("<div> Economic measures:<button id='iem' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i> 
                                                                  </button></div>"), 
                                           choices = DatVars()$SHORTDESCR, selected =DatVars()$SHORTDESCR[4]))
    }
  }
})

###################################################
#End economic measure select
###################################################

###################################################
# Identify Individual metrics for demography
###################################################
##DatVars() is defined in ex.reactives##
##Checkbox types are defined in ui.R
##Note: See statistic selection below for settings of 'Mean' 'Median' 'Total' when grouping by vessels##
output$demselect <- renderUI({
##Settings for grouping by 'Metrics'
  if(input$LayoutSelect=='Metrics'){
    if(input$Sect_sel=="CV"){
      if(input$AVE_MED2=="Total"){
      tags$div(class="ckboxCV",checkboxGroupInput("demSelect","", choices = c(DatVars()$METRIC1), selected = 'Number of vessels'))
      } else {
        tags$div(class="ckboxCV2",checkboxGroupInput("demSelect","", choices = c(DatVars()$METRIC1), selected = 'Vessel length')) 
      }
    } else {
          if(input$Sect_sel=='FR'){
            if(input$AVE_MED2=='Total'){
          tags$div(class="ckboxFR",checkboxGroupInput("demSelect","", choices = c(DatVars()$METRIC1), selected = "Number of processors"))
        } else {
          tags$div(class="ckboxCV2",checkboxGroupInput("demSelect","", choices = c(DatVars()$METRIC1), selected = "Number of species processed"))
        }
    
  } else {
    if(input$Sect_sel == 'CP'| input$Sect_sel == 'M') {
      if(input$AVE_MED2 == 'Total') {
        tags$div(class="ckboxCP",checkboxGroupInput("demSelect","", choices = c(DatVars()$METRIC1), selected = 'Number of vessels'))
      } else {
        tags$div(class="ckboxCV2",checkboxGroupInput("demSelect","", choices = c(DatVars()$METRIC1), selected = 'Vessel length'))
      }
##Settings for 'Group by vessels' or 'Group by processors'
    }}}} else {
        if(input$Sect_sel == 'FR') {
        tags$div(class="statbox",radioButtons("demSelect","", choices = c(DatVars()$METRIC1), selected= 'Number of processors'))
        } else {
          tags$div(class="statbox",radioButtons("demSelect","", choices = c(DatVars()$METRIC1), selected='Number of vessels'))
        }
}})



###################################################
#end demography select
###################################################

###################################################
#Identify individual metrics for crew class
###################################################
output$crewselect <- renderUI({
  if(input$LayoutSelect == 'Metrics') {
    if(input$Sect_sel != 'FR') {
      if(input$AVE_MED2 == 'Total') {
        tags$div(class="ckboxCV3",checkboxGroupInput("crewSelect","", choices = c(DatVars()$METRIC2), selected = "Number of positions (captain and crew)"))
      } else {
        tags$div(class="ckbox",checkboxGroupInput("crewSelect","", choices = c(DatVars()$METRIC2), selected = "Number of positions (captain and crew)"))
      }
    }}
  else {
      if(input$AVE_MED2 == 'Total') {
        tags$div(class="statbox",radioButtons("crewSelect","", choices = c(DatVars()$METRIC2), selected = "Number of positions (captain and crew)"))
      } else {
        tags$div(class="statbox",radioButtons("crewSelect","", choices = c(DatVars()$METRIC2)))
      }
    }
})
###################################################
#Identify individual metrics for Other class
###################################################
output$socselect <- renderUI({
##Setting when grouping by Metrics#####
    if(input$LayoutSelect=='Metrics'){
      if(input$Sect_sel=="CV"){
        if(input$AVE_MED2 =="Total"){
        tags$div(class="ckboxSOC2",checkboxGroupInput("socSelect","", choices = c(DatVars()$METRIC3a), selected = "Revenue per position-day"))
       } else {
        tags$div(class="ckboxSOC",checkboxGroupInput("socSelect","", choices = c(DatVars()$METRIC3a), selected = "Revenue per position-day")) 
       }
      } else {
        if(input$Sect_sel=="FR"){
          if(input$AVE_MED2 == 'Total') {
          tags$div(class="ckboxSOC3",checkboxGroupInput("socSelect","", choices = c(DatVars()$METRIC2), selected = 'Gini coefficient'))
      } else {
        tags$div(class="ckboxCV2",checkboxGroupInput("socSelect","", choices = c(DatVars()$METRIC2), selected = "Number of workers"))
        }} else {
            if(input$Sect_sel == 'CP' | input$Sect_sel == 'M') {
              if(input$AVE_MED2 == 'Total') {
                tags$div(class="ckbox",checkboxGroupInput("socSelect","", choices = c(DatVars()$METRIC3), selected = "Revenue per position-day"))
              } else {
                tags$div(class="ckboxSOC3",checkboxGroupInput("socSelect","", choices = c(DatVars()$METRIC3), selected = "Revenue per position-day"))
              }
            }
##Settings when 'Groups of vessels" or 'Groups of processors'#####
      }}} else {
        if(input$Sect_sel=="FR"){
          tags$div(class="statbox",radioButtons("socSelect","", choices = c(DatVars()$METRIC2), selected = "Gini coefficient"))

     } else {
       tags$div(class="statbox",radioButtons("socSelect","", choices = c(DatVars()$METRIC3), selected = "Revenue per position-day"))
  }
  }
})
###################################################

###################################################
#Show data summed across button
###################################################
output$VesSumSelect <- renderUI({
  if(PermitPlot()){
    if(input$VariableSelect!="All Fisheries"&input$VariableSelect!="All catch share fisheries"&input$VariableSelect!="All non-catch share fisheries") {
      
      tagList(
        tags$div(class="ckbox", radioButtons("VesSum", HTML("<div> Show data summed: <button id='iVesSum' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                                             choices=c("within selected fisheries"="within","across all catch share fisheries"="acrossCS","across all West Coast fisheries"="acrossWC")))
      )} else {return()}
  } else {return()}
})
#span("For all vessels that fished within selected fisheries, show data for activities:", style="font-size:11pt; font-weight:bold;"), #font-style:italic;

###################################################
#Statistic select for economic measures
###################################################
#Being by select average, median, or total values
output$Statselect <- renderUI({
  if(input$Sect_sel=="FR")  { 
    tagList(
      selectInput("AVE_MED", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                  c('Median, Mean, or Total values'="", Mean="A", Median="M", Total="T"), selectize=F),
      tags$div(class="statbox", radioButtons("StatSelect","",  choices = c(DatVars()$STAT[3:4]))))
  } else {
    tagList(
      selectInput("AVE_MED", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                  c('Median, Mean, or Total values'="", Mean="A", Median="M", Total="T"), selectize=F),
      tags$div(class="statbox", radioButtons("StatSelect","",  choices = c(DatVars()$STAT[4:6]), selected=DatVars()$STAT[4])))
  }
})

#select whether to show values per vessel, /vessel/day, or /vessel/metric-ton
observe({
  if (is.null(input$AVE_MED)) {return()}
  else 
  if(input$Sect_sel!='FR'){
    if(input$AVE_MED=="M"){
      updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[4:6]))
    }  else if(input$AVE_MED=="A"){
      updateRadioButtons(session,"StatSelect",   choices = c(DatVars()$STAT[1:3]))
    } else  if(input$AVE_MED=="T"){
      updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[7:9]))
    } 
  } else if(input$AVE_MED=="M"){
    updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[3:4]))
  }  else if(input$AVE_MED=="A"){
    updateRadioButtons(session,"StatSelect",   choices = c(DatVars()$STAT[1:2]))
  } else  if(input$AVE_MED=="T"){
    updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[5:6]))
  } 
})
###################################################
#End stat select  for economic measures
###################################################

###################################################
## Stat selection for non-economic metrics
###################################################
output$StatSelect2 <- renderUI({
  if(input$LayoutSelect=='Metrics'){
    tagList(
    radioButtons("AVE_MED2", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                   choices=c("Mean","Median",'Total'), select='Median'))
  } #End Metrics
    else {
      if(input$Ind_sel=="Vessel characteristics"){
        if(input$demSelect %in% c("Number of vessels","Number of processors")){
          tagList(
            tags$div(class='StatGrey', radioButtons("AVE_MED2", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                         choices=c("Mean","Median",'Total'), select='Total'))
            )
        } else if(input$demSelect %in% c("Vessel length", "Exponential Shannon Index", "Fishery participation", "Proportion of revenue from CS fishery", 
                                         "Proportion of revenue from catch share species", "Proportion of landings from CS fishery")){
          tagList(
            tags$div(class='StatGrey2', radioButtons("AVE_MED2", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                                                    choices=c("Mean","Median",'Total'), select='Median'))
          )
        } else{
        tagList(
          radioButtons("AVE_MED2", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                       choices=c("Mean","Median",'Total'), select='Median'))
      }
        } #End Vessel characteristics
      else if(input$Ind_sel=="Other"){
        if(input$socSelect %in% c("Share of landings by state","Seasonality", "Gini coefficient")) { 
          tags$div(class='met_mod', radioButtons("AVE_MED2","", choices =""), style="margin-bottom:20px;margin-top:-32px;margin-left:-15px;padding-top:0;")
      } else if (input$socSelect %in% c("Fuel use per day", "Speed while fishing", "Hourly compensation")) {
          tagList(
            tags$div(class='StatGrey2', radioButtons("AVE_MED2", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                         choices=c("Mean","Median",'Total'), select='Median')))            
      } else {
        tagList(
          radioButtons("AVE_MED2", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                       choices=c("Mean","Median",'Total'), select='Median'))            
        }
      }
      #End Other
      else if (input$Ind_sel == 'Crew') {
        if(input$crewSelect %in% c('Crew wage per day')) {
          tagList(
            tags$div(class='StatGrey2', radioButtons("AVE_MED2", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                                                     choices=c("Mean","Median",'Total'), select='Median'))) 
        } else {
          tagList(
            radioButtons("AVE_MED2", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                         choices=c("Mean","Median",'Total'), select='Median'))   
        }

    } #end compare vessels
}
  })
###################################################
#End stat selection for non-economic metrics
###################################################
#tags$div(class='met_mod', radioButtons("MetricSelect","", choices ="Select a metric below"), style="font-style:italic;margin-bottom:20px;margin-top:-32px;margin-left:-15px;padding-top:0;")

#======================================
###################################################
#Layout select (Compare vessels or compare metrics)
###################################################
output$Layoutselect <- renderUI({
  
  tags$div(radioButtons("LayoutSelect", HTML("<div> Compare: <button id='icompare' type='button' class='btn btn-default action-button shiny-bound-input'>
                                             <i class='fa fa-info-circle fa-fw'></i></button></div>"),
                        if(input$Sect_sel=='CV'){ choices = c('Groups of Catcher Vessels','Metrics')}
                        else if(input$Sect_sel=='M'){ choices = c('Groups of Motherships','Metrics')} 
                        else if(input$Sect_sel=='CP'){choices = c('Groups of Vessels','Metrics')}
                        else if(input$Sect_sel=='FR'){choices = c('Groups of processors','Metrics')}
                        ,selected=choices[1], inline=T))
})

###################################################
#ENd layout select
###################################################

# Plot options

#======================================
output$Plotselect <- renderUI({
  tags$div(class="ckbox", checkboxInput("PlotSelect", HTML("<div> <span' style='font-weight:bold;font-size:12pt'>Show variance: </span>
                                        <span style='font-weight:normal;font-style:italic;font-size:10pt'>Shaded area represents 1SD about mean or 25th and 75th percentiles about median. </span>
                                      <button id='ivariance' type='button' class='btn btn-dafault action-button shiny-bound-input'> 
                                      <i class='fa fa-info-circle fa-fw'></i></button></div>"),
                                      TRUE))
})
output$Plotselectoption <- renderUI({
  tags$div(class="ckbox", radioButtons("PlotSelectOption", span("", style="font-weight:bold;font-size:12pt"),
                                                            choices=c("Standard deviation or Median average deviation","Percentiles (25th and 75th)"), selected='Standard deviation or Median average deviation'))
})


#===============text ==========================================#
output$SelectText <- renderText ({ 
  if(input$CategorySelect!="Fisheries"){
    if(input$Sect_sel=="CV"){
    HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
         <b>Show data summed across these fisheries: </b><button id='isummed' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></div>")
  } else if(input$Sect_sel=="FR"){
    HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
         <b>Show data summed across these production categories: </b><button id='isummed' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></div>")
  }
    }#else  if(input$CategorySelect=="Fisheries"){
})

observeEvent(input$reset_input, {
  if(input$LayoutSelect=="Metrics") {
    updateRadioButtons(session, "VariableSelect", selected="")  
  } else{
    updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0))
  }
})

