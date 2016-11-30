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
                                           c('Catcher Vessels'="CV", 'Mothership Vessels'="M", 'Catcher Processor Vessels'="CP", 'First Receivers and Shorebased Processors'="FR"
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
             'West Coast Trawl Catch Share Program:  Catcher Processor Vessels'
           } else if(input$Sect_sel=="FR"){
             'West Coast Trawl Catch Share Program:  First Receivers and Shorebased Processors'
           }
  )
})
###################################################
#End
###################################################


output$moreOptions <- renderUI({
  tags$div(class="ckbox", checkboxInput("moreOptions","Click to select additional years", value = FALSE))
})

########################################################
##YEAR
########################################################

output$YearSelect <- renderUI({
  if(input$LayoutSelect=='Metrics'){
    if(input$Sect_sel=="FR"){  
      tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
    } else {
      tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:12], selected = DatVars()$YEAR[6:12]))
  }} else {
    if(input$Ind_sel=="Demographic"){
    if(input$demSelect!='Gini coefficient'&input$demSelect!='Number of vessels'& input$demSelect!='Vessel length'){
      if(input$Sect_sel=="FR"){  
        tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
      } else {
        tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:12], selected = DatVars()$YEAR[6:12]))
      }
    } else{
      if(input$moreOptions=="FALSE"){
        if(input$Sect_sel=="FR"){  
          tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
        } else {
          tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:12], selected = DatVars()$YEAR[6:12]))
        }
    } else{
      if(input$Sect_sel=="FR"){  
        tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11], inline=T))
      } else {
        tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR, selected = DatVars()$YEAR[6:12], inline=T))
      }
    }}
    } else if(input$Ind_sel=="Social and Regional"){ 
    if(input$socSelect!='Seasonality'&input$socSelect!='Share of landings by state'){
      if(input$Sect_sel=="FR"){  
        tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
      } else {
        tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:12], selected = DatVars()$YEAR[6:12]))
      }
    } else{
      if(input$moreOptions=="FALSE"){
        if(input$Sect_sel=="FR"){  
          tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
        } else {
          tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:12], selected = DatVars()$YEAR[6:12]))
        }
    } else{
      if(input$Sect_sel=="FR"){  
        tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11], inline=T))
      } else {
        tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR, selected = DatVars()$YEAR[6:12], inline=T))
      }
    }}
    } else if(input$Ind_sel=='Economic'){
#    if(input$ShortdescrSelect[1]!="Revenue"){
      if(!input$ShortdescrSelect[1] %in% "Revenue"){
        if(input$Sect_sel=="FR"){  
          tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
        } else {
          tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:12], selected = DatVars()$YEAR[6:12]))
        }
    } 
    else {
      if(input$moreOptions=="FALSE"){
        if(input$Sect_sel=="FR"){  
          tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
        } else {
          tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:12], selected = DatVars()$YEAR[6:12]))
        }
    } else{
      if(input$Sect_sel=="FR"){  
        tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[1:11], selected = DatVars()$YEAR[6:11], inline=T))
      } else {
        tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR, selected = DatVars()$YEAR[6:12], inline=T))
      }
    }
    }}
}
})  
#END YEAR
##############################################

fish.var <- c("All fisheries combined"="All fisheries"," All Catch Share fisheries combined"="All Catch Share fisheries","At-sea Pacific whiting","Shoreside Pacific whiting",
              "DTS trawl with trawl endorsement","Non-whiting midwater trawl","Non-whiting, non-DTS trawl with trawl endorsement",  "Groundfish fixed gear with trawl endorsement",
              "All non-Catch Share fisheries combined"="All non-Catch Share fisheries", "Groundfish fixed gear with fixed gear endorsement","Crab","Shrimp","Other fisheries")
###################################################
#CATEGORY SELECT
###################################################
output$CategorySelect <- renderUI({
  if(input$Sect_sel!="FR"){
  tags$div(class="ckbox", radioButtons("CategorySelect", "Group vessels according to:", choices = DatVars()$CATEGORY, selected=DatVars()$CATEGORY[1]))
} else {  tags$div(class="ckbox", radioButtons("CategorySelect", "Group processors according to:", choices = DatVars()$CATEGORY, selected=DatVars()$CATEGORY[1]))
}
  })
###################################################
# End Category selecto
###################################################

###################################################
#INDICATOR SELECT - DEMOGRAPHIC, ECONOMIC, SOCIAL
###################################################
output$IndicatorSelect <- renderUI({
  #  tagList(
  selectInput("Ind_sel", HTML("<div> Select an indicator category: <button id='ipo' type='button' class='btn btn-default action-button shiny-bound-input'> 
                              <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), c('Demographic',"Economic","Social and Regional"), selected='Demographic',selectize=T)#,
})  #END INDICATOR SELECT
###################################################
#End indicator select
###################################################

###################################################
##METRIC SELECT
###################################################
output$MetricSelect <- renderUI({ 
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
output$VariableSelect <- renderUI({  
  if(!is.null(input$CategorySelect)){
    if(input$Sect_sel=="M"|input$Sect_sel=="CP"){
      tags$div(class="ckbox2", checkboxGroupInput("VariableSelect","",choices="At-sea Pacific whiting", selected=""))
    } 
    else if(input$Sect_sel=="CV"){
      if(input$CategorySelect == "State"){
        if(input$LayoutSelect!='Metrics'){
          #if(input$Ind_sel=="Social and Regional"){
             if(input$Ind_sel=='Demographic'||input$Ind_sel=='Economic'||input$Ind_sel=='Social and Regional'&input$socSelect!="Share of landings by state"){
                       tagList(           
                        tags$div(class="select", selectInput("inSelect","",c("All fisheries",  "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
                        checkboxGroupInput("VariableSelect", "Select one or more state:", choices = factorOrder$state, selected=""))
                    }
          else{
              tagList(           
              tags$div(class="select", selectInput("inSelect","",c("All fisheries",  "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              tags$div(radioButtons("VariableSelect", "Select one state:", choices = c(#"No state selected"="",
                factorOrder$state), selected="")))
       }}#} #end not metrics
        else {
            tagList(           
              tags$div(class="select", selectInput("inSelect","",c("All fisheries",  "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              tags$div(class="rbutton2",radioButtons("VariableSelect", "Select one state:", choices = c("No state selected"="",factorOrder$state), selected="")))
          }
      } else if(input$CategorySelect == "Vessel length class"){
        if(input$LayoutSelect!='Metrics'){
          if(input$Ind_sel=="Demographic"||input$Ind_sel=="Economic"||input$Ind_sel=="Social and Regional"&input$socSelect!="Share of landings by state"){
            tagList(           
            tags$div(class="select", selectInput("inSelect","",c("All fisheries", "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
            checkboxGroupInput("VariableSelect",  "Select one vessel length class:", choices=factorOrder$lengths, selected="")) 
          }  
          else {
           tagList(           
              tags$div(class="select", selectInput("inSelect","",c("All fisheries", "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              radioButtons("VariableSelect",  "Select one vessel length class:", choices=factorOrder$lengths, selected="")) 
          }}else {
            tagList(           
              tags$div(class="select", selectInput("inSelect","",c("All fisheries", "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              tags$div(class="rbutton2", radioButtons("VariableSelect",  "Select one vessel length class:", choices=c("No vessel length selected"="",factorOrder$lengths), selected=""))) 
          }
      } else if(input$CategorySelect == "Homeport"){
        if(input$LayoutSelect!="Metrics"){
          if(input$Ind_sel=="Demographic"||input$Ind_sel=="Economic"||input$Ind_sel=="Social and Regional"&input$socSelect!="Share of landings by state"){
            tagList(           
              tags$div(class="select", selectInput("inSelect","", c("All fisheries","All Catch Share fisheries","All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              tags$div(checkboxGroupInput("VariableSelect", div("Select one or more homeport:", style="margin-top:0; padding:-10px"), choices=factorOrder$port, selected="")))  
          }
          else {
            tagList(           
              tags$div(class="select", selectInput("inSelect","", c("All fisheries","All Catch Share fisheries","All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              tags$div(radioButtons("VariableSelect", div("Select one homeport:", style="margin-top:0; padding:-10px"), choices=factorOrder$port, selected=""))) 
          } 
          }else {
            tagList(           
              tags$div(class="select", selectInput("inSelect","", c("All fisheries","All Catch Share fisheries","All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              tags$div(class="rbutton2",radioButtons("VariableSelect", div("Select one homeport:", style="margin-top:0; padding:-10px"), 
                                                     choices=c("No homeport selected"="",factorOrder$port), selected=""))) 
          }
      } #end homeport
      else if(input$CategorySelect=="Fisheries"){
        if(input$LayoutSelect!='Metrics'){
          if(input$Ind_sel=="Economic"||input$Ind_sel=="Demographic"||input$Ind_sel=="Social and Regional" &input$socSelect[1]!="Share of landings by state"){
           tags$div(class="ckbox2", checkboxGroupInput("VariableSelect", div("Select one or more fisheries:", style="margin-top:0; padding:-10px"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                                        choices=c("All fisheries combined"="All fisheries"," All Catch Share fisheries combined"="All Catch Share fisheries",fish.var[3:13])))
          }
          else {
            tagList(
             tags$div(class="ckbox2", radioButtons("VariableSelect", div("Select one fisheries:", style="margin-top:0; padding:-10px"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                                  choices=c("All fisheries combined"="All fisheries"," All Catch Share fisheries combined"="All Catch Share fisheries",fish.var[3:13]), selected="")))
          }  
          }else {
            tags$div(class="ckbox3", radioButtons("VariableSelect", div("Select one fisheries:", style="margin-top:0; padding:-10px"), 
                                                  choices=c("No fishery selected"="","All fisheries combined"="All fisheries"," All Catch Share fisheries combined"="All Catch Share fisheries",fish.var[3:13]), selected=""))
          } 
      }#end fisheries
    } ##END Catcher Vessels
    else if(input$Sect_sel=="FR"){
      if(input$CategorySelect == "Region"){
        if(input$LayoutSelect!='Metrics'){
          if(input$Ind_sel=="Demographic"){
            if(input$demSelect!="Proportion of revenue from catch share species"){
            tagList(
              tags$div(class="select", selectInput("inSelect","",c("All production",  "Groundfish production", "Other species production")), style="margin-bottom:-10px"),
              tags$div(class="ckbox", checkboxGroupInput("VariableSelect", HTML("<div> Select one or more region:<button id='FRr' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"),
                                                         choices = c('Washington and Oregon','California'), selected="")))
          } else {
            tags$div(class="ckbox", checkboxGroupInput("VariableSelect", HTML("<div> Select one or more region:<button id='FRr' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"),
                                                       choices = c('Washington and Oregon','California'), selected="")) 
          }  
        } else if(input$Ind_sel=="Economic"){
            tagList(
              tags$div(class="select", selectInput("inSelect","",c("All production",  "Groundfish production", "Other species production")), style="margin-bottom:-10px"),
              tags$div(class="ckbox", checkboxGroupInput("VariableSelect", HTML("<div> Select one or more region:<button id='FRr' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"),
                                                       choices = c('Washington and Oregon','California'), selected="")))
            } else {
            tags$div(class="ckbox", checkboxGroupInput("VariableSelect", HTML("<div> Select one or more region:<button id='FRr' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"),
                                                         choices = c('Washington and Oregon','California'), selected="")) 
            }
          } #ENd compare processors
        else {
          if(input$Ind_sel!="Social and Regional"){
            tagList(
                  tags$div(class="select", selectInput("inSelect","",c("All production",  "Groundfish production", "Other species production")), style="margin-bottom:-10px"),
                  tags$div(class="rbutton2",radioButtons("VariableSelect", HTML("<div> Select one region:<button id='FRr' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"), choices = c("No region selected"="",
                                                        'Washington and Oregon','California'), selected=""))
                )
          } else {
            tags$div(class="rbutton2",radioButtons("VariableSelect", HTML("<div> Select one region:<button id='FRr' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"), choices = c("No region selected"="",
                                                                                                                             'Washington and Oregon','California'), selected="")) 
          }
          }
      } else if(input$CategorySelect =="Processor size"){
        if(input$LayoutSelect!='Metrics'){
          if(input$Ind_sel=="Demographic"){
            if(input$demSelect!="Proportion of revenue from catch share species"){
            tagList(
              tags$div(class="select", selectInput("inSelect","",c("All production",  "Groundfish production", "Other species production")), style="margin-bottom:-10px"),
              tags$div(checkboxGroupInput("VariableSelect",  HTML("<div> Select one size class:<button id='FRs' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"), choices=c("Large",'Medium','Small'), selected=""))
            )
          } else {
            tags$div(checkboxGroupInput("VariableSelect",  HTML("<div> Select one size class:<button id='FRs' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"), choices=c("Large",'Medium','Small'), selected=""))
          }
          } else if(input$Ind_sel=='Economic'){
            tagList(
              tags$div(class="select", selectInput("inSelect","",c("All production",  "Groundfish production", "Other species production")), style="margin-bottom:-10px"),
              tags$div(checkboxGroupInput("VariableSelect",  HTML("<div> Select one size class:<button id='FRs' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"), choices=c("Large",'Medium','Small'), selected=""))
            )  
          } else {
            tags$div(checkboxGroupInput("VariableSelect",  HTML("<div> Select one size class:<button id='FRs' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"), choices=c("Large",'Medium','Small'), selected=""))
          }
          }else {
            if(input$Ind_sel!="Social and Regional"){
                  tagList(
                    tags$div(class="select", selectInput("inSelect","",c("All production",  "Groundfish production", "Other species production")), style="margin-bottom:-10px"),
                    tags$div(class="rbutton2",radioButtons("VariableSelect",  HTML("<div> Select one size class:<button id='FRs' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button></div>"), choices=c("No processor size selected"="","Large",'Medium','Small'), selected=""))
                  )
            } else {                   
                    tags$div(class="rbutton2",radioButtons("VariableSelect",  HTML("<div> Select one size class:<button id='FRs' type='button' class='btn btn-default action-button shiny-bound-input'> 
                               <i class='fa fa-info-circle fa-fw'></i></button></div>"), choices=c("No processor size selected"="","Large",'Medium','Small'), selected=""))
            }
                  }
      } #End processor class
      else {
        if(input$LayoutSelect!='Metrics'){
          if(input$Ind_sel=="Demographic"|input$Ind_sel=="Economic"){
            tags$div(checkboxGroupInput("VariableSelect", HTML("<div> Select one or more production activities:<button id='FRi' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                       <i class='fa fa-info-circle fa-fw'></i></button> <style margin-top:0; padding:-10px> </style></div>"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                        choices=c("All production","Groundfish production","Pacific whiting production",'Non-whiting groundfish production','Other species production'), selected=""))
          }
          else if(input$Ind_sel=="Social and Regional"){
            tags$div(class='StatGrey3', checkboxGroupInput("VariableSelect", HTML("<div> Select one production activity:<button id='FRi' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                         <i class='fa fa-info-circle fa-fw'></i></button> <style margin-top:0; padding:-10px> </style></div>"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                  choices=c("All production","Groundfish production","Pacific whiting production",'Non-whiting groundfish production','Other species production'), selected=""))
         }}else {
           if(input$Ind_sel=="Social and Regional"){
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
output$FishAkSelect <- renderUI({
  tags$div(class="ckbox", checkboxInput("FishAkSelect", p("Include Alaskan fisheries activities: ", 
                                                          span("By selecting this, you will include data from their activities in Alaska.", style="font-style:italic;font-size:10pt")), value = TRUE))
})
###################################################
#end fishak
###################################################

###################################################
#Select whiting (data summed across category)
###################################################
output$FishWhitingSelect <- renderUI({
  tags$div(class="ckbox", checkboxGroupInput("FishWhitingSelect", "Show data summed across:", choices=DatVars()$whitingv, selected=DatVars()$whitingv[1]))
})
###################################################
#end fishak
###################################################

###################################################
#Economic measure select
###################################################
output$ShortdescrSelect <- renderUI({ 
  if(input$LayoutSelect=='Metrics'){
    tags$div(class="ckbox", checkboxGroupInput("ShortdescrSelect", HTML("<div> Economic measures:<button id='iem' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i> 
                                                                        </button></div>"), 
                                               choices = DatVars()$SHORTDESCR, selected = DatVars()$SHORTDESCR))
  } else {
    tags$div(class="ckbox", radioButtons("ShortdescrSelect", HTML("<div> Economic measures:<button id='iem' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i> 
                                                                  </button></div>"), 
                                         choices = DatVars()$SHORTDESCR, selected =DatVars()$SHORTDESCR[1]))
  }
})
###################################################
#End economic measure select
###################################################

###################################################
# Identify Individual metrics for demography
###################################################
output$demSelect <- renderUI({
  if(input$LayoutSelect=='Metrics'){
    if(input$Sect_sel=="CV"){
      if(input$AVE_MED2=="Total"){
      tags$div(class="ckboxCV",checkboxGroupInput("demSelect","", choices = c(DatVars()$METRIC[1:7]), selected=DatVars()$METRIC[1]))
      } else {
        tags$div(class="ckboxCV2",checkboxGroupInput("demSelect","", choices = c(DatVars()$METRIC[1:7]), selected=DatVars()$METRIC[2]))  
      }
    } else {
        if(input$AVE_MED2!="Total"){ 
          tags$div(class="ckboxCPFR",checkboxGroupInput("demSelect","", choices = c(DatVars()$METRIC[1:5]), selected=DatVars()$METRIC[2]))  
        }else {
          if(input$Sect_sel=='FR'){
          tags$div(class="ckboxFR",checkboxGroupInput("demSelect","", choices = c(DatVars()$METRIC[1:5]), selected=DatVars()$METRIC[1]))
        } else {
          tags$div(class="ckboxCP",checkboxGroupInput("demSelect","", choices = c(DatVars()$METRIC[1:5]), selected=DatVars()$METRIC[1]))
        }}}
    }
  else {
    if(input$Sect_sel=="CV"){
        tags$div(class="statbox",radioButtons("demSelect","", choices = c(DatVars()$METRIC[1:7]), selected=DatVars()$METRIC[1]))
      }else {
        tags$div(class="statbox",radioButtons("demSelect","", choices = c(DatVars()$METRIC[1:5]), selected=DatVars()$METRIC[1]))
      }}
})
###################################################
#end demography select
###################################################

###################################################
#Identify individual metrics for social and regional class
###################################################
output$socSelect <- renderUI({
    if(input$LayoutSelect=='Metrics'){
      if(input$Sect_sel=="CV"){
        if(input$AVE_MED2!="Total"){
        tags$div(class="ckbox",checkboxGroupInput("socSelect","", choices = c(DatVars()$METRIC[8:11]), selected=DatVars()$METRIC[8]))
       } else {
        tags$div(class="ckboxSOC",checkboxGroupInput("socSelect","", choices = c(DatVars()$METRIC[8:11]), selected=DatVars()$METRIC[8])) 
       }
    }else {
      if(input$AVE_MED2!="Total"){
    tags$div(class="ckbox",checkboxGroupInput("socSelect","", choices = c(DatVars()$METRIC[6:7]), selected=DatVars()$METRIC[6]))
      } else {
    tags$div(class="ckboxSOC",checkboxGroupInput("socSelect","", choices = c(DatVars()$METRIC[6:7]), selected=DatVars()$METRIC[6]))
  }
      }}else{
    if(input$Sect_sel=="CV"){
      tags$div(class="statbox",radioButtons("socSelect","", choices = c(DatVars()$METRIC[8:12]), selected=DatVars()$METRIC[8]))
    } else if(input$Sect_sel=="FR"){
      tags$div(class="statbox",radioButtons("socSelect","", choices = c(DatVars()$METRIC[6:7]), selected=DatVars()$METRIC[6]))
    }else {
      tags$div(class="statbox",radioButtons("socSelect","", choices = c(DatVars()$METRIC[6:7]), selected=DatVars()$METRIC[6]))
  }
  }
})
###################################################
#End
###################################################


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

###################################################
#Statistic select for economic measures
###################################################
#Being by select average, median, or total values
output$StatSelect <- renderUI({
  if(input$Sect_sel=="FR")  { 
    tagList(
      selectInput("AVE_MED", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                  c('Median, Average, or Total values'="", Average="A", Median="M", Total="T"), selectize=F),
      tags$div(class="statbox", radioButtons("StatSelect","",  choices = c(DatVars()$STAT[3:4]))))
  } else {
    tagList(
      selectInput("AVE_MED", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                  c('Median, Average, or Total values'="", Average="A", Median="M", Total="T"), selectize=F),
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
                   choices=c("Average","Median",'Total'), select='Average'))
  } #nd Metrics
    else {
      if(input$Ind_sel=="Demographic"){
        if(input$demSelect=="Number of vessels"|input$demSelect=="Number of processors"|input$demSelect=="Gini coefficient"){
          tagList(
            tags$div(class='StatGrey', radioButtons("AVE_MED2", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                         choices=c("Average","Median",'Total'), select='Total'))
            )
        } else if(input$demSelect=="Vessel length"|input$demSelect=="Exponential Shannon Index"|input$demSelect=="Fishery participation"){
          tagList(
            tags$div(class='StatGrey2', radioButtons("AVE_MED2", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                                                    choices=c("Average","Median",'Total'), select='Average'))
          )
        } else{
        tagList(
          radioButtons("AVE_MED2", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                       choices=c("Average","Median",'Total'), select='Average'))
      }
        } #End demographic
      else if(input$Ind_sel=="Social and Regional"){
        if(input$socSelect[1]=="Share of landings by state") { 
          tags$div(class='met_mod', radioButtons("AVE_MED2","", choices =""), style="margin-bottom:20px;margin-top:-32px;margin-left:-15px;padding-top:0;")
      } else if (input$socSelect == "Hourly compensation"| input$socSelect == "Crew wage per day"){
          tagList(
            tags$div(class='StatGrey2', radioButtons("AVE_MED2", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                         choices=c("Average","Median",'Total'), select='Average')))            
      } else {
        tagList(
          radioButtons("AVE_MED2", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                       choices=c("Average","Median",'Total'), select='Average'))            
        }
        } #End social and regional
    } #end compare vessels
})
###################################################
#End stat selection for non-economic metrics
###################################################
#tags$div(class='met_mod', radioButtons("MetricSelect","", choices ="Select a metric below"), style="font-style:italic;margin-bottom:20px;margin-top:-32px;margin-left:-15px;padding-top:0;")

#======================================
###################################################
#Layout select (Compare vessels or compare metrics)
###################################################
output$LayoutSelect <- renderUI({
  
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
output$PlotSelect <- renderUI({
  tags$div(class="ckbox", checkboxInput("PlotSelect", p(span("Plot options: ", style="font-weight:bold;font-size:12pt"),span("Show variance (standard deviation or median average deviation).", style="font-style:italic;font-size:10pt")), value=TRUE))
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
