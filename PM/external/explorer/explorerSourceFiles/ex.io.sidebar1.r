#======================================

# this page handles all of the reactive 
# expressions for the dynamic user interface

#======================================

output$moreOptions <- renderUI({
  tags$div(class="ckbox", checkboxInput("moreOptions","Click to select additional years", value = FALSE))
})

##YEAR
output$YearSelect <- renderUI({
  if(input$LayoutSelect=='Metrics'){
    tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
  } else {
    if(input$Ind_sel=="Demographic"){
    if(input$demSelect[1]!='Gini coefficient'&input$demSelect[1]!='Herfindahl-Hirschman Index'&input$demSelect[1]!='Number of vessels'& input$demSelect[1]!='Vessel length'){
      tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
    } else if(input$moreOptions=="FALSE"){
      tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:",choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
    } else{
      tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR, selected = DatVars()$YEAR[6:11], inline=T))
    }
    } else if(input$Ind_sel=="Social and Regional"){ 
    if(input$socSelect[1]!='Seasonality'&input$socSelect[1]!='Share of landings by state'){
      tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
    } else if(input$moreOptions=="FALSE"){
      tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:",choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
    } else{
      tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR, selected = DatVars()$YEAR[6:11], inline=T))
    }
    } else if(input$Ind_sel=='Economic'){
    if(input$ShortdescrSelect[1]!="Revenue"){
      tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
    } 
    else if(input$moreOptions=="FALSE"){
      tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:",choices = DatVars()$YEAR[6:11], selected = DatVars()$YEAR[6:11]))
    } else{
      tags$div(class="ckbox", checkboxGroupInput( "YearSelect","Years:", choices = DatVars()$YEAR, selected = DatVars()$YEAR[6:11], inline=T))
    }
    }
}
})  #END YEAR

fish.var <- c("All fisheries combined"="All fisheries"," All Catch Share fisheries combined"="All Catch Share fisheries","At-sea Pacific whiting","Shoreside Pacific whiting",
              "DTS trawl with trawl endorsement","Non-whiting midwater trawl","Non-whiting, non-DTS trawl with trawl endorsement",  "Groundfish fixed gear with trawl endorsement",
              "All non-Catch Share fisheries combined"="All non-Catch Share fisheries", "Groundfish fixed gear with fixed gear endorsement","Crab","Shrimp","Other fisheries")

#CATEGORY SELECT
output$CategorySelect <- renderUI({
  tags$div(class="ckbox", radioButtons("CategorySelect", "Group vessels according to:", choices = DatVars()$CATEGORY, selected=DatVars()$CATEGORY[1]))
})


#INDICATOR SELECT - DEMOGRAPHIC, ECONOMIC, SOCIAL
output$IndicatorSelect <- renderUI({
  #  tagList(
  selectInput("Ind_sel", HTML("<div> Select an indicator category: <button id='ipo' type='button' class='btn btn-default action-button shiny-bound-input'> 
                              <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), c('Demographic',"Economic","Social and Regional"), selected='Demographic',selectize=T)#,
})  #END INDICATOR SELECT


##METRIC SELECT
output$MetricSelect <- renderUI({ 
  if(input$LayoutSelect!="Metrics"){
    if(input$Ind_sel=="Economic") {
      radioButtons("MetricSelect","", choices ="Select an economic measure and statistic below")
    } #end economic
    else# if(input$Ind_sel=="Demographic")
      {
      radioButtons("MetricSelect","", choices ="Select a metric below")
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
      radioButtons("MetricSelect","", choices ="Select an economic measure and statistic below")
    }
    else #if(input$Ind_sel=="Demographic")
      {
      # -------> MODIFY THIS PART <--------#
      radioButtons("MetricSelect","", choices ="Select a metric below") #NEED TO MODIFY THIS!!!!!!!!!!!!!!
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



##VARIABLE SELECT
output$VariableSelect <- renderUI({  
  if(!is.null(input$CategorySelect)){
    if(input$Sect_sel=="M"|input$Sect_sel=="CP"){
      tags$div(class="ckbox2", checkboxGroupInput("VariableSelect","",choices="At-sea Pacific whiting", selected=""))
    } 
    else if(input$Sect_sel=="CV"){
      if(input$CategorySelect == "State"){
        if(input$LayoutSelect!='Metrics'){
          #if(input$Ind_sel=="Social and Regional"){
             if(input$Ind_sel=='Demographic'|input$Ind_sel=='Economic'){#input$Ind_sel=='Social and Regional'&input$socSelect!="Share of landings by state"){
                       tagList(           
                        tags$div(class="select", selectInput("inSelect","",c("All fisheries",  "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
                        checkboxGroupInput("VariableSelect", "Select one or more state:", choices = factorOrder$state, selected=""))
                    }#} 
          else if(input$Ind_sel=='Social and Regional'){
            if(input$socSelect!="Share of landings by state"){
            tagList(           
              tags$div(class="select", selectInput("inSelect","",c("All fisheries",  "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              checkboxGroupInput("VariableSelect", "Select one or more state:", choices = factorOrder$state, selected=""))
          } 
          else{
                    tagList(           
              tags$div(class="select", selectInput("inSelect","",c("All fisheries",  "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              tags$div(class="rbutton2",radioButtons("VariableSelect", "Select one state:", choices = c(#"No state selected"="",
                factorOrder$state), selected="")))
       }}}  else {
            tagList(           
              tags$div(class="select", selectInput("inSelect","",c("All fisheries",  "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              tags$div(class="rbutton2",radioButtons("VariableSelect", "Select one state:", choices = c("No state selected"="",factorOrder$state), selected="")))
          }
      } else if(input$CategorySelect == "Vessel length class"){
        if(input$LayoutSelect!='Metrics'){
          if(input$Ind_sel=="Demographic"|input$Ind_sel=="Economic"){
            tagList(           
            tags$div(class="select", selectInput("inSelect","",c("All fisheries", "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
            checkboxGroupInput("VariableSelect",  "Select one vessel length class:", choices=factorOrder$lengths, selected="")) 
          }  
          else if (input$Ind_sel=="Social and Regional"){
          if(input$socSelect=="Share of landings by state"){
           tagList(           
              tags$div(class="select", selectInput("inSelect","",c("All fisheries", "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              radioButtons("VariableSelect",  "Select one vessel length class:", choices=factorOrder$lengths, selected="")) 
          } else {
               tagList(           
              tags$div(class="select", selectInput("inSelect","",c("All fisheries", "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              checkboxGroupInput("VariableSelect",  "Select one or more vessel length class:", choices=factorOrder$lengths, selected="")) 
         }}
          }else {
            tagList(           
              tags$div(class="select", selectInput("inSelect","",c("All fisheries", "All Catch Share fisheries", "All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              radioButtons("VariableSelect",  "Select one vessel length class:", choices=factorOrder$lengths, selected="")) 
          }
      } else if(input$CategorySelect == "Homeport"){
        if(input$LayoutSelect!="Metrics"){
          if(input$Ind_sel=="Demographic"|input$Ind_sel=="Economic"){
            tagList(           
              tags$div(class="select", selectInput("inSelect","", c("All fisheries","All Catch Share fisheries","All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              tags$div(checkboxGroupInput("VariableSelect", div("Select one or more homeport:", style="margin-top:0; padding:-10px"), choices=factorOrder$port, selected="")))  
          }
          else if(input$Ind_sel=="Social and Regional"){
            if(input$socSelect=="Share of landings by state"){
            tagList(           
              tags$div(class="select", selectInput("inSelect","", c("All fisheries","All Catch Share fisheries","All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              tags$div(radioButtons("VariableSelect", div("Select one homeport:", style="margin-top:0; padding:-10px"), choices=factorOrder$port, selected=""))) 
          } else {
            tagList(           
              tags$div(class="select", selectInput("inSelect","", c("All fisheries","All Catch Share fisheries","All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              tags$div(checkboxGroupInput("VariableSelect", div("Select one or more homeport:", style="margin-top:0; padding:-10px"), choices=factorOrder$port, selected=""))) 
          }}
          }else {
            tagList(           
              tags$div(class="select", selectInput("inSelect","", c("All fisheries","All Catch Share fisheries","All non-Catch Share fisheries")), style="margin-bottom:-10px"),
              tags$div(radioButtons("VariableSelect", div("Select one homeport:", style="margin-top:0; padding:-10px"), choices=factorOrder$port, selected=""))) 
          }
      } #end homeport
      else if(input$CategorySelect=="Fisheries"){
        if(input$LayoutSelect!='Metrics'){
          if(input$Ind_sel=="Economic"|input$Ind_sel=="Demographic"){
           tags$div(class="ckbox2", checkboxGroupInput("VariableSelect", div("Select one or more fisheries:", style="margin-top:0; padding:-10px"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                                        choices=c("All fisheries combined"="All fisheries"," All Catch Share fisheries combined"="All Catch Share fisheries",fish.var[3:13])))
          }
          else if(input$Ind_sel=="Social and Regional"){
            if(input$socSelect[1]=="Share of landings by state"){
            tagList(
             tags$div(class="ckbox2", radioButtons("VariableSelect", div("Select one fisheries:", style="margin-top:0; padding:-10px"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                                  choices=c("All fisheries combined"="All fisheries"," All Catch Share fisheries combined"="All Catch Share fisheries",fish.var[3:13]), selected="")))
          } else {
             tags$div(class="ckbox2", checkboxGroupInput("VariableSelect", div("Select one or more fisheries:", style="margin-top:0; padding:-10px"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                                          choices=c("All fisheries combined"="All fisheries"," All Catch Share fisheries combined"="All Catch Share fisheries",fish.var[3:13])))
          }} 
          }else {
            tags$div(class="ckbox2", radioButtons("VariableSelect", div("Select one fisheries:", style="margin-top:0; padding:-10px"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                                  choices=c("All fisheries combined"="All fisheries"," All Catch Share fisheries combined"="All Catch Share fisheries",fish.var[3:13]), selected=""))
          } 
      }#end fisheries
    } ##END Catcher Vessels
    else if(input$Sect_sel=="FR"){
      if(input$CategorySelect == "State"){
        if(input$LayoutSelect!='Metrics'){
          if(input$Ind_sel=="Economic"|input$Ind_sel=="Demographic"){
            checkboxGroupInput("VariableSelect", "Select one or more state:", choices = c('Washington','Oregon','California','Multi-state'), selected="")
          }
          else if(input$Ind_sel=="Social and Regional"){
            if(input$socSelect=="Share of landings by state"){
            tags$div(class="rbutton2",radioButtons("VariableSelect", "Select one state:", choices = c(#"No state selected"="",
              'Washington','Oregon','California','Multi-state'), selected=""))
          } else {
            checkboxGroupInput("VariableSelect", "Select one or more state:", choices = c('Washington','Oregon','California','Multi-state'), selected="")
          }}}else {
            tags$div(class="rbutton2",radioButtons("VariableSelect", "Select one state:", choices = c(#"No state selected"="",
              'Washington','Oregon','California','Multi-state'), selected=""))
          }
      } else if(input$CategorySelect == "Vessel length class"){
        if(input$LayoutSelect!='Metrics'){
          if(input$Ind_sel=="Demographic"|input$Ind_sel=="Economic"){
            checkboxGroupInput("VariableSelect",  "Select one size class:", choices=c("Large",'Medium','Small'), selected="")
          }
          else if(input$Ind_sel=="Social and Regional"){
            if(input$socSelect=="Share of landings by state"){
            radioButtons("VariableSelect",  "Select one size class:", choices=c("Large",'Medium','Small'), selected="")
          } else {
            checkboxGroupInput("VariableSelect",  "Select one or more size classes:", choices=c("Large",'Medium','Small'), selected="")
          }}}else {
            radioButtons("VariableSelect",  "Select one size class:", choices=c("Large",'Medium','Small'), selected="")
          }
      } #End Vessel length class
      else if(input$CategorySelect=="Fisheries"){
        if(input$LayoutSelect!='Metrics'){
          if(input$Ind_sel=="Demographic"|input$Ind_sel=="Economic"){
            tags$div(checkboxGroupInput("VariableSelect", div("Select one or more fisheries:", style="margin-top:0; padding:-10px"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                        choices=c("All fisheries combined"="All fisheries","Pacific whiting",'Non-whiting groundfish',"Other fisheries"), selected=""))
          }
          else if(input$Ind_sel=="Social and Regional"){
            if(input$socSelect=="Share of landings by state"){
             tags$div(radioButtons("VariableSelect", div("Select one fisheries:", style="margin-top:0; padding:-10px"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                  choices=c("All fisheries combined"="All fisheries","Pacific whiting",'Non-whiting groundfish',"Other fisheries"), selected=""))
          } else {
            tags$div(checkboxGroupInput("VariableSelect", div("Select one or more fisheries:", style="margin-top:0; padding:-10px"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                        choices=c("All fisheries combined"="All fisheries","Pacific whiting",'Non-whiting groundfish',"Other fisheries"), selected=""))
         }}}else {
            tags$div(radioButtons("VariableSelect", div("Select one fisheries:", style="margin-top:0; padding:-10px"),#HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                  choices=c("All fisheries combined"="All fisheries","Pacific whiting",'Non-whiting groundfish',"Other fisheries"), selected=""))
          }
      } #end fisheries 
    } #End FR
  }#end not null 
  else return ()
}) #END VARIABLE SELECT



output$FishAkSelect <- renderUI({
  tags$div(class="ckbox", checkboxInput("FishAkSelect", p("Include Alaskan fisheries activities: ", 
                                                          span("By selecting this, you will include data from their activities in Alaska.", style="font-style:italic;font-size:10pt")), value = TRUE))
})

output$FishWhitingSelect <- renderUI({
  tags$div(class="ckbox", checkboxGroupInput("FishWhitingSelect", "Show data summed across:", choices=DatVars()$whitingv, selected=DatVars()$whitingv[1]))
})

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
####TESTING##############################
output$demSelect <- renderUI({
  if(input$LayoutSelect=='Metrics'){
    if(input$Sect_sel=="CV"){
      tags$div(class="ckbox",checkboxGroupInput("demSelect","", choices = c(DatVars()$METRIC[1:8]), selected=DatVars()$METRIC[1]))
    }else {
      tags$div(class="ckbox",checkboxGroupInput("demSelect","", choices = c(DatVars()$METRIC[1:6]), selected=DatVars()$METRIC[1]))
    }}
  else {
    if(input$Sect_sel=="CV"){
        tags$div(class="statbox",radioButtons("demSelect","", choices = c(DatVars()$METRIC[1:8]), selected=DatVars()$METRIC[1]))
      }else {
        tags$div(class="statbox",radioButtons("demSelect","", choices = c(DatVars()$METRIC[1:6]), selected=DatVars()$METRIC[1]))
      }}
})

output$socSelect <- renderUI({
    if(input$LayoutSelect=='Metrics'){
      if(input$Sect_sel=="CV"){
    tags$div(class="ckbox",checkboxGroupInput("socSelect","", choices = c(DatVars()$METRIC[9:12]), selected=DatVars()$METRIC[9]))
  } else if(input$Sect_sel=="FR"){
    tags$div(class="ckbox",checkboxGroupInput("socSelect","", choices = c(DatVars()$METRIC[7:8]), selected=DatVars()$METRIC[7]))
  }else {
    tags$div(class="ckbox",checkboxGroupInput("socSelect","", choices = c(DatVars()$METRIC[7:10]), selected=DatVars()$METRIC[7]))
  }
      }else{
    if(input$Sect_sel=="CV"){
      tags$div(class="statbox",radioButtons("socSelect","", choices = c(DatVars()$METRIC[9:13]), selected=DatVars()$METRIC[9]))
    } else if(input$Sect_sel=="FR"){
      tags$div(class="statbox",radioButtons("socSelect","", choices = c(DatVars()$METRIC[7:9]), selected=DatVars()$METRIC[7]))
    }else {
      tags$div(class="statbox",radioButtons("socSelect","", choices = c(DatVars()$METRIC[7:10]), selected=DatVars()$METRIC[7]))
  }
  }
})
    

## --------------------------- Sector Select ------------------------------------------##
output$SectorSelect <- renderUI({
  tags$div(class="sectselect", selectInput("Sect_sel", span("West Coast Trawl Catch Share Program:", style="font-size:110%;font-style:italic; padding:5px; display:inline-block;vertical-align:middle"), 
                                           c('Catcher Vessels'="CV", 'Mothership Vessels'="M", 'Catcher Processor Vessels'="CP", 'First Receivers and Shorebased Processors'="FR"), width='35%')
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

## Modify which stats are shown
output$StatSelect2 <- renderUI({
  tagList(
    radioButtons("AVE_MED2", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                 if(input$LayoutSelect=='Metrics'){
                   choices=c("Average","Median", "Total or index value"='Total')
                 } else{
                   if(input$Ind_sel=="Demographic"){
                     choices=c("Average", "Median", "Total or index value"='Total') 
                   }
                   else if(input$Ind_sel=="Social and Regional"){
                     if(input$socSelect!="Share of landings by state") { 
                       choices=c("Average", "Median", "Total or index value"='Total') 
                 }else {
                   choices=c("")
                 }}
                 }
                 , selected=choices[1])
  )
})

#======================================
output$LayoutSelect <- renderUI({
  
  tags$div(radioButtons("LayoutSelect", HTML("<div> Compare: <button id='icompare' type='button' class='btn btn-default action-button shiny-bound-input'>
                                             <i class='fa fa-info-circle fa-fw'></i></button></div>"),
                        if(input$Sect_sel=='CV'){ choices = c('Groups of Catcher Vessels','Metrics')}
                        else if(input$Sect_sel=='M'){ choices = c('Groups of Motherships','Metrics')} 
                        else if(input$Sect_sel=='CP'){choices = c('Groups of Vessels','Metrics')}
                        else if(input$Sect_sel=='FR'){choices = c('Groups of Vessels','Metrics')}
                        ,selected=choices[1], inline=T))
})



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
})

observeEvent(input$reset_input, {
  if(input$LayoutSelect=="Metrics") {
    updateRadioButtons(session, "VariableSelect", selected="")  
  } else{
    updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0))
  }
})
