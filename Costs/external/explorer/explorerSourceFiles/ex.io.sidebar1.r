#
#======================================
#
# this page handles all of the reactive 
# expressions for the dynamic user interface
# These reactive expressions are then called in ex.reactives.R 
#======================================


###################################################
## --------------------------- Sector Select ------------------------------------------##
###################################################
output$Sectorselect <- renderUI({
  tags$div(class="sectselect", selectInput("Sect_sel", span("Select a sector:", style="font-style:strong;display:inline-block;vertical-align:middle"), 
                                           c('Catcher Vessels'="CV", 'Mothership Vessels'="M", 'Catcher-Processor Vessels'="CP", 'First Receivers and Shorebased Processors'="FR"), 
                                           "CV", width='90%')
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
           } else {
             'West Coast Trawl Catch Share Program:  First Receivers and Shorebased Processors'
           }
  )
})
###################################################


observeEvent(input$reset_input, {
    updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0))
})


##############################################################################
#-----------------Costs Meaures--------------------------------------------
output$CostCategoryselect <- renderUI({
   selectInput("CostCategorySelect",'Cost categories:',#HTML("<div> Cost categories:<button id='iem' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                                      #<i class='fa fa-info-circle fa-fw' ></i> </button></div>"),
               c("All cost categories", "Variable costs", "Fixed costs"), 
               selected='All cost categories',selectize=F)
})
output$Shortdescrselect <- renderUI({
#  if(input$CostCategorySelect=="All cost categories"){
    if(input$Sect_sel=='CV'){
#      if(input$PlotSelect!='Stacked bar'){
#        tags$div(class="statboxC", checkboxGroupInput("ShortdescrSelect",'Cost categories:',
#                                             choices = DatVars()$SHORTDESCR,selected = DatVars()$SHORTDESCR))
#      } else {
   if(input$tabs=="Panel1"){        
     tags$div(class="statboxC", checkboxGroupInput("ShortdescrSelect",'Cost categories:',
                                                      choices = DatVars()$SHORTDESCR,selected = c('All fixed costs','All variable costs')))
   } else {
        tags$div(class="statboxC", checkboxGroupInput("ShortdescrSelect",'Cost categories:',
                                                      choices = DatVars()$SHORTDESCR, selected = DatVars()$SHORTDESCR[c(2:8)])) 
      }
      
#      }
    } else if(input$Sect_sel=='FR'){
#      if(input$PlotSelect!='Stacked bar'){
#        tags$div(class="statboxF", checkboxGroupInput("ShortdescrSelect",'Cost categories:',
#                                                      choices = DatVars()$SHORTDESCR,selected = DatVars()$SHORTDESCR))
#      } else{
      tags$div(class="statboxF", checkboxGroupInput("ShortdescrSelect",'Cost categories:',
                                                   choices = DatVars()$SHORTDESCR,selected = c('All fixed costs','All variable costs')))
#    }
      }else if(input$Sect_sel=='M'){
#        if(input$PlotSelect!='Stacked bar'){
#          tags$div(class="statboxM", checkboxGroupInput("ShortdescrSelect",'Cost categories:',
#                                                        choices = DatVars()$SHORTDESCR,selected = DatVars()$SHORTDESCR))
#        } else {
          tags$div(class="statboxM", checkboxGroupInput("ShortdescrSelect",'Cost categories:',
                                                    choices = DatVars()$SHORTDESCR,selected = c('All fixed costs', 'All variable costs')))
#        }
    } else {
#      if(input$PlotSelect!='Stacked bar'){
#        tags$div(class="statboxM", checkboxGroupInput("ShortdescrSelect",'Cost categories:',
#                                                      choices = DatVars()$SHORTDESCR,selected = DatVars()$SHORTDESCR))
#      } else{
      tags$div(class="statboxM", checkboxGroupInput("ShortdescrSelect",'Cost categories:',
                                                   choices = DatVars()$SHORTDESCR,selected =  c('All fixed costs', 'All variable costs')))
      }
#      }
#}  else if(input$CostCategorySelect=="Variable costs"){
#  if(input$Sect_sel=='CV'|input$Sect_sel=='FR'){
#    tags$div(class="statbox", checkboxGroupInput("ShortdescrSelect","",
#                                                 choices = DatVars()$SHORTDESCR[1:8],selected = DatVars()$SHORTDESCR[1:8]))
#  } else if(input$Sect_sel=='M'){
#    tags$div(class="statbox", checkboxGroupInput("ShortdescrSelect","",
#                                                 choices = DatVars()$SHORTDESCR[1:7],selected = DatVars()$SHORTDESCR[1:7]))
#  } else {
#    tags$div(class="statbox", checkboxGroupInput("ShortdescrSelect","",
#                                                 choices = DatVars()$SHORTDESCR[1:6],selected = DatVars()$SHORTDESCR[1:6]))
    
#  }
#} else if(input$CostCategorySelect=="Fixed costs"){
#  if(input$Sect_sel=="CV"){
#    tags$div(class="statbox", checkboxGroupInput("ShortdescrSelect","",
#                                                choices = DatVars()$SHORTDESCR[9:12],selected = DatVars()$SHORTDESCR[9:12]))
#  } else if(input$Sect_sel=="FR"){
#    tags$div(class="statbox", checkboxGroupInput("ShortdescrSelect","",
#                                                 choices = DatVars()$SHORTDESCR[9:11],selected = DatVars()$SHORTDESCR[9:11]))
#  } else if(input$Sect_sel=='M'){
#    tags$div(class="statbox", checkboxGroupInput("ShortdescrSelect","",
#                                                 choices = DatVars()$SHORTDESCR[8:12],selected = DatVars()$SHORTDESCR[8:12]))
#  } else{    
#    tags$div(class="statbox", checkboxGroupInput("ShortdescrSelect","",
#                                                          choices = DatVars()$SHORTDESCR[7:11],selected = DatVars()$SHORTDESCR[7:11]))
#  }
#}
})
################################################################################

########################################################################################
#----------------Year Select -------------------------------------------------
######################################################################################
#HTML("<div> Statistic:<i class='fa fa-info fa-fw' style='font-size:12px; color:blue'></i></div>")
output$Yearselect <- renderUI({
  tags$div(class="ckbox", checkboxGroupInput( "YearSelect", "Years:", choices = DatVars()$YEAR, selected = DatVars()$YEAR))
})
output$Yearselect2 <- renderUI({
  tags$div(class="ckbox", checkboxGroupInput( "YearSelect2", "Years:", choices = DatVars()$YEAR, selected = DatVars()$YEAR))
})

###################################################################################


##################################################################################
#-------------------Category Select
#################################################################################
output$Categoryselect <- renderUI({
  if(input$Sect_sel[1]=="FR"){ 
    tags$div(class="ckbox", radioButtons("CategorySelect", "Group processors according to:", choices = DatVars()$CATEGORY, selected=DatVars()$CATEGORY[1]))
  } else if(input$Sect_sel[1]!="CV"){
    tags$div(class="ckbox", radioButtons("CategorySelect", "Group vessels according to:", choices = DatVars()$CATEGORY, selected=DatVars()$CATEGORY[1]))
  } else {
    if(input$tabs=='Panel1'){
    tags$div(class="ckbox", radioButtons("CategorySelect", "Group vessels according to:", choices = DatVars()$CATEGORY, selected=DatVars()$CATEGORY[1])) 
    } else {
    tags$div(class="ckbox", radioButtons("CategorySelect", "Group vessels according to:", choices = DatVars()$CATEGORY, selected=DatVars()$CATEGORY[3]))
    } 
  }
#  tags$div(class="ckbox", radioButtons("CategorySelect", "Group vessels according to:", choices = DatVars()$CATEGORY))
})
#################################################################################


fish.var <- c("All fisheries combined"="All fisheries"," All catch share fisheries combined"="All catch share fisheries","Pacific whiting","At-sea Pacific whiting","Shoreside Pacific whiting",
              "Groundfish with trawl gear","DTS trawl with trawl endorsement","Non-whiting, non-DTS trawl with trawl endorsement",  "Non-whiting midwater trawl","Groundfish fixed gear with trawl endorsement",
              "All non-catch share fisheries combined"="All non-catch share fisheries", "Groundfish fixed gear with fixed gear endorsement","Crab","Shrimp")



#################################################################################
##-----------Variable Select#
##################################################################################
output$Variableselect <- renderUI({  
    if(!is.null(input$CategorySelect)){
      if(input$CategorySelect == "State"){
        tagList(           
if(input$tabs=='Panel1'){
  tagList(           
    tags$div(class="select", selectInput("inSelect","",
                                         c("All fisheries","All catch share fisheries","All non-catch share fisheries")), style="margin-bottom:-10px"),
    checkboxGroupInput("VariableSelect", "Select one or more state:", choices = factorOrder$state, selected="")
  )
} else {
  tagList(           
    tags$div(class="select", selectInput("inSelect","",
                                         c("All fisheries","All catch share fisheries","All non-catch share fisheries"), selected='All catch share fisheries'), style="margin-bottom:-10px"),
    checkboxGroupInput("VariableSelect", "Select one or more state:", choices = factorOrder$state, selected=factorOrder$state)
  )  
}
        )
        } else if (input$CategorySelect=="Region"){
          tagList(           
            tags$div(class="select", selectInput("inSelect","",
                                                 c("All production","Groundfish production","Other species production")), style="margin-bottom:-10px"),
          checkboxGroupInput("VariableSelect", "Select one or more regions:", choices = c('Washington and Oregon','California'), selected="")
          )
        
       } else if(input$CategorySelect == "Vessel length class"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",
                                               c("All fisheries","All catch share fisheries","All non-catch share fisheries")), style="margin-bottom:-10px"),
          checkboxGroupInput("VariableSelect",  "Select one or more vessel length class:", choices=c("Large vessel (> 80 ft)","Medium vessel (> 60ft, <= 80ft)", "Small vessel (<= 60ft)"), selected="")
        )
      }else if(input$CategorySelect == "Processor size"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",
                                               c("All production","Groundfish production","Other species production")), style="margin-bottom:-10px"),
          checkboxGroupInput("VariableSelect",  "Select one or more processor size class:", choices=c('Large','Medium','Small'), selected=""))
      } else if(input$CategorySelect == "Homeport"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",
                                               c("All fisheries", "All catch share fisheries", "All non-catch share fisheries")), style="margin-bottom:-10px"),
          #$bsButton("selectall", "Select all", style="primary", size="extra-small",block=F, type="action"),
          tags$div(checkboxGroupInput("VariableSelect", div("Select one or more homeport:", style="margin-top:0; padding:-10px"), choices=factorOrder$port, selected=""))
        )
      } else if(input$CategorySelect=="Fisheries"){
        if(input$Sect_sel=="M"|input$Sect_sel=="CP"){
          tags$div(checkboxGroupInput("VariableSelect","",choices=c("At-sea Pacific whiting"), selected="")) 
        } else if(input$Sect_sel=="CV"){
        tagList(
          #              selectInput("fishCatSelect","", c("Catch share fisheries"="CSF", "Non-Catch Shares fisheries"="NSF", "All fisheries"="AF"), selected="AF"),
          actionButton("selectall2", "All fisheries", style="default", size="extra-small",block=F, type="action"),
          actionButton("selectallcs", "All catch share fisheries", style="default",size="extra-small", block=F, type="action"),
          actionButton("selectallncs", "All non-catch share fisheries", style="default", size="extra-small", block=F, type="action"),
          #  conditionalPanel("input.fishCatSelect==AF", 
          tags$div(class="ckbox2", checkboxGroupInput("VariableSelect", HTML("<div style='font-style:italic; font-size:10.87pt; font-weight:normal; margin-top:8.5pt'> 
                                                                             or select fisheries individually:  <button id='ivs' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
                                                      choices=c("All fisheries combined"="All fisheries"," All catch share fisheries combined"="All catch share fisheries",fish.var[3:14])#div(fish.var, stlye="font-style:bold")#fish.var
                                                      , selected=""))
          #tags$div(class="ckbox", checkboxGroupInput("ShortdescrSelect", HTML("<div> Economic measures:<button id='iem' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i> 
          
          )
      }}
        else if(input$CategorySelect=="Production activities"){
          tags$div(class='frckbox',checkboxGroupInput("VariableSelect",  'Select one or more production activities:',
                                                      choices=c("All production","Groundfish production","Pacific whiting production","Non-whiting groundfish production","Other species production")
                                                      , selected="")
          )
      } # end fisheries
    } else return ()
})
#############################################################################################################

##########################################################################################################
#-----------Select all buttons - for CV fisheries
##########################################################################################################
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
    updateCheckboxGroupInput(session,"VariableSelect", selected=fish.var[2:8])
  }
})

observe({
  if (is.null(input$selectallncs) || input$selectallncs == 0) return() 
  else if (input$selectallncs%%2 == 0) {
    updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0)) } 
  else {
    updateCheckboxGroupInput(session,"VariableSelect", selected=fish.var[9:12])
  }
})
##########################################################################################################


#############################################################
#--------------Fished in Alaskan fisheries---------------------
output$FishAkselect <- renderUI({
  tags$div(class="ckbox", checkboxInput("FishAkSelect", p("Include vessels that fished in Alaska: ", 
                                                          span("By selecting this, you will include vessels that also participated in Alaskan fisheries. 
                                                               Data from their activities in Alaska are not included.", style="font-style:italic;font-size:10pt")), 
                                        value = TRUE))
})
###############################################################

################################################################
#Select whiting (data summed across category)
###################################################
output$FishWhitingselect <- renderUI({
  if(input$tabs=='Panel1'){
  tags$div(class="ckbox", radioButtons("FishWhitingSelect",  HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>                                                                       </button></div>"), choices=DatVars()$whitingv, selected=DatVars()$whitingv[1]))
  } else {
    tags$div(class="ckbox", radioButtons("FishWhitingSelect",  HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>                                                                       </button></div>"), choices=DatVars()$whitingv, selected=DatVars()$whitingv[2]))
  }
})


#--------Exclude vessels that fished for whiting-------------------------
#output$FishWhitingselect <- renderUI({
#  tags$div(class="ckbox", checkboxInput("FishWhitingSelect", p("Include vessels that fished for Pacific whiting: ", 
#                                                               span("By selecting this, you will include vessels that also fished for Pacific whiting. 
#                                                                    Data from their activities are included if this box is selected.", style="font-style:italic;font-size:10pt")), 
#                                        value = TRUE))
#})
#################################################################

################################################################
#--------Select production categories-------------------------
#output$Productionselect <- renderUI({
#    selectInput("ProductionSelect", HTML("<div> Show data for: <button id='iprod' type='button' class='btn btn-default action-button shiny-bound-input'> 
#                                         <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
#                choices=c("All processors","Catch share processors"), selected="Catch share processors")
#})
##################################################################


##################################################################
#------------------Select statistics--------------------------
#output$Statselect <- renderUI({
#  tagList(
#    tags$div(radioButtons("StatSelect","Select a statistic:",  choices = c(DatVars()$STAT[1:3])))
#  )
#})


output$Statselect <- renderUI({
  if(input$Sect_sel=="FR")  { 
    tagList(
      selectInput("AVE_MED", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                  c('Median, Mean, or Total values'="", Mean="A", Median="M", Total="T"), selectize=F),
      tags$div(class="statbox", radioButtons("StatSelect","",  choices = c(DatVars()$STAT[4:6]))))
  } else if (input$Sect_sel=='CP'|input$Sect_sel=='M') {
    tagList(
      selectInput("AVE_MED", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                  c('Median, Mean, or Total values'="", Mean="A", Median="M", Total="T"), selectize=F),
      tags$div(class="statbox", radioButtons("StatSelect","",  choices = c(DatVars()$STAT[5:8]), selected=DatVars()$STAT[5])))
  }else {
    if(input$tabs=='Panel1'){
    tagList(
      selectInput("AVE_MED", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                  c('Median, Mean, or Total values'="", Mean="A", Median="M", Total="T"), selectize=F),
      tags$div(class="statbox", radioButtons("StatSelect","",  choices = c(DatVars()$STAT[5:8]), selected=DatVars()$STAT[5])))
    } else {
      tagList(
        selectInput("AVE_MED", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                    c('Median, Mean, or Total values'="", Mean="A", Median="M", Total="T"), selectize=F),
        tags$div(class="statbox", radioButtons("StatSelect","",  choices = c(DatVars()$STAT[5:8]), selected=DatVars()$STAT[6])))
    }
  }
})


#select whether to show values per vessel, /vessel/day, or /vessel/metric-ton
observe({
  if (is.null(input$AVE_MED)) {return()}
  else 
    if(input$Sect_sel=='CP'|input$Sect_sel=='M'){
      if(input$AVE_MED=="M"){
        updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[5:8]))
      }  else if(input$AVE_MED=="A"){
        updateRadioButtons(session,"StatSelect",   choices = c(DatVars()$STAT[1:4]))
      } else  if(input$AVE_MED=="T"){
        updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[9:12]))
      } 
    } else if(input$Sect_sel=='CV'){
      if(input$AVE_MED=="M"){
        updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[5:8]))
      }  else if(input$AVE_MED=="A"){
        updateRadioButtons(session,"StatSelect",   choices = c(DatVars()$STAT[1:4]))
      } else  if(input$AVE_MED=="T"){
        updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[9:12]))
      } 
    } else if(input$AVE_MED=="M"){
      updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[4:6]))
    }  else if(input$AVE_MED=="A"){
      updateRadioButtons(session,"StatSelect",   choices = c(DatVars()$STAT[1:3]))
    } else  if(input$AVE_MED=="T"){
      updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[7:9]))
    } 
})

################################################################################


##################################################
#-------------- Plot options------------------------------
output$Plotselect <- renderUI({
  if(input$tabs=='Panel1'){
      tags$div(class="ckbox", selectInput("PlotSelect", "Plot options:", choices= c("Bar", "Stacked bar", "Line"), selected='Line'))
  } else {
    tags$div(class="ckbox", selectInput("PlotSelect", "Plot options:", choices= c("Bar", "Stacked bar", "Line"), selected='Stacked bar'))
  }
})

##############################################################


####################################################################
#===============text ==========================================
output$SelectText <- renderText ({ 
  if(input$CategorySelect[1]!="Fisheries"&input$Sect_sel[1]=="CV"){
    HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
                                       <b>Show data summed across these fisheries: </b><button id='isummed' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></div>")
  } else  if(input$Sect_sel=="CV"&input$CategorySelect=="Fisheries"){
    HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:20px; margin-bottom:-25px;'>
                                       <b>Select fisheries:</b> 
                                       <h5><i>Select a fishery group,  </i>
       <button id='ifg' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></h5></div>")#,
  }else if(input$Sect_sel=="FR"&input$CategorySelect!="Production activities"){
    HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
                                       <b>Show data summed across these production categories: </b><button id='isummed' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></div>")
  }
  
})
###########################################################################

