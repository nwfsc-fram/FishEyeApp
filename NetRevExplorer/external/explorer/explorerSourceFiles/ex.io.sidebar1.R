#
#======================================

# this page handles all of the reactive 
# expressions for the dynamic user interface
# These reactive expressions are then called in ex.reactives.R 
#======================================

###This section commented out. Kept in case decide to use direct email form
#observe({
#  if(is.null(input$send) || input$send==0) return(NULL)
#  from <- isolate(input$from)
#  to <- "nwfsc.fisheye@noaa.gov"
#  subject <- isolate(input$subject)
#  msg <- paste(isolate(input$from), isolate(input$message))
#  sendmail("melanie.harsch@noaa.gov", subject, msg, password="rmail")
#})
###################################################
## --------------------------- Sector Select ------------------------------------------##
###################################################
output$SectorSelect <- renderUI({
  if(input$tabs=="Panel1"){
  tags$div(class="sectselect", 
           selectInput("Sect_sel", span("Select a sector:", 
                                       style="font-style:strong;display:inline-block; vertical-align:middle"), 
                                           c('Catcher Vessels'="CV", 'Mothership Vessels'="M", 
                                             'Catcher Processor Vessels'="CP", 
                                             'First Receivers and Shorebased Processors'="FR"),
                       selected='CV', width='90%')
          )
  } else {
    tags$div(class="sectselect", 
             selectInput("Sect_sel", span("Select a sector:", 
                                         style="font-style:strong;display:inline-block;vertical-align:middle"), 
                                             c('Catcher Vessels'="CV", 
                                               'First Receivers and Shorebased Processors'="FR"), width='90%')
          )
         }
})

output$SectPrint <- renderUI({
           if(input$Sect_sel=="CV") {
             span('West Coast Trawl Catch Share Program:  Catcher Vessels', 
                  style="font-size:210%;font-style:italic; padding:10px; padding-left:15px;
                  display:inline-block;vertical-align:middle")
           } else if(input$Sect_sel=="M"){
             span('West Coast Trawl Catch Share Program:  Mothership Vessels', 
                  style="font-size:210%;font-style:italic; padding:10px; padding-left:15px;
                  display:inline-block;vertical-align:middle")
           } else if(input$Sect_sel=="CP"){
             span('West Coast Trawl Catch Share Program:  Catcher Processor Vessels', 
                  style="font-size:210%;font-style:italic; padding:10px; padding-left:15px;
                  display:inline-block;vertical-align:middle")
           } else{
             span('West Coast Trawl Catch Share Program:  First Receivers and Shorebased Processors', 
                  style="font-size:210%;font-style:italic; padding:10px; padding-left:15px;
                  display:inline-block;vertical-align:middle")
           }
})
###################################################


observeEvent(input$reset_input, {
  if(input$tabs=="Panel2") {
    updateRadioButtons(session, "VariableSelect", selected="")  
  } else if(input$tabs=="Panel1"){
    updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0))
  }  
})

###################################################
#Layout select (Compare vessels or compare metrics)
###################################################
output$Layoutselect <- renderUI({
  tags$div(radioButtons("LayoutSelect", 
                            HTML("<div> Compare: <button id='icompare' type='button' 
                                              class='btn btn-default action-button shiny-bound-input'>
                                             <i class='fa fa-info-circle fa-fw'></i></button></div>"),
                        if(input$Sect_sel=='CV'){ choices = c('Groups of Catcher Vessels','Economic measures')}
                        else if(input$Sect_sel=='M'){ choices = c('Groups of Motherships','Economic measures')} 
                        else if(input$Sect_sel=='CP'){choices = c('Groups of Vessels','Economic measures')}
                        else if(input$Sect_sel=='FR'){choices = c('Groups of processors','Economic measures')}
                        ,selected=choices[1], inline=T))
})

###################################################
#ENd layout select
###################################################


##############################################################################
#-----------------Economic Meaures--------------------------------------------
output$Shortdescrselect <- renderUI({
  if(input$tabs=='Panel1'){
    if(input$DodgeSelect == 'Composition of Variable Cost Net Revenue'){
    tags$div(class="ckbox3", checkboxGroupInput("ShortdescrSelect",  
                                HTML("<div> Economic measures:<button id='iem' type='button' 
                                          class='btn btn-default action-button shiny-bound-input'> 
                                          <i class='fa fa-info-circle fa-fw' ></i> </button></div>"), 
                                               choices = c("Variable costs",  "Variable Cost Net Revenue"),
                                               selected = c("Variable costs", "Variable Cost Net Revenue")))
  }else if(input$DodgeSelect == 'Composition of Total Cost Net Revenue'){
    tags$div(class="ckbox3", checkboxGroupInput("ShortdescrSelect",
                                HTML("<div> Economic measures:<button id='iem' type='button' 
                                            class='btn btn-default action-button shiny-bound-input'> 
                                            <i class='fa fa-info-circle fa-fw' ></i> </button></div>"), 
                                              choices = c("Variable costs", "Fixed costs",  "Total Cost Net Revenue"),
                                              selected = c("Variable costs",  "Fixed costs","Total Cost Net Revenue")))
  }else if(input$DodgeSelect == 'Economic measures side-by-side') {
      tags$div(class="ckbox", checkboxGroupInput("ShortdescrSelect", HTML("<div> Economic measures:<button id='iem' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                                      <i class='fa fa-info-circle fa-fw' ></i> </button></div>"), 
                                                 choices = c("Revenue","Variable costs", "Fixed costs", "Variable Cost Net Revenue", "Total Cost Net Revenue"),
                                                 selected = c("Revenue", "Variable costs",  "Fixed costs", "Variable Cost Net Revenue","Total Cost Net Revenue")))
      
  }} else {
      if(input$LayoutSelect=='Economic measures'){
  tags$div(class="ckbox", checkboxGroupInput("ShortdescrSelect", HTML("<div> Economic measures:<button id='iem' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                                      <i class='fa fa-info-circle fa-fw' ></i> </button></div>"), 
                                             choices = c("Revenue","Variable costs", "Fixed costs", "Variable Cost Net Revenue", "Total Cost Net Revenue"),
                                              selected = c("Revenue", "Variable costs",  "Fixed costs", "Variable Cost Net Revenue","Total Cost Net Revenue")))
      } else {
        tags$div(class="ckbox", radioButtons("ShortdescrSelect", HTML("<div> Economic measures:<button id='iem' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                                                      <i class='fa fa-info-circle fa-fw' ></i> </button></div>"), 
                                                   choices = c("Revenue","Variable costs", "Fixed costs", "Variable Cost Net Revenue", "Total Cost Net Revenue"),
                                                   selected = c("Revenue")))
        
      }
        }
})
################################################################################

########################################################################################
#----------------Year Select -------------------------------------------------
######################################################################################
#HTML("<div> Statistic:<i class='fa fa-info fa-fw' style='font-size:12px; color:blue'></i></div>")
output$Yearselect <- renderUI({
  tags$div(class="ckbox", sliderInput("YearSelect","Years:", min = min(DatVars()$YEAR), max = max(DatVars()$YEAR), 
                                      value = c(2009, max(DatVars()$YEAR)), step = 1, sep ='', ticks = F))
})
output$Yearselect2 <- renderUI({
  tags$div(class="ckbox", sliderInput("YearSelect2","Years:", min = min(DatVars()$YEAR), max = max(DatVars()$YEAR), 
                                       value = c(2009, max(DatVars()$YEAR)), step = 1, sep ='', ticks = F))
})

###################################################################################


##################################################################################
#-------------------Category Select
#################################################################################
output$Categoryselect <- renderUI({
  if(input$Sect_sel!="FR"){
    tags$div(class="ckbox", radioButtons("CategorySelect", "Group vessels according to:", choices = DatVars()$CATEGORY, selected=DatVars()$CATEGORY[1]))
  } else if(input$Sect_sel[1]=="FR"){ 
    tags$div(class="ckbox", radioButtons("CategorySelect", "Group processors according to:", choices = DatVars()$CATEGORY, selected=DatVars()$CATEGORY[1]))
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
  if(input$tabs=="Panel1"){
    if(!is.null(input$CategorySelect)){
      if(input$CategorySelect == "State"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",
                                               c("All fisheries","All catch share fisheries","All non-catch share fisheries")), style="margin-bottom:-10px"),
          checkboxGroupInput("VariableSelect", "Select one or more state:", choices = factorOrder$state, selected="")
        )
        } 
      else if (input$CategorySelect=="Region"){
          tagList(           
            tags$div(class="select", selectInput("inSelect","",
                                                 c("All production","Groundfish production","Other species production")), style="margin-bottom:-10px"),
          checkboxGroupInput("VariableSelect", "Select one or more regions:", choices = c('Washington and Oregon','California'), selected="")
          )
        
       } 
      else if(input$CategorySelect == "Vessel length class"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",
                                               c("All fisheries","All catch share fisheries","All non-catch share fisheries")), style="margin-bottom:-10px"),
          checkboxGroupInput("VariableSelect",  "Select one or more vessel length class:", choices=c("Large vessel (> 80 ft)","Medium vessel (> 60ft, <= 80ft)",'Small vessel (<= 60ft)'), selected="")
        )
      }
      else if(input$CategorySelect == "Processor size"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",
                                               c("All production","Groundfish production","Other species production")), style="margin-bottom:-10px"),
          checkboxGroupInput("VariableSelect",  "Select one or more processor size class:", choices=c('Large','Medium','Small'), selected=""))
      } 
      else if(input$CategorySelect == "Homeport"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",
                                               c("All fisheries","All catch share fisheries","All non-catch share fisheries")), style="margin-bottom:-10px"),
          #$bsButton("selectall", "Select all", style="primary", size="extra-small",block=F, type="action"),
          tags$div(checkboxGroupInput("VariableSelect", div("Select one or more homeport:", style="margin-top:0; padding:-10px"), choices=factorOrder$port, selected=""))
        )
      } 
      else if(input$CategorySelect=="Fisheries"){
        if(input$Sect_sel=="M"|input$Sect_sel=="CP"){
          tags$div(checkboxGroupInput("VariableSelect","",choices=c("At-sea Pacific whiting"), selected="")) 
        } 
        else if(input$Sect_sel=="CV"){
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
    
  } else if(input$tabs=="Panel2") {
    if(!is.null(input$CategorySelect)){
      
      if(input$CategorySelect == "State"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",
                                               c("All fisheries", "All catch share fisheries", "All non-catch share fisheries")), style="margin-bottom:-10px"),
          if(input$LayoutSelect=='Economic measures'){
            tags$div(class="rbutton2",  radioButtons("VariableSelect", "Select ONE state", choices = c("No state selected"="","Washington"="Washington", "Oregon"="Oregon","California"="California"), selected="")) 
        } else {
          tags$div(checkboxGroupInput("VariableSelect", "Select one or more states", choices = c("Washington"="Washington", "Oregon"="Oregon","California"="California"), selected="")) 
        })
        } 
      else if(input$CategorySelect=="Region"){
          tagList(
          tags$div(class="select", selectInput("inSelect","",
                                               c("All production")), style="margin-bottom:-10px"),
          if(input$LayoutSelect=='Economic measures'){
            tags$div(class="rbutton2",  radioButtons("VariableSelect", "Select ONE region", choices = c("No region selected"="","Washington and Oregon","California"), selected=""))
          } else {
            tags$div(checkboxGroupInput("VariableSelect", "Select one or more regions", choices = c("Washington and Oregon","California"), selected=""))
          })
      } 
      else if(input$CategorySelect == "Vessel length class"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",
                                               c("All fisheries","All catch share fisheries","All non-catch share fisheries")), style="margin-bottom:-10px"),
          if(input$LayoutSelect=='Economic measures'){
            tags$div(class="rbutton2", radioButtons("VariableSelect",  "Select ONE vessel length class", choices=c("No vessel length selected"="","Large vessel (> 80 ft)","Medium vessel (> 60ft, <= 80ft)",'Small vessel (<= 60ft)'), selected=""))
        } else {
          tags$div(checkboxGroupInput("VariableSelect",  "Select one or more vessel length class", 
                                      choices=c("Large vessel (> 80 ft)","Medium vessel (> 60ft, <= 80ft)","Small vessel (<= 60ft)"), selected=""))
        })
      } 
      else if(input$CategorySelect == "Processor size"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",
                                               c("All production")), style="margin-bottom:-10px"),
          if(input$LayoutSelect=='Economic measures'){
          tags$div(class="rbutton2", radioButtons("VariableSelect",  "Select ONE processor size class:", choices=c('No size selected'='','Large','Medium','Small'), selected=""))
          } else {
            tags$div(checkboxGroupInput("VariableSelect",  "Select one or more processor size class:", choices=c('Large','Medium','Small'), selected=""))
          }
            )
      } 
      else if(input$CategorySelect == "Homeport"){
        tagList(           
          tags$div(class="select", selectInput("inSelect","",
                                               c("All fisheries","All catch share fisheries","All non-catch share fisheries")), style="margin-bottom:-10px"),
          if(input$LayoutSelect=='Economic measures'){
            tags$div(class="rbutton2", radioButtons("VariableSelect", "Select ONE homeport", choices=c("No homeport selected"="",factorOrder$port), selected=""))
          } else {
            tags$div(checkboxGroupInput("VariableSelect", "Select one or more homeports", choices=c(factorOrder$port), selected=""))
          }
         )
      } 
      else if(input$CategorySelect=="Fisheries"){
        # tagList(
        # selectInput("fishCatSelect2","", c("Catch share fisheries"="CSF", "Non-Catch Share fisheries"="NSF", "All fisheries"="AF"), selected="AF"),
        # conditionalPanel(
        #    condition="input.fishCatSelect2==AF",
        if(input$Sect_sel=="CV"){
          if(input$LayoutSelect=='Economic measures'){
            tags$div(class="rbutton", radioButtons("VariableSelect", HTML("<div> Select ONE fishery <button id='iof' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
                                               choices=c("No fishery selected"="",fish.var), selected=""))
          } else {
            tags$div(class="ckbox2", checkboxGroupInput("VariableSelect", HTML("<div> Select one or more fisheries <button id='iof' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
                                                   choices=fish.var, selected=""))
          }
#        } else if(input$Sect_sel=="CP"|input$Sect_sel=="M"){
#          if(input$LayoutSelect=='Economic measures'){
#          tags$div(class="rbutton2", radioButtons("VariableSelect", HTML("<div> Select ONE fishery <button id='iof' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
#                                                 choices=c("No fishery selected"="","At-sea Pacific whiting"), selected=""))
#          } else {
#            tags$div(class="rbutton2", checkboxGroupInput("VariableSelect", HTML("<div> Select ONE fishery <button id='iof' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
#                                                    choices=c("At-sea Pacific whiting"), selected=""))
#        }
      }} else {
        if(input$CategorySelect=="Production activities"){   
        if(input$LayoutSelect=='Economic measures'){
           tags$div(class="frbutton", radioButtons("VariableSelect", HTML("<div> Select ONE production activity <button id='iof' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
                                                             choices=c("No production activity selected"="","All production","Groundfish production","Pacific whiting production",'Non-whiting groundfish production','Other species production'), selected=""))
        } else {
          tags$div(class="frckbox", checkboxGroupInput("VariableSelect", HTML("<div> Select one or more production activities <button id='iof' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
                                                  choices=c("All production","Groundfish production","Pacific whiting production",'Non-whiting groundfish production','Other species production'), selected=""))
        }
        
        }}#end fisheries
    } #else return ()
  } # end Panel 2
})
#############################################################################################################

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
    updateCheckboxGroupInput(session,"VariableSelect", selected=fish.var[2:10])
  }
})

observe({
  if (is.null(input$selectallncs) || input$selectallncs == 0) return() 
  else if (input$selectallncs%%2 == 0) {
    updateCheckboxGroupInput(session,"VariableSelect", selected=as.character(0)) } 
  else {
    updateCheckboxGroupInput(session,"VariableSelect", selected=fish.var[11:14])
  }
})
##########################################################################################################


#output$FisherySubsetSelect <- renderUI({
#  if(is.null(input$CategorySelect)) return()
#  if(input$CategorySelect != "Fisheries"){
#     return(em("Economic data from all fisheries are included"))# data from all fisheries
#}
# } )

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
#--------Exclude vessels that fished for whiting-------------------------
output$FishWhitingselect <- renderUI({
  tags$div(class="ckbox", checkboxInput("FishWhitingSelect", HTML("<div> Include vessels that fished for Pacific whiting:
                                                              <button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'><i class='fa fa-info-circle fa-fw'></i></button> <p style='font-style:italic;font-size:10pt'>By selecting this, you will include vessels that also fished for Pacific whiting. Data from their activities are included if this box is selected.</p></div>"), 
                                        value = TRUE))
})
#
#HTML("<div> Show data summed across:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i #class='fa fa-info-circle fa-fw'></i></button></div>")
#################################################################

################################################################
#--------Select production categories-------------------------
output$Productionselect <- renderUI({
    selectInput("ProductionSelect", HTML("<div> Show data for: <button id='iprod' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                         <i class='fa fa-info-circle fa-fw' ></i></button> </div>"), 
                choices=c("All processors","Catch share processors"), selected="Catch share processors")
})
#################################################################


##################################################################
#------------------Select statistics--------------------------
output$Statselect <- renderUI({
  tagList(
    selectInput("AVE_MED", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                c('Choose Median, Mean, or Total'="", Mean="A", Median="M", Total="T"), selectize=F),
    #    actionButton("MED", "Median", style="default",size="extra-small", block=F, type="action"),
    #    if(input$AVE_MED!="A"){
    if(input$Sect_sel=='FR'){
      tags$div(class="statbox", radioButtons("StatSelect","",  choices = c(DatVars()$STAT[3:4]))) 
    } else {
    tags$div(class="statbox", radioButtons("StatSelect","",  choices = c(DatVars()$STAT[4:6])))
      }#"Statistic:",
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
    if(input$Sect_sel=='FR'){
      updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[3:4]))
    } else {
      updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[4:6]))
    }
  }  else if(input$AVE_MED=="A"){
    if(input$Sect_sel=='FR'){
    updateRadioButtons(session,"StatSelect",   choices = c(DatVars()$STAT[1:2]))
    } else {
      updateRadioButtons(session,"StatSelect",   choices = c(DatVars()$STAT[1:3]))
    }
  } else  if(input$AVE_MED=="T"){
    if(input$Sect_sel!='FR'){
    updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[7]))
    } else {
      updateRadioButtons(session,"StatSelect", choices = c(DatVars()$STAT[5])) 
    }
  } 
})
################################################################################


##################################################
#-------------- Plot options------------------------------
output$Dodgeselect <- renderUI({
  if(input$tabs!="Panel2"){
    radioButtons("DodgeSelect", HTML("<div> Plot Options: <button id='ipo' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></div>"), 
                 choices= c("Economic measures side-by-side", "Composition of Variable Cost Net Revenue","Composition of Total Cost Net Revenue"))
  } else return()
})


output$Plotselect <- renderUI({
  if(input$tabs!="Panel2"){
    if(input$DodgeSelect == "Economic measures side-by-side") {
      tags$div(class="ckbox", selectInput("PlotSelect", "", choices= c("Bar", "Point", "Line")))
    }} else return()
})


##############################################################

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

####################################################################
#===============text ==========================================
output$SelectText <- renderText ({ 
  if(input$Sect_sel=="CV"&input$CategorySelect!="Fisheries"){
    HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
                                       <b>Show data summed across these fisheries: </b><button id='isummed' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></div>")
  } else  if(input$tabs!="Panel2" & input$Sect_sel=="CV"&input$CategorySelect=="Fisheries"){
    HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:20px; margin-bottom:-25px;'>
                                       <b>Select fisheries:</b> 
                                       <h5><i>Select a fishery group,  </i>
       <button id='ifg' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></h5></div>")#,
    # HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
    #                                      <i>Select fishery groups:</i></div>") 
    
  }else if(input$Sect_sel=="FR"&input$CategorySelect!="Production activities"){
    HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
                                       <b>Show data summed across these production categories: </b><button id='isummed' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></div>")
  }
  
})
###########################################################################


#output$SelectTextYear <- renderText({
#  HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
#                                       <i>The Catch Share program was implemented after 2010</i></div>")
#})
#attr(input, "readonly") <- FALSE
#input$ActionButtonMemory <- 0
#observe({
#  if(length(input$data)>0){
#    if((input$data-input$ActionButtonMemory)>0){
#      input$ActionButtonMemory<- input$data # Equalize
#    }}
#})

