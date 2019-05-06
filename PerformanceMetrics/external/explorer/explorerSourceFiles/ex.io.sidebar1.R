#= = = = = = = = = = = = = = = = = = = = = = 
# this page handles all of the reactive
# expressions for the dynamic user interface
#= = = = = = = = = = = = = = = = = = = = = = 

output$metrics <- renderUI({
  if (input$Sect_sel != "FR") {
    name = "Vessel characteristics"
  } else {
    name = "Processor characteristics"
  }   
  tabsetPanel(
    tabPanel(name, uiOutput("demselect"), uiOutput("vesselCharacteristicStats")),
    tabPanel("Economic", uiOutput("ShortdescrSelect"), uiOutput("Statselect")),
    tabPanel("Labor", uiOutput("crewSelect"), uiOutput("crewStats")),
    tabPanel("Cost", uiOutput("costSelect"), uiOutput("costStats")),
    tabPanel("Other", uiOutput("socSelect"), uiOutput("otherStats")),
    id = "Ind_sel", type = c("tabs")
  )
})

output$SectPrint <- renderUI({
  tags$div(style = "font-size:210%;font-style:italic; padding:10px; padding-left:15px;display:inline-block;vertical-align:middle",
    if (input$Sect_sel == "CV") {
      'West Coast Trawl Catch Share Program:  Catcher Vessels'
    } else if (input$Sect_sel == "M") {
      'West Coast Trawl Catch Share Program:  Mothership Vessels'
    } else if (input$Sect_sel == "CP") {
      'West Coast Trawl Catch Share Program:  Catcher-Processor Vessels'
    } else if (input$Sect_sel == "FR") {
      'West Coast Trawl Catch Share Program:  First Receivers and Shorebased Processors'
    })
})

#End Sector Select

#= = = = = = = = = = = = = = = = = = = = = = 
##YEAR ####
#= = = = = = = = = = = = = = = = = = = = = = 

output$Yearselect <- renderUI({
  if (is.null(input$Ind_sel) || is.null(input$CategorySelect)) {
    tags$div(
      class = "ckbox",
      sliderInput(
        "YearSelect",
        "Years:",
        min = 2009,
        max = max(DatVars()$YEAR),
        value = c(2009, max(DatVars()$YEAR)),
        step = 1,
        sep = '',
        ticks = F
      )
    )
  }
  else if (input$Ind_sel == "Vessel characteristics" ||
      input$Ind_sel == 'Processor characteristics') {
    if (input$Sect_sel == "CV" &
        input$CategorySelect == 'Fisheries' &
        input$demSelect %in% c('Number of vessels', 'Vessel length')) {
      tags$div(
        class = "ckbox",
        sliderInput(
          "YearSelect",
          "Years:",
          min = 2004,
          max = max(DatVars()$YEAR),
          value = c(2009, max(DatVars()$YEAR)),
          step = 1,
          sep = '',
          ticks = F
        )
      )
    } else {
      tags$div(
        class = "ckbox",
        sliderInput(
          "YearSelect",
          "Years:",
          min = 2009,
          max = max(DatVars()$YEAR),
          value = c(2009, max(DatVars()$YEAR)),
          step = 1,
          sep = '',
          ticks = F
        )
      )
    }
  }
  else if (input$Ind_sel == 'Other') {
    if (input$Sect_sel == "CV" &
        input$CategorySelect == 'Fisheries' &
        input$socSelect %in% c('Seasonality', 'Share of landings by state', 'Gini coefficient')) {
      tags$div(
        class = "ckbox",
        sliderInput(
          "YearSelect",
          "Years:",
          min = 2004,
          max = max(DatVars()$YEAR),
          value = c(2009, max(DatVars()$YEAR)),
          step = 1,
          sep = '',
          ticks = F
        )
      )
    }
    else {
      tags$div(
        class = "ckbox",
        sliderInput(
          "YearSelect",
          "Years:",
          min = 2009,
          max = max(DatVars()$YEAR),
          value = c(2009, max(DatVars()$YEAR)),
          step = 1,
          sep = '',
          ticks = F
        )
      )
    }
  }
  else if (input$Ind_sel == 'Economic') {
    if (input$Sect_sel == 'CV' &
        input$CategorySelect == 'Fisheries' &
        input$ShortdescrSelect[1] %in% 'Revenue') {
      tags$div(
        class = "ckbox",
        sliderInput(
          "YearSelect",
          "Years:",
          min = 2004,
          max = max(DatVars()$YEAR),
          value = c(2009, max(DatVars()$YEAR)),
          step = 1,
          sep = '',
          ticks = F
        )
      )
    }
    else {
      tags$div(
        class = "ckbox",
        sliderInput(
          "YearSelect",
          "Years:",
          min = 2009,
          max = max(DatVars()$YEAR),
          value = c(2009, max(DatVars()$YEAR)),
          step = 1,
          sep = '',
          ticks = F
        )
      )
    }
  }
  else if (input$Ind_sel == 'Labor' || input$Ind_sel == 'Cost') {
    tags$div(
      class = "ckbox",
      sliderInput(
        "YearSelect",
        "Years:",
        min = 2009,
        max = max(DatVars()$YEAR),
        value = c(2009, max(DatVars()$YEAR)),
        step = 1,
        sep = '',
        ticks = F
      )
    )
  }
})

#END YEAR
#= = = = = = = = = = = = = = = = = = = = = = 

fish.var <-
  c(
    "All fisheries combined" = "All fisheries",
    " All catch share fisheries combined" = "All catch share fisheries",
    "Trawl only catch share fisheries",
    "Pacific whiting",
    "At-sea Pacific whiting",
    "Shoreside Pacific whiting",
    "Groundfish with trawl gear",
    "DTS trawl with trawl endorsement",
    "Non-whiting midwater trawl",
    "Non-whiting, non-DTS trawl with trawl endorsement",
    "Groundfish fixed gear with trawl endorsement",
    "All non-catch share fisheries combined" = "All non-catch share fisheries",
    "Crab",
    "Shrimp",
    "Other fisheries"
  )

fishgrps4cats <-
  c(
    "All fisheries",
    "All catch share fisheries",
    "Trawl only catch share fisheries",
    "All non-catch share fisheries"
  )

#= = = = = = = = = = = = = = = = = = = = = = 
#CATEGORY SELECT ####
#= = = = = = = = = = = = = = = = = = = = = = 
output$Categoryselect <- renderUI({
  if (input$Sect_sel != "FR") {
    tags$div(
      class = "ckbox",
      radioButtons(
        "CategorySelect",
        "Group vessels according to:",
        choices = DatVars()$CATEGORY,
        selected = DatVars()$CATEGORY[1]
      )
    )
  } else {
    tags$div(
      class = "ckbox",
      radioButtons(
        "CategorySelect",
        "Group processors according to:",
        choices = DatVars()$CATEGORY,
        selected = DatVars()$CATEGORY[1]
      )
    )
  }
})
#= = = = = = = = = = = = = = = = = = = = = = 
# End Category select
#= = = = = = = = = = = = = = = = = = = = = = 

#= = = = = = = = = = = = = = = = = = = = = = 
#INDICATOR SELECT - Vessel/processor characteristics, ECONOMIC, labor and other ####
#= = = = = = = = = = = = = = = = = = = = = = 
output$IndicatorSelect <- renderUI({
  if (input$Sect_sel != 'FR') {
    selectInput(
      "Ind_sel",
      HTML(
        "<div> Select an indicator category: <button id='ipo' type='button' class='btn btn-default action-button shiny-bound-input'>
        <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
      ),
      c('Vessel characteristics', "Economic", "Labor", "Other"),
      selected = 'Vessel characteristics',
      selectize = T
      )
  } else {
    selectInput(
      "Ind_sel",
      HTML(
        "<div> Select an indicator category: <button id='ipo' type='button' class='btn btn-default action-button shiny-bound-input'>
        <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
      ),
      c('Processor characteristics', "Economic", "Labor", "Other"),
      selected = 'Processor characteristics',
      selectize = T
      )
  }
  
  #  }
})  

#= = = = = = = = = = = = = = = = = = = = = = 
#END INDICATOR SELECT
#= = = = = = = = = = = = = = = = = = = = = = 


#= = = = = = = = = = = = = = = = = = = = = = 
##METRIC SELECT ####
#= = = = = = = = = = = = = = = = = = = = = = 
##################################################
output$Metricselect <- renderUI({ 
  if(!input$LayoutSelect){
    if(input$Ind_sel=="Economic") {
      tags$div(class='met_mod', radioButtons("MetricSelect","", choices ="Select an economic measure and statistic below"), style="font-style:italic;margin-bottom:20px;margin-top:-32px;margin-left:-15px;padding-top:0;")
    } #end economic
    else# if(input$Ind_sel=="Demographic")
      {
        tags$div(class='met_mod', radioButtons("MetricSelect","", choices ="Select a metric below"), style="font-style:italic;margin-bottom:20px;margin-top:-32px;margin-left:-15px;padding-top:0;")

      } 
  } else {
    ##Metric Select
    if(input$Ind_sel=="Economic") {
     tags$div(class='met_mod', radioButtons("MetricSelect","", choices ="Select an economic measure and statistic below"), style="font-style:italic;margin-bottom:20px;margin-top:-32px;margin-left:-15px;padding-top:0;")
    }
    else #if(input$Ind_sel=="Demographic")
      {
      # -------> MODIFY THIS PART <--------#
      tags$div(class='met_mod', radioButtons("MetricSelect","", choices ="Select a metric below"), style="font-style:italic;margin-bottom:-20px;margin-top:-32px;margin-left:-15px;padding-top:0;") #NEED TO MODIFY THIS!!!!!!!!!!!!!!
      } #end demographic
  }
}) #END METRIC SELECT
#= = = = = = = = = = = = = = = = = = = = = = 
#End Metric Select
#= = = = = = = = = = = = = = = = = = = = = = 

#= = = = = = = = = = = = = = = = = = = = = = 
##VARIABLE SELECT ####
#= = = = = = = = = = = = = = = = = = = = = = 

output$filters <- renderUI({
  if (input$Sect_sel == "CV") {
    tabsetPanel(
      tabPanel(DatVars()$CATEGORY[1]),
      tabPanel(DatVars()$CATEGORY[2]),
      tabPanel(names(DatVars()$CATEGORY[3]), value=DatVars()$CATEGORY[3]),
      tabPanel(DatVars()$CATEGORY[4]),
      id = "CategorySelect"
    )
  } else if (input$Sect_sel == "FR") {
    tabsetPanel(
      tabPanel(names(DatVars()$CATEGORY[1]), value=DatVars()$CATEGORY[1]),
      tabPanel(DatVars()$CATEGORY[2]),
      tabPanel(DatVars()$CATEGORY[3]),
      id = "CategorySelect"
    )
  } else {
  }
})

output$Variableselect <- renderUI({
  if (input$Sect_sel == "M" | input$Sect_sel == "CP") {
    hidden(
      checkboxGroupInput(
        "VariableSelect",
        "",
        choices = "At-sea Pacific whiting",
        selected = "At-sea Pacific whiting"
      )
    )
  }
  if (!is.null(input$CategorySelect)) {
    if (input$Sect_sel == "CV") {
      if (input$CategorySelect == "State") {
        if (!input$LayoutSelect) {
          tagList(
            checkboxGroupInput(
              "VariableSelect",
              NULL,
              choices = factorOrder$state,
              selected = factorOrder$state[1]
            )
          )
        } else {
          tagList(
            tags$div(
              class = "rbutton3",
              radioButtons(
                "VariableSelect",
                NULL,
                choices = c(factorOrder$state),
                selected = factorOrder$state[1]
              )
            )
          )
        }
      } #state
      else if (input$CategorySelect == "Vessel length class") {
        if (!input$LayoutSelect) {
          tagList(
            checkboxGroupInput(
              "VariableSelect",
              NULL,
              choices = c(
                "Large vessel (> 80 ft)",
                "Medium vessel (> 60ft, <= 80ft)",
                "Small vessel (<= 60 ft)"
              ),
              selected = "Large vessel (> 80 ft)"
            )
          )
        } else {
          tagList(
            tags$div(
              class = "rbutton3",
              radioButtons(
                "VariableSelect",
                NULL,
                choices = c(
                  "Large vessel (> 80 ft)",
                  "Medium vessel (> 60ft, <= 80ft)",
                  "Small vessel (<= 60 ft)"
                ),
                selected = "Large vessel (> 80 ft)"
              )
            )
          )
        }
      } else if (input$CategorySelect == "Homeport") {
        if (!input$LayoutSelect) {
          tagList(
            tags$div(
              checkboxGroupInput(
                "VariableSelect",
                NULL,
                choices = factorOrder$port,
                selected = factorOrder$port[1]
              )
            )
          )
        } else {
          tagList(
            tags$div(
              class = "rbutton3",
              radioButtons(
                "VariableSelect",
                NULL,
                choices = c(factorOrder$port),
                selected = factorOrder$port[1]
              )
            )
          )
        }
      } #end homeport
      else if (input$CategorySelect == "Fisheries") {
        if (!input$LayoutSelect) {
          tags$div(class = "ckbox2",
                   checkboxGroupInput(
                     "VariableSelect",
                     NULL,
                     choices = fish.var,
                     selected = fish.var[1]
                   ))
        } else {
          tags$div(class = "ckbox3",
            radioButtons(
              "VariableSelect",
              NULL,
              choices = c(fish.var),
              selected = fish.var[1]
            ))
        }
      }#end fisheries
    } ##END Catcher Vessels
    
    
    
    else if (input$Sect_sel == "FR") {
      if (input$CategorySelect == "Region") {
        if (!input$LayoutSelect) {
          if (input$Ind_sel == "Processor characteristics") {
            if (input$demSelect != "Proportion of revenue from catch share species") {
              tagList(
                tags$div(
                  class = "ckbox",
                  checkboxGroupInput(
                    "VariableSelect",
                    NULL,
                    choices = c('Washington and Oregon', 'California'),
                    selected = "Washington and Oregon"
                    )
                )
              )
            } else {
              tagList(
                tags$div(
                  class = "ckbox",
                  checkboxGroupInput(
                    "VariableSelect",
                    NULL,
                    choices = c('Washington and Oregon', 'California'),
                    selected = "Washington and Oregon"
                    )
                )
              )
            }
          } else if (input$Ind_sel == "Economic") {
            tagList(
              tags$div(
                class = "ckbox",
                checkboxGroupInput(
                  "VariableSelect",
                  NULL,
                  choices = c('Washington and Oregon', 'California'),
                  selected = "Washington and Oregon"
                  )
              )
            )
          } else {
            tagList(
              tags$div(
                class = "ckbox",
                checkboxGroupInput(
                  "VariableSelect",
                  NULL,
                  choices = c('Washington and Oregon', 'California'),
                  selected = "Washington and Oregon"
                  )
              )
            )
          }
        } #ENd compare processors
        else {
          if (input$Ind_sel != "Other") {
            tagList(
              tags$div(
                class = "rbutton3",
                radioButtons(
                  "VariableSelect",
                  NULL,
                  choices = c(
                    'Washington and Oregon',
                    'California'
                  ),
                  selected = "Washington and Oregon"
                  )
            )
            )
          } else {
            tagList(
              tags$div(
                class = "rbutton3",
                radioButtons(
                  "VariableSelect",
                  NULL,
                  choices = c(
                    'Washington and Oregon',
                    'California'
                  ),
                  selected = "Washington and Oregon"
                  )
            )
            )
          }
        }
        } else if (input$CategorySelect == "Processor size") {
          if (!input$LayoutSelect) {
            if (input$Ind_sel == 'Processor characteristics') {
              if (input$demSelect != "Proportion of revenue from catch share species") {
                tagList(
                  tags$div(
                    checkboxGroupInput(
                      "VariableSelect",
                      NULL,
                      choices = c("Large", 'Medium', 'Small'),
                      selected = "Large"
                      )
                  )
                )
              } else {
                tagList(
                  tags$div(
                    checkboxGroupInput(
                      "VariableSelect",
                      NULL,
                      choices = c("Large", 'Medium', 'Small'),
                      selected = "Large"
                      )
                  )
                )
              }
            } else if (input$Ind_sel == 'Economic') {
              tagList(
                tags$div(
                  checkboxGroupInput(
                    "VariableSelect",
                    NULL,
                    choices = c("Large", 'Medium', 'Small'),
                    selected = "Large"
                    )
                )
              )
            } else {
              tagList(
                tags$div(
                  checkboxGroupInput(
                    "VariableSelect",
                    NULL,
                    choices = c("Large", 'Medium', 'Small'),
                    selected = "Large"
                    )
                )
              )
            }
          } else {
            if (input$Ind_sel != "Other") {
              tagList(
                tags$div(
                  class = "rbutton3",
                  radioButtons(
                    "VariableSelect",
                    NULL,
                    choices = c(
                      "Large",
                      'Medium',
                      'Small'
                    ),
                    selected = "Large"
                    )
              )
              )
            } else {
              tagList(
                tags$div(
                  class = "rbutton3",
                  radioButtons(
                    "VariableSelect",
                    NULL,
                    choices = c(
                      "Large",
                      'Medium',
                      'Small'
                    ),
                    selected = "Large"
                    )
              )
              )
            }
          }
          } #End processor class
      else {
        if (!input$LayoutSelect) {
          tags$div(
            class = 'FRprod',
            checkboxGroupInput(
              "VariableSelect",
              NULL,
              choices = c(
                "All production",
                "Groundfish production",
                "Pacific whiting production",
                'Non-whiting groundfish production',
                'Other species production'
              ),
              selected = "All production"
            )
          )
        } else {
          tags$div(class = 'rbutton2',
                   radioButtons(
                     "VariableSelect",
                     NULL,
                     choices = c(
                       "All production",
                       "Groundfish production",
                       "Pacific whiting production",
                       'Non-whiting groundfish production',
                       'Other species production'
                     ),
                     selected = "All production"
                   ))
        } #End metrics
      } #end fisheries All production; Pacific whiting production; Non-whiting groundfish production; Other species production
    } #End FR
  }#end not null
  else
    return ()
}) 
#= = = = = = = = = = = = = = = = = = = = = = 
#End Variable select
#= = = = = = = = = = = = = = = = = = = = = = 

# Select FISHAK ####
output$FishAkselect <- renderUI({
  if (!is.null(input$LayoutSelect) && !input$LayoutSelect &&
      !is.null(input$Sect_sel) && input$Sect_sel == 'CV' &&
      !is.null(input$Ind_sel) && input$Ind_sel == "Vessel characteristics") {
    if (input$demSelect == 'Revenue diversification' || input$demSelect == 'Proportion of revenue from catch share fishery' || input$demSelect == 'Number of fisheries') {
      tagList(
        tags$div(style = "font-weight:bold; margin-bottom: 7px", "Alaskan Fisheries:"),
        tags$div(materialSwitch(
          inputId = "FishAkSelect",
          label = "Alaskan fisheries",
          right = TRUE
        )))
    }
  }
})

#= = = = = = = = = = = = = = = = = = = = = = 
#end fishak
#= = = = = = = = = = = = = = = = = = = = = = 

output$fisheriesOptions <- renderUI({
  if (input$Sect_sel == "CV") {
    if (!is.null(input$CategorySelect) && input$CategorySelect != "Fisheries") {
      radioButtons(
        "inSelect",
        "Fisheries",
        fishgrps4cats
      )
    }
  }
  else if (input$Sect_sel == "FR") {
    if (!is.null(input$CategorySelect) && input$CategorySelect != "Fisheries") {
      radioButtons(
        "inSelect",
        "Production Categories",
        c(
          "All production",
          "Groundfish production",
          "Other species production"
        )
      )
    }
  }
})

#= = = = = = = = = = = = = = = = = = = = = = 
#Select whiting (data summed across category) ####
#= = = = = = = = = = = = = = = = = = = = = = 
output$FishWhitingselect <- renderUI({
  if (input$Sect_sel == "M" || input$Sect_sel == "CP") {
    hidden(uiOutput("FishWhitingselectBox"))
  } else {
    uiOutput("FishWhitingselectBox")
  }
})


output$FishWhitingselectBox <- renderUI({
    tags$div(
      class = 'ckbox',
      checkboxGroupInput(
        'FishWhitingSelect',
        HTML(
          "<div> Vessel type:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
          </button></div>"
        ),
        choices = DatVars()$whitingv,
        selected = DatVars()$whitingv[1]
        )
    )
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
#= = = = = = = = = = = = = = = = = = = = = = 
#end whiting
#= = = = = = = = = = = = = = = = = = = = = = 

#Economic measure select ####

output$ShortdescrSelect <- renderUI({
  if (input$LayoutSelect) {
    tags$div(
      class = "ckbox",
      checkboxGroupInput(
        "ShortdescrSelect",
        NULL,
        choices = DatVars()$SHORTDESCR,
        selected = DatVars()$SHORTDESCR
        )
    )
  } else {
    tags$div(
      class = "ckbox",
      radioButtons(
        "ShortdescrSelect",
        NULL,
        choices = DatVars()$SHORTDESCR,
        selected = DatVars()$SHORTDESCR[1]
      )
    )
  }
})

#= = = = = = = = = = = = = = = = = = = = = = 
#End economic measure select
#= = = = = = = = = = = = = = = = = = = = = = 


# Identify Individual metrics for demography ####

##DatVars() is defined in ex.reactives##
##Checkbox types are defined in ui.R
##Note: See statistic selection below for settings of 'Mean' 'Median' 'Total' when grouping by vessels##
output$demselect <- renderUI({
  ##Settings for grouping by 'Metrics'
  if (input$LayoutSelect) {
    if (input$Sect_sel == "CV") {
      if (input$AVE_MED2 == "Total") {
        tags$div(
          class = "ckbox2345",
          checkboxGroupInput(
            "demSelect",
            NULL,
            choices = c(DatVars()$METRIC1),
            selected = 'Number of vessels'
          )
        )
      } else {
        tags$div(
          class = "ckbox_1",
          checkboxGroupInput(
            "demSelect",
            NULL,
            choices = c(DatVars()$METRIC1),
            selected = 'Vessel length'
          )
        )
      }
    } else {
      if (input$Sect_sel == 'FR') {
        if (input$AVE_MED2 == 'Total') {
          tags$div(
            class = "ckbox_4",
            checkboxGroupInput(
              "demSelect",
              NULL,
              choices = c(DatVars()$METRIC1),
              selected = "Number of processors"
            )
          )
        } else {
          tags$div(
            class = "ckbox_1",
            checkboxGroupInput(
              "demSelect",
              NULL,
              choices = c(DatVars()$METRIC1),
              selected = "Number of species processed"
            )
          )
        }
        
      } else {
        if (input$Sect_sel == 'CP' | input$Sect_sel == 'M') {
          if (input$AVE_MED2 == 'Total') {
            tags$div(
              class = "ckbox23",
              checkboxGroupInput(
                "demSelect",
                NULL,
                choices = c(DatVars()$METRIC1),
                selected = 'Number of vessels'
              )
            )
          } else {
            tags$div(
              class = "ckbox_1",
              checkboxGroupInput(
                "demSelect",
                NULL,
                choices = c(DatVars()$METRIC1),
                selected = 'Vessel length'
              )
            )
          }
          ##Settings for 'Group by vessels' or 'Group by processors'
        }
      }
    }
  } else {
    if (input$Sect_sel == 'FR') {
      tags$div(
        class = "ckbox",
        radioButtons(
          "demSelect",
          NULL,
          choices = c(DatVars()$METRIC1),
          selected = 'Number of processors'
        )
      )
    } else {
      tags$div(
        class = "ckbox",
        radioButtons(
          "demSelect",
          NULL,
          choices = c(DatVars()$METRIC1),
          selected = 'Number of vessels'
        )
      )
    }
  }
})



#= = = = = = = = = = = = = = = = = = = = = = 
#end demography select
#= = = = = = = = = = = = = = = = = = = = = = 

#= = = = = = = = = = = = = = = = = = = = = = 
#Identify individual metrics for crew class ####
#= = = = = = = = = = = = = = = = = = = = = = 
output$crewSelect <- renderUI({
  if (input$LayoutSelect) {
    if (input$Sect_sel == 'CV') {
      if (input$crewStat == 'Total') {
        tags$div(
          class = "ckbox23456",
          checkboxGroupInput(
            "crewSelect",
            NULL,
            choices = c(DatVars()$METRIC2),
            selected = "Number of crew"
          )
        )
      } else {
        tags$div(
          class = "ckbox",
          checkboxGroupInput(
            "crewSelect",
            NULL,
            choices = c(DatVars()$METRIC2),
            selected = "Number of crew"
          )
        )
      }
    } else if (input$Sect_sel == 'M' | input$Sect_sel == 'CP') {
        if (input$crewStat == 'Total') {
          tags$div(
            class = "ckbox23456",
            checkboxGroupInput(
              "crewSelect",
              NULL,
              choices = c(DatVars()$METRIC2),
              selected = "Number of processing and non-processing crew"
            )
          )
    } else {
      tags$div(
        class = "ckbox",
        checkboxGroupInput(
          "crewSelect",
          NULL,
          choices = c(DatVars()$METRIC2),
          selected = "Number of processing and non-processing crew"
        )
      )
    }
  } else {
      if(input$crewStat == 'Total') {
        tags$div(
          class = 'ckbox_2',
          checkboxGroupInput(
            "crewSelect",
            NULL,
            choices = c(DatVars()$METRIC2),
            selected = 'Number of workers'
          )
        )
      }
    else {
      tags$div(
        class = "ckbox",
        checkboxGroupInput(
          "crewSelect",
          NULL,
          choices = c(DatVars()$METRIC2),
          selected = "Number of workers"
        )
      )
    }
    }
  }
  else {
    if (input$Sect_sel == 'CV') {
      tags$div(
        class = "ckbox",
        radioButtons(
          "crewSelect",
          NULL,
          choices = c(DatVars()$METRIC2),
          selected = "Number of crew"
        )
      )
    }
    else if (input$Sect_sel == 'M' | input$Sect_sel == 'CP') {
      tags$div(
        class = "ckbox",
        radioButtons(
          "crewSelect",
          NULL,
          choices = c(DatVars()$METRIC2),
          selected = "Number of processing and non-processing crew"
        )
      )
    } else {
      tags$div(
        class = "statbox",
        radioButtons(
          "crewSelect",
          NULL,
          choices = c(DatVars()$METRIC2),
          selected = "Number of workers"
        ))
    }
  }
})
#= = = = = = = = = = = = = = = = = = = = = = 
#Identify individual metrics for Other class ####
#= = = = = = = = = = = = = = = = = = = = = = 
output$socSelect <- renderUI({
  ##Setting when grouping by Metrics#####
  if (input$LayoutSelect) {
    if (input$Sect_sel == "CV") {
      if (input$otherStat == "Total") {
        tags$div(
          class = "ckbox_3",
          checkboxGroupInput(
            "socSelect",
            NULL,
            choices = c(DatVars()$METRIC3a),
            selected = "Days at sea"
          )
        )
      } else {
        tags$div(
          class = "ckbox",
          checkboxGroupInput(
            "socSelect",
            NULL,
            choices = c(DatVars()$METRIC3a),
            selected = "Days at sea"
          )
        )
      }
    } else {
      if (input$Sect_sel == "FR") {
        if (input$otherStat == 'Total') {
          tags$div(
            class = "ckbox",
            checkboxGroupInput(
              "socSelect",
              NULL,
              choices = "",
              selected = ""
            )
          )
        } else {
          tags$div(
            class = "ckbox_1",
            checkboxGroupInput(
              "socSelect",
              NULL,
              choices = "",
              selected = ""
            )
          )
        }
      } else {
        if (input$Sect_sel == 'CP' | input$Sect_sel == 'M') {
          if (input$otherStat == 'Total') {
            tags$div(
              class = "ckbox",
              checkboxGroupInput(
                "socSelect",
                NULL,
                choices = c(DatVars()$METRIC3a),
                selected = "Days at sea"
              )
            )
          } else {
            tags$div(
              class = "ckbox",
              checkboxGroupInput(
                "socSelect",
                NULL,
                choices = c(DatVars()$METRIC3a),
                selected = "Days at sea"
              )
            )
          }
        }
        ##Settings when 'Groups of vessels" or 'Groups of processors'#####
      }
    }
  } else {
    if (input$Sect_sel == "FR") {
      tags$div(
        class = "ckbox",
        radioButtons(
          "socSelect",
          NULL,
          choices = c(DatVars()$METRIC3),
          selected = "Gini coefficient"
        )
      )
      
    } else {
      tags$div(
        class = "ckbox",
        radioButtons(
          "socSelect",
          NULL,
          choices = c(DatVars()$METRIC3),
          selected = "Days at sea"
        )
      )
    }
  }
})

##Identify individual metrics for the cost category####
output$costSelect <- renderUI({
  if(input$Sect_sel=='CV'){
    if (!input$LayoutSelect) {
      tags$div(class="statboxC", radioButtons("costSelect",'Cost categories:',
                                              choices = DatVars()$COSTS, selected = c('All variable costs')))
    } else {
      tags$div(class="statboxC", checkboxGroupInput("costSelect",'Cost categories:',
                                                    choices = DatVars()$COSTS, selected = c('All fixed costs','All variable costs')))
    }
  } else if(input$Sect_sel=='FR'){
    if (!input$LayoutSelect) {
      tags$div(class="statboxF", radioButtons("costSelect",'Cost categories:',
                                              choices = DatVars()$COSTS, selected = c('All variable costs')))
    } else {
      tags$div(class="statboxF", checkboxGroupInput("costSelect",'Cost categories:',
                                                    choices = DatVars()$COSTS, selected = c('All fixed costs','All variable costs')))
    }
  } else if(input$Sect_sel=='M' || input$Sect_sel=='CP'){
    if (!input$LayoutSelect) {
      tags$div(class="statboxM", radioButtons("costSelect",'Cost categories:',
                                                    choices = DatVars()$COSTS, selected = c('All variable costs')))
    } else {
      tags$div(class="statboxM", checkboxGroupInput("costSelect",'Cost categories:',
                                                    choices = DatVars()$COSTS, selected = c('All fixed costs', 'All variable costs')))
    }
  }# else {
   # tags$div(class="statboxM", checkboxGroupInput("costSelect",'Cost categories:',
                       #                           choices = DatVars()$COSTS,selected =  c('All fixed costs', 'All variable costs')))
  #}
})
#= = = = = = = = = = = = = = = = = = = = = = 
#Show data summed across button ####
#= = = = = = = = = = = = = = = = = = = = = = 
output$VesSumSelect <- renderUI({
  if (PermitPlot()) {
    if (input$VariableSelect != "All Fisheries" &
        input$VariableSelect != "All catch share fisheries" &
        input$VariableSelect != "All non-catch share fisheries") {
      tagList(tags$div(class = "ckbox", radioButtons(
        "VesSum",
        HTML(
          "<div> Show data summed: <button id='iVesSum' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
        ),
        choices = c(
          "within selected fisheries" = "within",
          "across all catch share fisheries" = "acrossCS",
          "across all West Coast fisheries" = "acrossWC"
        )
      )))
    } else {
      return()
    }
  } else {
    return()
  }
})
#span("For all vessels that fished within selected fisheries, show data for activities:", style="font-size:11pt; font-weight:bold;"), #font-style:italic;

#= = = = = = = = = = = = = = = = = = = = = = 
#Statistic select for economic measures ####
#= = = = = = = = = = = = = = = = = = = = = = 
#Being by select average, median, or total values
output$Statselect <- renderUI({
  if (input$Sect_sel == "FR")  {
    tagList(
      selectInput(
        "AVE_MED",
        HTML(
          "<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
        ),
        c(
          Mean = "A",
          Median = "M",
          Total = "T"
        ),
        selectize = F
      ),
      tags$div(class = "statbox", radioButtons(
        "StatSelect", "",  choices = c(DatVars()$STAT[5:6])
      ))
    )
  } else {
    tagList(
      selectInput(
        "AVE_MED",
        HTML(
          "<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
        ),
        c(
          Mean = "A",
          Median = "M",
          Total = "T"
        ),
        selectize = F
      ),
      tags$div(
        class = "statbox",
        radioButtons(
          "StatSelect",
          "",
          choices = c(DatVars()$STAT[5:7]),
          selected = DatVars()$STAT[7]
        )
      )
    )
  }
})

#select whether to show values per vessel, /vessel/day, or /vessel/metric-ton
observe({
  if (is.null(input$AVE_MED)) {
    return()
  }
  else
    if (input$Sect_sel != 'FR') {
      if (input$AVE_MED == "M") {
        updateRadioButtons(session, "StatSelect", choices = c(DatVars()$STAT[5:7]))
      }  else if (input$AVE_MED == "A") {
        updateRadioButtons(session, "StatSelect",   choices = c(DatVars()$STAT[1:3]))
      } else  if (input$AVE_MED == "T") {
        updateRadioButtons(session, "StatSelect", choices = c(DatVars()$STAT[9:11]))
      }
    } else if (input$AVE_MED == "M") {
      updateRadioButtons(session, "StatSelect", choices = c(DatVars()$STAT[4:5]))
    }  else if (input$AVE_MED == "A") {
      updateRadioButtons(session, "StatSelect",   choices = c(DatVars()$STAT[1:2]))
    } else  if (input$AVE_MED == "T") {
      updateRadioButtons(session, "StatSelect", choices = c(DatVars()$STAT[7:8]))
    }
})


#= = = = = = = = = = = = = = = = = = = = = = 
#End stat select  for economic measures
#= = = = = = = = = = = = = = = = = = = = = = 
output$costStats <- renderUI({
  if(input$Sect_sel=="FR")  { 
    tagList(
      selectInput("AVE_MED_COSTS", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                        <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                  c(Mean="A", Median="M", Total="T"), selectize=F),
      tags$div(class="statbox", radioButtons("costStatSelect","",  choices = c(DatVars()$STAT[4:6]))))
  } else if (input$Sect_sel=='CP'|input$Sect_sel=='M') {
    tagList(
      selectInput("AVE_MED_COSTS", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                        <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                  c(Mean="A", Median="M", Total="T"), selectize=F),
      tags$div(class="statbox", radioButtons("costStatSelect","",  choices = c(DatVars()$STAT[5:8]), selected=DatVars()$STAT[5])))
  }else {
    tagList(
      selectInput("AVE_MED_COSTS", HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> 
                                        <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
                  c(Mean="A", Median="M", Total="T"), selectize=F),
      tags$div(class="statbox", radioButtons("costStatSelect","",  choices = c(DatVars()$STAT[5:8]), selected=DatVars()$STAT[5])))
  }
})

#select whether to show values per vessel, /vessel/day, or /vessel/metric-ton
observe({
  if (is.null(input$AVE_MED_COSTS)) {return()}
  else 
    if(input$Sect_sel=='CP'|input$Sect_sel=='M'){
      if(input$AVE_MED_COSTS=="M"){
        updateRadioButtons(session,"costStatSelect", choices = c(DatVars()$STAT[5:8]))
      }  else if(input$AVE_MED_COSTS=="A"){
        updateRadioButtons(session,"costStatSelect",   choices = c(DatVars()$STAT[1:4]))
      } else  if(input$AVE_MED_COSTS=="T"){
        updateRadioButtons(session,"costStatSelect", choices = c(DatVars()$STAT[9:12]))
      } 
    } else if(input$Sect_sel=='CV'){
      if(input$AVE_MED_COSTS=="M"){
        updateRadioButtons(session,"costStatSelect", choices = c(DatVars()$STAT[5:8]))
      }  else if(input$AVE_MED_COSTS=="A"){
        updateRadioButtons(session,"costStatSelect",   choices = c(DatVars()$STAT[1:4]))
      } else  if(input$AVE_MED_COSTS=="T"){
        updateRadioButtons(session,"costStatSelect", choices = c(DatVars()$STAT[9:12]))
      } 
    } else if(input$AVE_MED_COSTS=="M"){
      updateRadioButtons(session,"costStatSelect", choices = c(DatVars()$STAT[4:6]))
    }  else if(input$AVE_MED_COSTS=="A"){
      updateRadioButtons(session,"costStatSelect",   choices = c(DatVars()$STAT[1:3]))
    } else  if(input$AVE_MED_COSTS=="T"){
      updateRadioButtons(session,"costStatSelect", choices = c(DatVars()$STAT[7:9]))
    } 
})

#= = = = = = = = = = = = = = = = = = = = = = 
## Stat selection for non-economic metrics ####
#= = = = = = = = = = = = = = = = = = = = = = 
output$vesselCharacteristicStats <- renderUI({
  if (input$LayoutSelect) {
    tagList(radioButtons(
      "AVE_MED2",
      HTML(
        "<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
      ),
      choices = c("Mean", "Median", 'Total'),
      select = 'Median'
    ))
  } #End Metrics
  else {
    if (input$demSelect %in% c("Number of species processed")) {
      tagList(radioButtons(
        "AVE_MED2",
        HTML(
          "<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
        ),
        choices = c("Mean", "Median", 'Total'),
        select = 'Median'
      ))
    } else if (input$demSelect %in% c("Number of vessels", "Number of processors")) {
      tagList(tags$div(
        class = 'StatGrey',
        radioButtons(
          "AVE_MED2",
          HTML(
            "<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
          ),
          choices = c("Mean", "Median", 'Total'),
          select = 'Total'
        )
      ))
    } else if (input$demSelect %in% c(
      "Vessel length",
      "Revenue diversification",
      "Number of fisheries",
      "Proportion of revenue from CS fishery",
      "Proportion of revenue from catch share species",
      "Proportion of landings from CS fishery",
      "Vessel market value",
      "Vessel replacement value",
      "Vessel horsepower"
    )) {
      tagList(tags$div(
        class = 'StatGrey2',
        radioButtons(
          "AVE_MED2",
          HTML(
            "<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
          ),
          choices = c("Mean", "Median", 'Total'),
          select = 'Median'
        )
      ))
    }}}
)

output$crewStats <- renderUI({
  if (input$LayoutSelect) {
    tagList(radioButtons(
      "crewStat",
      HTML(
        "<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
      ),
      choices = c("Mean", "Median", 'Total'),
      select = 'Median'
    ))
  }
  else if (input$crewSelect == 'Crew wage per day' |
      input$crewSelect == 'Hourly compensation'|
      input$crewSelect == 'Crew wage per year' |
      input$crewSelect == 'Crew wage per dollar revenue' |
      input$crewSelect == 'Revenue per position-day' |
      input$crewSelect == 'Revenue per crew-day') {
    tagList(tags$div(
      class = 'StatGrey2',
      radioButtons(
        "crewStat",
        HTML(
          "<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
        ),
        choices = c("Mean", "Median", 'Total'),
        select = 'Median'
      )
    ))
  } else {
    tagList(radioButtons(
      "crewStat",
      HTML(
        "<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
      ),
      choices = c("Mean", "Median", 'Total'),
      select = 'Median'
    ))
  }
})

output$otherStats <- renderUI({
  if (input$LayoutSelect) {
    tagList(radioButtons(
      "otherStat",
      HTML(
        "<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
      ),
      choices = c("Mean", "Median", 'Total'),
      select = 'Median'
    ))
  } else {
      if (input$socSelect %in% c("Share of landings by state",
                                 "Seasonality",
                                 "Gini coefficient")) {
        tags$div(
          class = 'met_mod',
          radioButtons("otherStat", "", choices = ""),
          style = "margin-bottom:20px;margin-top:-32px;margin-left:-15px;padding-top:0;"
        )
      } else if (input$socSelect %in% c("Fuel use per day",
                                        "Speed while fishing",
                                        "Hourly compensation")) {
        tags$div(
          class = 'StatGrey2',
          radioButtons(
            "otherStat",
            HTML(
              "<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
            ),
            choices = c("Mean", "Median", 'Total'),
            select = 'Median'
          )
        )
      } else {
        tagList(radioButtons(
          "otherStat",
          HTML(
            "<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
          ),
          choices = c("Mean", "Median", 'Total'),
          select = 'Median'
        ))
      }
  }
})
#= = = = = = = = = = = = = = = = = = = = = = 
#End stat selection for non-economic metrics
#= = = = = = = = = = = = = = = = = = = = = = 
#tags$div(class='met_mod', radioButtons("MetricSelect","", choices ="Select a metric below"), style="font-style:italic;margin-bottom:20px;margin-top:-32px;margin-left:-15px;padding-top:0;")

#= = = = = = = = = = = = = = = = = = = = = = 
#Layout select (Compare vessels or compare metrics) ####
#= = = = = = = = = = = = = = = = = = = = = = 
output$Layoutselect <- renderUI({
  materialSwitch(
    inputId = "LayoutSelect",
    label = "Select Multiple Metrics",
    right = TRUE,
    value = FALSE
  )
})

#= = = = = = = = = = = = = = = = = = = = = = 
#ENd layout select
#= = = = = = = = = = = = = = = = = = = = = = 

# Plot options

#======================================
output$Plotselect <- renderUI({
  materialSwitch(
    inputId = "PlotSelect",
    label = "Show variance",
    right = TRUE,
    value = TRUE
  )
  # } else {
  #   hidden(
  #     tagList(
  #       tags$div(style = "font-weight:bold; margin-bottom: 7px", "Display Options:"),
  #       materialSwitch(
  #         inputId = "PlotSelect",
  #         label = "Show variance",
  #         right = TRUE,
  #         value = TRUE
  #       )
  #     )
  #   )
  # }
  
  
  #tags$div(class = "ckbox", checkboxInput("PlotSelect", "Show variance", TRUE),
#  bsTooltip(id = "PlotSelect", title = "Shaded area represents 1SD about mean or 25th and 75th percentiles about median.", placement = "bottom", trigger = "hover", options = NULL)
  #)
  
  
  
})

output$Plotselectoption <- renderUI({
  tags$div(
    class = "ckbox",
    radioButtons(
      "PlotSelectOption",
      span("", style = "font-weight:bold;font-size:12pt"),
      choices = c(
        "Standard deviation or Median average deviation",
        "Percentiles (25th and 75th)"
      ),
      selected = 'Standard deviation or Median average deviation'
    )
  )
})


#===============text ==========================================#
output$SelectText <- renderText ({
  if (input$CategorySelect != "Fisheries") {
    if (input$Sect_sel == "CV") {
      HTML(
        "<div style='display:inline-block;width:100%; margin-top:10px'>
        <b>Show data summed across these fisheries: </b><button id='isummed' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></div>"
      )
    } else if (input$Sect_sel == "FR") {
      HTML(
        "<div style='display:inline-block;width:100%; margin-top:10px'>
        <b>Show data summed across these production categories: </b><button id='isummed' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button></div>"
      )
    }
    }#else  if(input$CategorySelect=="Fisheries"){
  })

observeEvent(input$reset_input, {
  if (input$LayoutSelect) {
    updateRadioButtons(session, "VariableSelect", selected = "")
  } else{
    updateCheckboxGroupInput(session, "VariableSelect", selected = as.character(0))
  }
})
