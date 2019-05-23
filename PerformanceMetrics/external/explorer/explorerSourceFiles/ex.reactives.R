#This file handles the reactive expressions for data management and statistical operations.

# creating the dat() reactive function that contains the user selected dataset
# The re-classification of data types can be transfered to the read-in file

# DatMain: data load ####
DatMain <- reactive({
  # data load moved to serverhead
  # data is loaded from serverHead.R load call
  if (input$Sect_sel == "CV") {
    dat <- CVperfmetrics
  } else if (input$Sect_sel == "M") {
    dat <- Mperfmetrics
  } else if (input$Sect_sel == "CP") {
    dat <- CPperfmetrics
  } else if (input$Sect_sel == "FR") {
    dat <- FRperfmetrics
  }

})

nrcomponents <- c('Revenue', 'Variable costs', 'Fixed costs', 'Variable cost net revenue', 'Total cost net revenue')

# DatVars: sidebar inputs ####
DatVars <- reactive({
  outputOptions(output, "filters", suspendWhenHidden = FALSE)
  outputOptions(output, "Variableselect", suspendWhenHidden = FALSE)
  outputOptions(output, "FishWhitingselect", suspendWhenHidden = FALSE)
  outputOptions(output, "FishWhitingselectBox", suspendWhenHidden = FALSE)
  outputOptions(output, "fisheriesOptions", suspendWhenHidden = FALSE)
  outputOptions(output, "Yearselect", suspendWhenHidden = FALSE)
  outputOptions(output, "FishAkselect", suspendWhenHidden = FALSE)
  
  # create a list of variable names used in the sidebar inputs
  dat <- DatMain()
  if (input$Sect_sel == "CV") {
    datVars <- with(
      dat,
      list(
        YEAR = 2004:currentyear,
        NRlist = nrcomponents,
        CATEGORY = c(
          "Fisheries",
          "Homeport",
          "State of homeport" = "State",
          "Vessel length class"
        ),
        inclAK = unique(inclAK),
        whitingv = c("All vessels", "Non-whiting vessels", "Whiting vessels"),
        STAT =  c(
          "Mean per vessel",
          "Mean per vessel/day",
          "Mean per vessel/metric ton caught",
          "Mean per vessel/dollar revenue",
          "Median per vessel",
          "Median per vessel/day",
          "Median per vessel/metric ton caught",
          "Median per vessel/dollar revenue",
          "Fleet-wide total",
          "Fleet-wide average/day",
          "Fleet-wide average/metric ton caught",
          "Fleet-wide average/dollar revenue"
        ),
        ##Vessel characteristics metrics##
        METRIC1 =  c(
          "Number of vessels", 
          "Vessel length", 
          "Vessel replacement value",
          "Vessel market value",
          "Vessel horsepower",
          "Number of fisheries", 
          "Proportion of revenue from catch share fishery" = "Proportion of revenue from CS fishery", 
          "Revenue diversification"
          ), 
        ##Labor metrics###
        METRIC2 = c(
          "Number of crew", 
          "Number of crew-days",
          "Crew wage per year",
          "Crew wage per day",
          "Crew wage per dollar revenue",
          "Revenue per crew-day"
          ),
        ##Other metrics###
        METRIC3 = c(
          "Days at sea", 
          "Fuel use per day", 
          "Speed while fishing", 
          "Gini coefficient", 
          "Share of landings by state",
          "Seasonality"
        ),
##When grouping by Metrics, don't include 'Share of landings by state' or 'Seasonsality'
        METRIC3a = c(
          "Days at sea", 
          "Fuel use per day", 
          "Speed while fishing"
        ),
        COSTS = c(
          'All variable costs',
            'Buyback fees',
            'Captain',
            'Cost recovery fees',
            'Crew',
            'Fuel',
            'Observers', 
            'Other variable costs', 
          'All fixed costs',
            'Fishing gear',
            'On-board equipment',
            'Other fixed costs')
        )
      )
  } else if (input$Sect_sel == "FR") {
    datVars <- with(
      dat,
      list(
        YEAR = 2004:currentyear,
        NRlist = nrcomponents,
        CATEGORY = c("Production activities" = "Fisheries", "Region", "Processor size"),
        whitingv = c(
          "All processors",
          "Whiting processors",
          "Non-whiting processors"
        ),
        STAT =  c(
          "Mean per processor",
          "Mean per processor/metric ton of groundfish products produced" = "Mean per processor/metric ton produced",
          "Mean per processor/dollar of revenue",
          "Median per processor",
          "Median per processor/metric ton of groundfish products produced" = "Median per processor/metric ton produced",
          "Median per processor/dollar of revenue",
          "Industry-wide total",
          "Industry-wide average/metric ton of groundfish products produced" = "Industry-wide average/metric ton produced",
          "Industry-wide average/dollar of revenue"
        ),
        ##Processor characteristic metrics##
        METRIC1 =  c(
          "Number of processors",
          "Number of species purchased",
          "Proportion of production value from West Coast groundfish" = "Proportion of revenue from catch share species",
          "Revenue diversification"
          ),
        ##Labor metrics##
        METRIC2 = c(
          'Number of workers',
          "Hourly compensation"
        ),
        ##Other metrics##
        METRIC3 = c(
          "Gini coefficient"),
        COSTS = c(
      "All variable costs",
          'Fish purchases',
          'Freight',
          'Labor',
          'Monitoring',
          'Off-site freezing & storage',
          'Packing materials', 
          'Utilities',
          'Other variable costs',
      "All fixed costs",
          'Buildings',
          'Equipment',
          'Other fixed costs')
      )
    )
  } else if (input$Sect_sel == "M") {
    datVars <- with(
      dat,
      list(
        YEAR = 2004:currentyear,
        NRlist = nrcomponents,
        CATEGORY = "Fisheries",
        inclAK = unique(inclAK),
        whitingv = "Whiting vessels",
        STAT =  c(
          "Mean per vessel",
          "Mean per vessel/day",
          "Mean per vessel/metric ton produced",
          "Mean per vessel/dollar of revenue",
          "Median per vessel",
          "Median per vessel/day",
          "Median per vessel/metric ton produced",
          "Median per vessel/dollar of revenue",
          "Fleet-wide total",
          'Fleet-wide average/day',
          'Fleet-wide average/metric ton produced',
          'Fleet-wide average/dollar of revenue'
        ),
        ##Vessel characteristic metrics##
        METRIC1 =  c(
          "Number of vessels",
          "Vessel length",
          "Proportion of landings from catch share fishery" =
          "Proportion of landings from CS fishery"
          ),
        ##Crew metrics##
        METRIC2 = c(
          "Number of processing and non-processing crew",
          "Crew wage per day",
          "Revenue per crew-day"
          ),
        ##Other metrics##
        METRIC3 = c(
          "Days at sea",
          "Gini coefficient",
          "Seasonality"
        ),
        ##When grouping by Metrics, don't include 'Seasonsality'
        METRIC3a = c(
          "Days at sea"
        ),
        COSTS = c(
        "All variable costs",
          "Fish purchases",
          "Fuel",
          "Non-processing crew",
          "Observers",
          "Processing crew",
          "Other variable costs",
        "All fixed costs",
          "Fishing gear",
          "On-board equipment",
          "Processing equipment",
          'Other fixed costs')
        )
      )
} else if (input$Sect_sel == "CP") {
  datVars <- with(
    dat,
    list(
      YEAR = 2004:currentyear,
      NRlist = nrcomponents,
      CATEGORY = "Fisheries",
      inclAK = unique(inclAK),
      whitingv = "Whiting vessels",
      STAT =  c(
        "Mean per vessel",
        "Mean per vessel/day",
        "Mean per vessel/metric ton produced",
        "Mean per vessel/dollar of revenue",
        "Median per vessel",
        "Median per vessel/day",
        "Median per vessel/metric ton produced",
        "Median per vessel/dollar of revenue",
        "Fleet-wide total",
        'Fleet-wide average/day',
        'Fleet-wide average/metric ton produced',
        'Fleet-wide average/dollar of revenue'
      ),
      ##Vessel characteristic metrics##
      METRIC1 =  c(
        "Number of vessels",
        "Vessel length",
        "Proportion of landings from catch share fishery" =
          "Proportion of landings from CS fishery"
      ),
      ##Crew metrics##
      METRIC2 = c(
        "Number of processing and non-processing crew",
        "Crew wage per day",
        "Revenue per crew-day"
      ),
      ##Other metrics##
      METRIC3 = c(
        "Days at sea",
        "Gini coefficient",
        "Seasonality"
      ),
      ##When grouping by Metrics, don't include 'Seasonsality'
      METRIC3a = c(
        "Days at sea"
      ),
      COSTS = c(
      "All variable costs",
        'Cost recovery fees', 
        "Fuel",
        "Non-processing crew",
        "Observers",
        "Processing crew",
        "Other variable costs",
      "All fixed costs",
        "Fishing gear",
        "On-board equipment",
        "Processing equipment",
        'Other fixed costs')
    )
  )
}
})

# Mini filtering functions to use in DatSub({}) ####
# choose the list of metrics
metricselections <- reactive({
  if(grepl('characteristics', input$Ind_sel)) {
    return(input$demSelect)
  } else if(input$Ind_sel == 'Labor') {
    return(input$crewSelect)
  } else if(input$Ind_sel == 'Cost')  {
    return(input$costSelect)
  } else if(input$Ind_sel == 'Other') {
    return(input$socSelect)
  } else if(input$Ind_sel == 'Economic') {
    return(input$econSelect)
  } else return('')
})

# choose the list of statistics
statselections <- reactive({
  if(grepl('characteristics', input$Ind_sel)) {
    return(input$demStats)
  } else if(input$Ind_sel == 'Labor') {
    return(input$crewStats)
  } else if(input$Ind_sel == 'Cost')  {
    return(input$costStats)
  } else if(input$Ind_sel == 'Other') {
    return(input$socStats)
  } else if(input$Ind_sel == 'Economic') {
    return(input$econStats)
  } else return('')
})

akselections <- reactive({
  if(any(input$demSelect %in% c('Revenue diversification', 'Proportion of revenue from CS fishery', 'Number of fisheries')) &
      !input$LayoutSelect) {
    return(ifelse(input$FishAkSelect == TRUE, 'YES', 'NO'))
  } else return('')
})

# choose the list of categories
csselections <- reactive({ 
  if(input$CategorySelect != "Fisheries") {
    return(input$inSelect)
  } else return('')
})


# DatSubTable: HUGE reactive for subsetting for data table####
# Subset data for table
# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the quoted method in dcast
DatSubRaw <- reactive({
  dat <- DatMain()

  # data filter differs whether it is CV/FR module or CP/MS module
  if (input$Sect_sel == "CV" | input$Sect_sel == "FR") {
    datSubforSector <-
      subset(dat,
        YEAR %in% seq(input$YearSelect[1], input$YearSelect[2], 1) &
          CATEGORY == input$CategorySelect &
          VARIABLE %in% input$VariableSelect &
          whitingv %in% input$FishWhitingSelect
      )
  } else {
    datSubforSector <-
      subset(dat, 
        YEAR %in% seq(input$YearSelect[1], input$YearSelect[2], 1))
  }

  # subset the sector specific data according to all of the fisheye toggles
 datSub <- subset(datSubforSector,
   METRIC %in% metricselections() &
   STAT   %in% statselections() &
   inclAK %in% akselections() &
   CS     %in% csselections())

})
  
# Format the data for the view data tab
DatSubTable <- reactive({

 datSub <- DatSubRaw()

 # table formatting for the data view tab

 datSub$sort <- 1:nrow(datSub)

 tabformatfun <- function(x) {
   rounding <- ifelse(all(datSub$unit == ''), 1, 0)
   dollar   <- ifelse(grepl('$', datSub$ylab, fixed = T), '$', '')
   paste0(dollar, prettyNum(round(x), big.mark = ','))
 }
 datSub$VALUE <-    tabformatfun(datSub$VALUE)
 datSub$VARIANCE <- tabformatfun(datSub$VARIANCE)
 datSub$q25 <-      tabformatfun(datSub$q25)
 datSub$q75 <-      tabformatfun(datSub$q75)

 Ntitle <- ifelse(input$Sect_sel == "FR", 'Number of processors', 'Number of vessels')
 valuetitle <- statselections()
 vartitle <- ifelse(statselections() %in% c('Total', ''), 'VARIANCE',
   ifelse(statselections() == 'Median', 'Mean average deviation',
     'Standard deviation'))

 # rename the columns 
 datSub <-
   rename(datSub,
     Year = YEAR,
     Metric = METRIC,
     !!quo_name(valuetitle) := VALUE,
     #Statistic = STAT,
     !!quo_name(vartitle) := VARIANCE,
     `Quartile: 25th` = q25,
     `Quartile: 75th` = q75,
     `Summary variable` = VARIABLE,  
     `Data summed across` = whitingv,  
     `Alaskan activities included` = inclAK, 
     `Delivery location` = AGID,
     !!quo_name(Ntitle) := N)
  
# need to redesign the fishak column and then this will work
  alwaysexclude <- c('metric_flag', 'conf', 'flag', 'unit', 'tab', 'ylab', 'sort', 'CATEGORY', 'STAT')
datSub <- select(datSub, colnames(datSub)[apply(datSub, 2, function(x) sum(x != '' & !is.na(x) & x != 'NA') > 0 )], 
  -alwaysexclude) 

  return(datSub)

})

# DatSub: subsets the data ####
DatSub <- reactive({
  
datSub <- DatSubRaw()
 
 # SORT ####
if (!input$LayoutSelect) {
    if (input$Ind_sel == 'Other' &&
        input$socSelect == 'Share of landings by state') {
      datSub$sort <- as.character(datSub$AGID)
    } else {
      if (input$Sect_sel == "CV") {
        if (input$CategorySelect == "Fisheries") {
          datSub$sort <- case_when(
            datSub$VARIABLE == "All fisheries" ~ 1,
            datSub$VARIABLE == "All catch share fisheries" ~ 2,
            datSub$VARIABLE == "Pacific whiting" ~ 3,
            datSub$VARIABLE == "At-sea Pacific whiting" ~ 4,
            datSub$VARIABLE == "Shoreside Pacific whiting" ~ 5,
            datSub$VARIABLE == "Groundfish with trawl gear" ~ 6,
            datSub$VARIABLE == "DTS trawl with trawl endorsement" ~ 7,
            datSub$VARIABLE == "Non-whiting midwater trawl" ~ 8,
            datSub$VARIABLE == "Non-whiting, non-DTS trawl with trawl endorsement" ~ 9,
            datSub$VARIABLE == "Groundfish fixed gear with trawl endorsement" ~ 10,
            datSub$VARIABLE == "All non-catch share fisheries" ~ 11,
            datSub$VARIABLE == "Other fisheries" ~ 12,
            datSub$VARIABLE == "Crab" ~ 13,
            datSub$VARIABLE == "Shrimp" ~ 14,  
            T ~ 15)
          
        } else if (input$CategorySelect == "Homeport") {
          
          datSub$sort <- case_when(
              datSub$VARIABLE == "Puget Sound" ~ 1,
              datSub$VARIABLE == "South and central WA coast" ~ 2,
              datSub$VARIABLE == "Astoria" ~ 3,
              datSub$VARIABLE == "Tillamook" ~ 4,
              datSub$VARIABLE == "Newport" ~ 5,
              datSub$VARIABLE == "Coos Bay" ~ 6,
              datSub$VARIABLE == "Brookings" ~ 7,
              datSub$VARIABLE == "Crescent City" ~ 8,
              datSub$VARIABLE == "Eureka" ~ 9,
              datSub$VARIABLE == "Fort Bragg" ~ 10,
              datSub$VARIABLE == "San Francisco" ~ 11, 
              T ~ 12)
        } else {
          datSub$sort <- datSub$VARIABLE
        }
      }# End CV
      else if (input$Sect_sel == 'FR') {
        if (input$CategorySelect == "Fisheries") {
          datSub$sort <- ifelse(datSub$VARIABLE == "All production", 1,
            ifelse(datSub$VARIABLE == "Non-whiting groundfish production", 2,
              ifelse(datSub$VARIABLE == "Pacific whiting production", 3, 4)
            )
          )
        } else if (input$CategorySelect == "Region") {
          datSub$sort <-
            ifelse(datSub$VARIABLE == "Washington and Oregon", 1, 2)
        } else {
          datSub$sort <- as.character(datSub$VARIABLE)
        }
      } #end FR
      else {
        datSub$sort <- "At-sea Pacific whiting"
      } #end MS and CP
    }#end not Other
  } #end not Metrics
  else {
    if (input$Ind_sel == "Economic") {
      datSub$sort <- ifelse(
        datSub$METRIC == "Revenue", 1,
        ifelse(datSub$METRIC == "Variable costs", 2,
          ifelse(datSub$METRIC == "Fixed costs", 3,
            ifelse(datSub$METRIC == "Variable Cost Net Revenue", 4,  5)
          )
        )
      )
    } else {
      datSub$sort <- as.character(datSub$METRIC)
    }
  }
 
 # end SORT ####

 return(datSub)

    })


PermitPlot <- reactive({
  if (!(
    is.null(input$YearSelect) | is.null(input$CategorySelect) |
      is.null(input$VariableSelect)
  )) {
    if (!(input$YearSelect[1]   == "" |
        input$CategorySelect[1] == "" |
        input$VariableSelect[1] == "")) {
      x <- TRUE
    } else {
      x <- FALSE
    }
  } else
    x <- FALSE
  x
  
})

#Download buttons only shows up if PermitPlot()==T
output$download_Table <- renderUI({
  if (PermitPlot()) {
    tags$div(class = "actbutton",
      downloadButton("dlTable", "Download Data Table", class = "btn btn-info"))
    #    tags$div(actionButton("", "Download Data Table coming soon",class = "btn btn-info"))
    #    }
  }
})

output$download_figure <- renderUI({
  if (PermitPlot()) {
    tags$div(class = "actbutton",
      downloadButton("dlFigure", "Download Plot(s)", class = "btn btn-info"))
    #    tags$div(actionButton("", "Download Plot(s) coming soon",class = "btn btn-info"))
  }
})

output$resetButton <- renderUI({
  if (PermitPlot()) {
    tags$div(class = "actbutton",
      actionButton(
        "reset_input",
        "Clear",
        class = "btn btn-info"
      ))
  }
})

vars2 = reactiveValues(counter = 0.5)
output$DataButton2 <- renderUI({
  if (PermitPlot()) {
    actionButton("data2", label = label2())
  }
})


###-------------Case study buttons --------------------------###
#values <- reactiveValues(shouldShow = FALSE)
#observeEvent(input$hideshow2, {
#    values$shouldShow = TRUE
#})

vars3 = reactiveValues(counter = 0.5)
observeEvent(input$hideshow1, {
  toggle("PlotMain2")
  #  hide('CaseStudyFig2')
  #  hide('PlotMain3')
  #  isolate({
  #    vars3$counter <- vars3$counter + .5
  #  })
})
vars4 = reactiveValues(counter = 0.5)
observeEvent(input$hideshow2, {
  toggle('CaseStudyFig2')
  #  hide('PlotMain2')
  #  hide('PlotMain3')
})
observeEvent(input$hideshow3, {
  toggle('PlotMain3')
  #  toggle('CaseStudyFig3')
  #  hide('PlotMain2')
  ##  isolate({
  #    vars4$counter <- vars4$counter + .5
  #  })
  #  hide('CaseStudyFig2')
})

observeEvent(input$hideshow4, {
  toggle('CaseStudyFig3')
})

observeEvent(input$hideshow5, {
  toggle('CaseStudyFig4')
})

observeEvent(input$hideshow6, {
  toggle('CaseStudyFig5')
})
###-------------End Case study buttons --------------------------###
