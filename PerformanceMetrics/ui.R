.libPaths(c("/usr/lib64/R/shiny_library", .libPaths()))

library(appFrame)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)
library(bsplus)

# custom css functions
# calls .css selector for well-sub
wellPanelSub <- function(...){div(class = "well-sub", ...)}
# calls .css selector for radioButton header
wellPanelHeading <- function(...){div(class = "well-radioHeading", ...)} 


function(request) {
  fluidPage(title = "FISHEyE",
    useShinyjs(),
    # create a CSS to modify style of validation test error (for Variability analysis)
    tags$head(tags$body(includeHTML("google-analytics.noaa.js"))),
    tags$head(
      tags$style(HTML(".shiny-output-error-validation {color: red;font-size: 120%;}")),
      tags$style(HTML(".select {margin-top:-20px}"),tags$textarea(id="message", rows=3, cols=40, "Default value")),
      #  tags$style(HTML(".navbar .nav > li { position:relative; z-index: 10000;}")),
      tags$style(HTML(".navbar {position:static}")),
      tags$style(HTML(".ckbox {margin-top: 0px; margin-bottom: -15px}")),
      tags$style(HTML(".statbox {margin-top: 0px; margin-bottom: -15px}")),
      tags$style(HTML(".radio:first-child {margin-top: 10px;}")),
      ##These are used to gray out different metrics when grouping by 'Metrics' 
      #and are labeled based on the # in seq that should be greyed out. i.e., items 2-5 are grayed out in ckbox2345
      tags$style(HTML(".ckbox2345 .checkbox:nth-child(2) label{color:grey;} .checkbox.input:nth-child(2),
                           .ckbox2345 .checkbox:nth-child(3) label{color:grey;} .checkbox.input:nth-child(3),
                           .ckbox2345 .checkbox:nth-child(4) label{color:grey;} .checkbox.input:nth-child(4),
                           .ckbox2345 .checkbox:nth-child(5) label{color:grey;} .checkbox.input:nth-child(5),
                           .ckbox23456 .checkbox:nth-child(2) label{color:grey;} .checkbox.input:nth-child(2),
                           .ckbox23456 .checkbox:nth-child(3) label{color:grey;} .checkbox.input:nth-child(3),
                           .ckbox23456 .checkbox:nth-child(4) label{color:grey;} .checkbox.input:nth-child(4),
                           .ckbox23456 .checkbox:nth-child(5) label{color:grey;} .checkbox.input:nth-child(5),
                           .ckbox23456 .checkbox:nth-child(6) label{color:grey;} .checkbox.input:nth-child(6),
                           .ckbox_1 .checkbox:nth-child(1) label{color:grey;} .checkbox.input:nth-child(1),
                           .ckbox_2 .checkbox:nth-child(2) label{color:grey;} .checkbox.input:nth-child(2),
                           .ckbox23 .checkbox:nth-child(2) label{color:grey;} .checkbox.input:nth-child(2),
                           .ckbox23 .checkbox:nth-child(3) label{color:grey;} .checkbox.input:nth-child(3),
                           .ckbox_4 .checkbox:nth-child(4) label{color:grey;} .checkbox.input:nth-child(4),
                           .ckbox34 .checkbox:nth-child(3) label{color:grey;} .checkbox.input:nth-child(3),
                           .ckbox34 .checkbox:nth-child(4) label{color:grey;} .checkbox. input:nth-child(4),
                           .StatGrey .radio:nth-child(-n+2) label{color:grey;} .radio.input:nth-child(-n+2) {border: 0px; width: 0%; height:0em;}#"
      )),
      
      tags$style(HTML(".StatGrey  .radio:first-child  input[type=radio],
                                            .StatGrey  .radio:nth-child(-n+2) input[type=radio],
                                            .StatGrey1 .checkbox:nth-child(n+2) input[type=checkbox],
                                            .StatGrey2  .radio:nth-child(3) input[type=radio],
                                            .StatGrey3  .checkbox:nth-child(n+3) input[type=checkbox],
                                            .StatGrey4  .radio:nth-child(n+4) input[type=radio],
                                            .ckbox2345  .checkbox:nth-child(5)  input[type=checkbox],
                                            .ckbox2345  .checkbox:nth-child(4)  input[type=checkbox],
                                            .ckbox2345  .checkbox:nth-child(3)  input[type=checkbox],
                                            .ckbox2345  .checkbox:nth-child(2)  input[type=checkbox],
                                            .ckbox23456  .checkbox:nth-child(2)  input[type=checkbox],
                                            .ckbox23456  .checkbox:nth-child(3)  input[type=checkbox],
                                            .ckbox23456  .checkbox:nth-child(4)  input[type=checkbox],
                                            .ckbox23456  .checkbox:nth-child(5)  input[type=checkbox],
                                            .ckbox23456  .checkbox:nth-child(6)  input[type=checkbox],
                                            .ckbox_1  .checkbox:nth-child(1)  input[type=checkbox],
                                            .ckbox_2  .checkbox:nth-child(2)  input[type=checkbox],
                                            .ckbox23  .checkbox:nth-child(2)  input[type=checkbox],
                                            .ckbox23  .checkbox:nth-child(3)  input[type=checkbox],
                                            .ckbox_4  .checkbox:nth-child(4)  input[type=checkbox],
                                            .ckbox34  .checkbox:nth-child(3)  input[type=checkbox] {border: 0px;    width: 0%;    height:0em;}#")),
      #Make checkbox and radio buttons bold    
      tags$style(HTML(".ckbox2 .checkbox:first-child label,
                                      .ckbox2 .checkbox:nth-child(2) label, 
                                      .ckbox2 .checkbox:nth-child(12) label,
                                      .ckbox3 .radio:nth-child(2) label,
                                      .ckbox3 .radio:nth-child(3) label,
                                      .ckbox3 .radio:nth-child(13) label,
                                      .ckbox3 .radio:first-child label,
                                      .rbutton .radio:nth-child(2) label,
                                      .rbutton .radio:nth-child(3) label, 
                                      .rbutton .radio:nth-child(10) label{font-weight:bold;}")),
      
      #Style appears of Cost category check boxes - indents, bolding, spacing
      tags$style(HTML(".statbox {margin-top: -30px; margin-bottom: 0px}
                                     .statboxC  .checkbox:first-child label, 
                                     .statboxC  .checkbox:first-child label,
                                     .statboxM  .checkbox:first-child label,
                                     .statboxF  .checkbox:first-child label,
                                     .statboxF  .checkbox:nth-child(10) label,
                                     .statboxC  .checkbox:nth-child(9) label,
                                     .statboxM  .checkbox:nth-child(8) label,
                                     .ckbox2 .checkbox:first-child label,
                                     .ckbox2 .checkbox:nth-child(2) label,
                                     .ckbox2 .checkbox:nth-child(11) label,

                                     .statboxC  .radio:first-child label, 
                                     .statboxC  .radio:first-child label,
                                     .statboxM  .radio:first-child label,
                                     .statboxF  .radio:first-child label,
                                     .statboxF  .radio:nth-child(10) label,
                                     .statboxC  .radio:nth-child(9) label,
                                     .statboxM  .radio:nth-child(8) label,
                                     .ckbox2 .radio:first-child label,
                                     .ckbox2 .radio:nth-child(2) label,
                                     .ckbox2 .radio:nth-child(11) label{font-weight:bold;}
                              .statboxC .checkbox:nth-child(-n+14) label,
                                    .statboxF .checkbox:nth-child(-n+14) label,
                                    .statboxM .checkbox:nth-child(-n+12) label,
                                    .ckbox2 .checkbox:nth-child(-n+14) label,
                                   .frckbox .checkbox:nth-child(3) label,
                                   .frckbox .checkbox:nth-child(4) label,

                                    .statboxC .radio:nth-child(-n+14) label,
                                    .statboxF .radio:nth-child(-n+14) label,
                                    .statboxM .radio:nth-child(-n+12) label,
                                    .ckbox2 .radio:nth-child(-n+14) label,
                                    .frckbox .radio:nth-child(3) label,
                                    .frckbox .radio:nth-child(4) label{margin-left:17px;}
                              .ckbox2 .checkbox:nth-child(4) label,
                                    .ckbox2 .checkbox:nth-child(5) label,
                                    .ckbox2 .checkbox:nth-child(7) label,
                                    .ckbox2 .checkbox:nth-child(8) label,
                                    .ckbox2 .checkbox:nth-child(9) label,

                                    .ckbox2 .radio:nth-child(4) label,
                                    .ckbox2 .radio:nth-child(5) label,
                                    .ckbox2 .radio:nth-child(7) label,
                                    .ckbox2 .radio:nth-child(8) label,
                                    .ckbox2 .radio:nth-child(9) label{margin-left:34px;}
                              .statboxM .checkbox:nth-child(1) label,
                                    .statboxC .checkbox:nth-child(1) label,
                                    .statboxF .checkbox:nth-child(1) label,
                                    .statboxC .checkbox:nth-child(9) label,
                                    .statboxF .checkbox:nth-child(10) label,
                                    .statboxM .checkbox:nth-child(8) label,
                                    .ckbox2 .checkbox:nth-child(-n+2) label,
                                    .ckbox2 .checkbox:nth-child(11) label,

                                    .statboxM .radio:nth-child(1) label,
                                    .statboxC .radio:nth-child(1) label,
                                    .statboxF .radio:nth-child(1) label,
                                    .statboxC .radio:nth-child(9) label,
                                    .statboxF .radio:nth-child(10) label,
                                    .statboxM .radio:nth-child(8) label,
                                    .ckbox2 .radio:nth-child(-n+2) label,
                                    .ckbox2 .radio:nth-child(11) label{margin-left:0px;}"
      )),
      
      tags$style(HTML(".ckbox2 .checkbox:nth-child(-n+15) label,
                                      .ckbox3 .radio:nth-child(-n+16) label,
                                      .FRprod .checkbox:nth-child(4) label,
                                      .FRprod .checkbox:nth-child(3) label,
                                      .rbutton .radio:nth-child(-n+14) label,
                                      .rbutton2 .radio:nth-child(4) label,
                                      .rbutton2 .radio:nth-child(5) label{margin-left:17px;}
                             .ckbox2 .checkbox:nth-child(5) label,
                                     .ckbox2 .checkbox:nth-child(6) label,
                                     .ckbox2 .checkbox:nth-child(8) label,
                                     .ckbox2 .checkbox:nth-child(9) label,
                                     .ckbox2 .checkbox:nth-child(10) label,
                                     .ckbox3 .radio:nth-child(6) label,
                                     .ckbox3 .radio:nth-child(7) label,
                                     .ckbox3 .radio:nth-child(9) label,
                                     .ckbox3 .radio:nth-child(10) label,
                                     .ckbox3 .radio:nth-child(11) label{margin-left:34px;}
                            .ckbox2 .checkbox:nth-child(-n +2) label,
                                     .ckbox2 .checkbox:nth-child(12) label,
                                     .ckbox3 .radio:nth-child(-n+3) label,
                                     .ckbox3 .radio:nth-child(13) label,
                                     .rbutton .radio:nth-child(-n+3) label,
                                     .rbutton .radio:nth-child(10) label{margin-left:0px;}")),

      tags$style(HTML(".StatGrey2 .radio:nth-child(3) label{color:grey;}")),
      tags$style(HTML(".StatGrey .radio:nth-child(-n+2) label{color:grey;}")),
      
      #check boxes and radio buttons that are greyed out and not usuable (because the option is not available)       
      tags$style(HTML(".StatGrey3  .checkbox:nth-child(n+3) label{margin-left:17px;color:grey;} 
                                         .checkbox.input:nth-child(n+3) {border: 0px;    width: 0%;    height:0em;}")),
      tags$style(HTML(".StatGrey3  .checkbox:nth-child(5) label{margin-left:0px;color:grey;} 
                                         .checkbox.input:nth-child(5) {border: 0px;    width: 0%;    height:0em;}")),
      tags$style(HTML(".StatGrey4  .radio:nth-child(n+4) label{margin-left:17px;color:grey;} 
                                         .radio.input:nth-child(n+4) {border: 0px;    width: 0%;    height:0em;}")),
      tags$style(HTML(".StatGrey4  .radio:nth-child(6) label{margin-left:0px;color:grey;} .radio.input:nth-child(6) {border: 0px;    width: 0%;    height:0em;}")),
      tags$style(HTML(".StatGrey1  .checkbox:nth-child(n+2) label{margin-left:0px;color:grey;} 
                                           .checkbox.input:nth-child(n+2) {border: 0px;    width: 0%;    height:0em;}")),
      
      
      #tags$style(HTML(".met_mod label{font-style:italic;margin-bottom:20px;margin-top:-90px;padding-top:0;}")),#{margin-top:-40px;margin-bottom:50px; height:12px;}
      tags$style(HTML(".met_mod  input[type=radio] {border: 0px;    width: 0%;    height:0em;}#")),
      tags$style(HTML(".sectselect >li{position:relative; z-index:10000; background-color:black !important; display:inline; vertical-align:middle;}")),
      #            tags$style(HTML(".sectselect{font-size:28px; line-height:28px;}")),
      tags$style(HTML(".actbutton {margin-bottom:5px}")),
      tags$style(HTML(".rbutton {margin-top:15px}")),
      tags$style(HTML('#iof,
                             #isummed,
                             #ifg,
                             #istat,
                             #ipo,
                             #ivs,
                             #iem,
                             #ivariance,
                             #FRr,
                             #FRs,
                             #FRi,
                             #icompare,
                             #iwhiting{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; 
                              color:RoyalBlue}'))),
    #tags$style(HTML('#iVesSum{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
    # java script 
    tags$style(type='text/css', "#data2, 
                                       #data  { background-color:RoyalBlue; color:white; height:37px;position:absolute;bottom:170%;left:425%;}"),
    #{ background-color:RoyalBlue; color:white; height:37px;position:absolute;bottom:170%;left:425%;}          
    tags$head(includeScript("google-analytics.js")),
    tags$head(tags$script(src = "message-handler.js")),
    
    tags$head(
      # Main css page, downloaded from bootswatch
      tags$link(rel="stylesheet", type="text/css", href="bootstrap.css"),
      # secondary css page with fisheye specific attributes
      tags$link(rel="stylesheet", type="text/css", href="fisheye.css"),
      tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.4.0/css/font-awesome.min.css"),
      tags$style(type="text/css", ".tab-content {overflow: visible;}")         
    ),
    
    # source("www/shiny_framebuster/framebuster.R")$value,
    ## example R framebusting code
    appFrameHeaderScrolling(),
    ### #-- Government Shutdown banner --- ###
    #tags$div(class='header', #tags$syle(HTML('background-color:white;color:blue;width:auto; text-align:left;float:center;margin:0 auto; padding: 0.3em 0 0.5em;font-size:large">
    #  tags$p("The Federal Government is closed. The site will not be updated; however NOAA websites and social media channels necessary to protect lives and property will be maintained. See", tags$a(href="https://www.weather.gov", "www.weather.gov."), "Learn more at", tags$a(href="https://noaa.gov", "NOAA.gov."))),
    ### #-- Government Shutdown banner --- ###
    # fluidRow(div(style = "padding-bottom: 5px;margin-bottom:0"),
    #   tags$h2(style = "margin-left: 15px", 
    #     HTML("<div><p style='font-size:120%'><strong><a style='color:white; background-color:#1a5d99;  font-family:Cambria; border-radius:25px; padding:5px' 
    #                           href='https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/'> FISHEyE</a>- Performance Metrics </strong></p> 
    #                       </div>")), htmlOutput("SectPrint")
    # ),
    navbarPage(id="page", collapsible=TRUE, inverse=F,
      title="",
      
      tabPanel("Explore the data", value="results",    
        sidebarLayout(
          
          sidebarPanel( 
            wellPanel( 
              fluidRow(
                column(4,
                       uiOutput("resetButton"),
                  uiOutput('Button'))),#end fluidRow
              
              # Select a vessel/Processor Type
              radioGroupButtons("Sect_sel", label = NULL,
                                choices = c('Catcher Vessels'="CV", 'Mothership Vessels'="M", 'Catcher-Processor Vessels'="CP", 'First Receivers and Shorebased Processors'="FR"),
                                individual = TRUE
              ),

              # Metrics
              tags$div(class="header collapsed", "Metric")%>% bs_attach_collapse("collapse1"),
              bs_collapse(id = "collapse1",
                          content = tags$div(column(12, uiOutput('metrics'),
                                                    style = "background:white; padding: 10px;margin-below:4px;border-color: #bce8f1;")),
                          show = TRUE
              ),

              # Filters
              conditionalPanel(condition="input.Sect_sel=='CV' || input.Sect_sel=='FR'", 
                               tags$div(class="header collapsed", "Filters")%>% bs_attach_collapse("collapse2"),
                               bs_collapse(id = "collapse2",
                                           content = tags$div(column(12, uiOutput('filters'),
                                                                     uiOutput("Variableselect"),
                                                                     style = "background:white; padding: 10px;margin-below:4px;border-color: #bce8f1;"))
                               )),

              # Additional Filters
              tags$div(class="header collapsed", "Additional Filters")%>% bs_attach_collapse("collapse3"),
              bs_collapse(id = "collapse3",
                          content = tags$div(column(12,
                                                    fluidRow(
                                                      column(6, uiOutput("FishWhitingselect")),
                                                      column(6, uiOutput("fisheriesOptions"))
                                                    ),
                                                    fluidRow(
                                                      column(6, uiOutput("Yearselect")),
                                                      column(6, uiOutput("FishAkselect"))
                                                    ),
                                                    style = "background:white; padding: 10px;margin-below:4px;border-color: #bce8f1;"))
              ) ,

              tags$div(style = "font-weight:bold; margin-bottom: 7px", "Display Options:"),
              
              # Multiple metrics
              fluidRow(
                column(12,
                       uiOutput('Layoutselect')
                )),

              conditionalPanel(condition="input.Ind_sel!='Economic'&input.AVE_MED2!='Total'&
                                                              input.socSelect!='Seasonality'&
                                                              input.socSelect!='Share of landings by state'||
                                                              input.Ind_sel=='Economic'&input.AVE_MED!='T'",
                               uiOutput("Plotselect")),

              fluidRow(
                column(4, uiOutput("download_figure")),
                column(4, uiOutput("download_Table")),
                column(4, bookmarkButton())
              )
              
            ),      style = "padding: 0px;overflow-y:scroll; max-height: 700px "), # end right side column

          mainPanel(
            tabsetPanel(id = "tabs",
                        tabPanel("Visualize the Data", value="Panel1", plotOutput("PlotMain")),
                        tabPanel("Dataset", value="Panel2", dataTableOutput("TableMain"))
            ))
          
        )),
      #tabPanel(HTML('History'), htmlOutput('HistoryText')),
      navbarMenu("Instructions",
        tabPanel("About", 
          source("external/explorer/explorerSourceFiles/about.R")$value
        ),
        
        tabPanel("Instructions", 
          source("external/explorer/explorerSourceFiles/instructions.R")$value
        ),
        tabPanel("Definitions", 
          source("external/explorer/explorerSourceFiles/definitions.R")$value
        )
      ), # end right side column     
      tabPanel(HTML('<i class="fa fa-thumb-tack" style="margin-right:2ex;display:inline-block;vertical-align:bottom;float:left;white-space:nowrap"> Bulletin Board</i>'),
        fluidRow(
          column(12, htmlOutput("BlogText")),
          column(5,  htmlOutput("BlogUpdates")),
          column(1),
          column(5,  htmlOutput("BlogResponses")))),
      tabPanel(HTML('<i class="fa fa-envelope-o fa-fw" style="margin-right:9ex;display:inline-block;vertical-align:bottom;float:left;white-space:nowrap"> Contact us</i>'),
        fluidRow(
          column(12, htmlOutput("Email")))
      ),
      tabPanel(HTML('<i class="fa fa-folder-open-o fa-fw" style="margin-right:18ex;display:inline-block;vertical-align:bottom;float:left;white-space:nowrap"> FISHEyE Applications</i>'),
        fluidRow(
          column(12, htmlOutput("ApplicationsText"))
        )),
      tabPanel(HTML('<a class="btn btn-warning", href="http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/"
                        style="height:37px;border-radius:25px;margin: -24px -50px; float:top;position:absolute;right:-100px;
                              font-familiy: Arial, Helvetica, sans-serif;font-size: 12pt; padding-top:7px;
                        padding-bottom:10px"> FISHEyE Homepage</a>' ),style='width:1000px')
    ), #end app level fluid row#, target="_blank"
    appFrameFooterScrolling()
  ) # end fluid Page
}