
################################
#Other applications
################################
output$ApplicationsText <- renderUI({
  tags$div(style="margin:15px 15px 30px; with: 60%",
           tags$br(),
  HTML('<a class="btn btn-primary", href="http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/"
                        style="height:37px;border-radius:25px;font-familiy: Arial, Helvetica, sans-serif;font-size: 12pt; padding-top:7px;
       padding-bottom:10px"> FISHEyE Net Revenue Explorer</a>' ),
  HTML('<a class="btn btn-primary", href="http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/Costs/"
                        style="height:37px;border-radius:25px;font-familiy: Arial, Helvetica, sans-serif;font-size: 12pt; padding-top:7px;
       padding-bottom:10px"> FISHEyE Costs Explorer</a>' )
  )
})
####################################

####################################
#Contact us
######################################
output$Email <- renderUI({
  withTags(
  div(style = "margin: 15px 15px 30px; width: 60%",
           h3("Contact us"),
           p("We look forward to receiving feedback and questions.", br()),
           p(h4("Please email us at", strong("nmfs.nwfsc.fisheye@noaa.gov")),
                  # tags$a(href="mailto:nwfsc.fisheye@noaa.gov?subject=FISHEyE", 'nwfsc.fisheye@noaa.gov'),
                  br()),

           p(
             "Erin Steiner", br(),
             "Economist", br(),
             "206-860-3202", br(),
             "Northwest Fisheries Science Center", br(),br(),
             #                    "nwfsc.fisheye@noaa.gov",

             "Ashley Vizek", br(),
             "Contractor-ECS Federal, Inc.", br(),
             "In support of NMFS", br(),
             "Northwest Fisheries Science Center", br(),
             #                    "nwfsc.fisheye@noaa.gov", tags$br(),
             br(),
             hr(),

             # "You can send comments and questions directly to us by clicking",
             #                 tags$a(href="mailto:nwfsc.fisheye@noaa.gov?subject=FISHEyE", 'contact us'), 'or by copying our email address',
             #                 tags$em('nwfsc.fisheye@noaa.gov'), 'and using your favorite email program.',
             br(),
             br()
           ))
  ) #end withTags
})
####################################

####################################
#Blog page intro text
####################################
output$BlogText <- renderUI({
  withTags(
  div(style = "margin-top: 15px;margin-bottom:0; width: 80%; ",
           h3("Bulletin Board"),
           p("On this page we will provide information on updates and responses to questions received by email that are of general interest."),
           hr()
  )
  ) #end withTags
}) #        br(),
####################################

#       ,
####################################
#Blog
####################################
output$BlogUpdates <- renderUI({ 
  withTags(
  div(style = "margin-top:0; padding-top:0;background-color:#F8F8E9;",
           h3("Updates"),
      # 2019 vessel data
      div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
       border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
           # step 1: update date
           HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Jan</span><br />
            <span class='day' style='font-size:16px;'>6</span><br />
            <span class='year' style='font-size:11px;line-height: 10px;'>2021</span>")
      ),
      # step 2: update title
      p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>2019 vessel data now available</span>")), 
      # step 3: update message
      p(br(),"2019 data are now available for catcher vessels, catcher processors, and motherships."),
      hr(),
      # New FR metrics
      div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
       border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
           # step 1: update date
           HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Aug</span><br />
            <span class='day' style='font-size:16px;'>13</span><br />
            <span class='year' style='font-size:11px;line-height: 10px;'>2020</span>")
      ),
      # step 2: update title
      p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>NEW metrics!</span>")), 
      # step 3: update message
      p(br(),"The first receiver and shorebased processors sector now includes new metrics including more detailed cost categories, wages for production and non-production employees, and more!"),
      hr(),
      # 2018 data for cv, ms, cp
      div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
       border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
           # step 1: update date
           HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Dec</span><br />
            <span class='day' style='font-size:16px;'>11</span><br />
            <span class='year' style='font-size:11px;line-height: 10px;'>2019</span>")
      ),
      # step 2: update title
      p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>2018 vessel data now available</span>")), 
      # step 3: update message
      p(br(),"2018 data are now available for catcher vessels, catcher processors, and motherships."),
      hr(),
      ##2017 FR data added
      div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
       border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
           # step 1: update date
           HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Dec</span><br />
            <span class='day' style='font-size:16px;'>02</span><br />
            <span class='year' style='font-size:11px;line-height: 10px;'>2019</span>")
      ),
      # step 2: update title
      p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>2017 FR data now available</span>")), 
      # step 3: update message
      p(br(),"2017 data are now available for First Receivers."),
      hr(),
      ##Impacts added
      div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
       border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
           # step 1: update date
           HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Jun</span><br />
            <span class='day' style='font-size:16px;'>17</span><br />
            <span class='year' style='font-size:11px;line-height: 10px;'>2019</span>")
      ),
      # step 2: update title
      p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>West Coast Impacts added</span>")), 
      # step 3: update message
      p(br(),"We added West Coast employment and income impacts for catcher vessels. 
        All impacts are for the West Coast overall. More information can be found here: https://www.nwfsc.noaa.gov/assets/25/1620_08012011_142237_InputOutputModelTM111WebFinal.pdf"),
      hr(),
      #Fresh new look
      div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
       border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
           # step 1: update date
           HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Jun</span><br />
            <span class='day' style='font-size:16px;'>12</span><br />
            <span class='year' style='font-size:11px;line-height: 10px;'>2019</span>")
      ),
      # step 2: update title
      p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Fresh new look</span>")), 
      # step 3: update message
      p(br(),"We gave FISHEyE a fresh new look! We phased out the Net Revenue and Costs applications and integrated the data into the Performance Metrics app.
        We also added a few new metrics. In the 'Vessel characteristics' tab (previously, 'Demographics') we added: (1) vessel replacement value, (2) vessel market value, 
        (3) vessel horsepower, and (4) vessel fuel capacity. The 'Other' tab includes the following new metrics: (1) fuel use per day, and (2) speed while fishing. 
        The 'Labor' tab includes the following new metrics: (1) number of crew-days, (2) crew wage per year, and (3) crew wager per dollar revenue."),
      hr(),
  # 2017 CV + CP + MS data loaded ####
  div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
       border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
    # step 1: update date
       HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Feb</span><br />
            <span class='day' style='font-size:16px;'>02</span><br />
            <span class='year' style='font-size:11px;line-height: 10px;'>2019</span>")
       ),
    # step 2: update title
  p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>2017 data now available</span>")), 
    # step 3: update message
  p(br(),"2017 data are now available for Catcher Vessels, Motherships, and Catcher Processors. Data collection and quality control for First Receivers is still ongoing, data will be posted as soon as all issues are resolved."),
  hr(),
  # Data updates
  div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
       border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
       HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>May</span><br />
            <span class='day' style='font-size:16px;'>15</span><br />
            <span class='year' style='font-size:11px;line-height: 10px;'>2018</span>")
       ),
  p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Data updated</span>")), 
  p(br(),"Data are periodically updated. Please check back here for updates on data. Data last updated on 05/14/2018."),
  hr(),
  #Major update - adding other sectors
  
  div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
       border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
       HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Oct</span><br />
            <span class='day' style='font-size:16px;'>12</span><br />
            <span class='year' style='font-size:11px;line-height: 10px;'>2017</span>")
       ),
  p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Bookmarking feature</span>")), 
  p(br(),"We have added a bookmarking feature. The bookmarking feature allows you to return to a previously saved state, share the output of a query on FISHEyE without downloading the plot, continue your data exploration at a later date without having to remember selections, etc. A url will be provided when you click the bookmark button on the top right hand corner of control panel. This url is specific to the selections you made in the control panel."),
  hr(),
  
  div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
            HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>July</span><br />
                 <span class='day' style='font-size:16px;'>12</span><br />
                 <span class='year' style='font-size:11px;line-height: 10px;'>2017</span>")
  ),
  p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Percentiles around the median</span>")), 
  p(br(),"We have been asked to provide percentiles as a measure of variance. All plots and tables now show the 25th and 75th percentiles when median is selected and the standard deviation when mean is selected."),
  hr(),
  

  div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
            HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Jun</span><br />
                 <span class='day' style='font-size:16px;'>23</span><br />
                 <span class='year' style='font-size:11px;line-height: 10px;'>2017</span>")
            ),
  p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Updates to economic metrics calculations</span>")), 
  p(br(),"We have updated the analysis of the economic metrics. We now provide the fleet-wide or industry-wide average rather than the 
         total for rate measurements (per day, per metric ton). The fleet-wide average is calculated as the sum of the economic measure divided by the
         sum of days at sea or metric tons."),
  hr(),

  div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
            HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Dec</span><br />
                 <span class='day' style='font-size:16px;'>02</span><br />
                 <span class='year' style='font-size:11px;line-height: 10px;'>2016</span>")
            ),
  p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>New sectors!</span>")), 
  p(br(),"We have added motherships, catcher-processors, and first receivers and shorebased processors."),
  hr(),
  
div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                     HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Nov</span><br />
                          <span class='day' style='font-size:16px;'>02</span><br />
                          <span class='year' style='font-size:11px;line-height: 10px;'>2016</span>")
                     ),
           p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Improved functionality!</span>")), 
           p(br(),"Several major updates have been made to this application. We have provided updated data for catcher vessels.
                  In addition, we revised the functionality of the application. You can now compare by vessel or by metrics. 
                  When comparing metrics, the metrics displayed will depend upon the selected statistics."),
           hr(),
   # Welcome to fisheye
           div( class='date', style='height:45px;width:30px;font-family:Arial;font-weight:bold;background-color:#ffffff;text-align:center;border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                     HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Jun</span><br />
                          <span class='day' style='font-size:16px;'>20</span><br />
                          <span class='year' style='font-size:11px;line-height: 10px;'>2016</span>")
                     ),
           p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Welcome to the Performance Metrics application!</span>")), 
           p(br(),
                  "Welcome to FISHEyE Performance Metrics. If you have any difficulties accessing or using this application, please contact us at nwfsc.fisheye@noaa.gov. 
                  Your comments will help us improve performance of this application."),
           br(),
           
           hr()#, 
           
           )#,
    ) #end withTags
})
#    tags$p("10.21.15"),
#              tags$ul(   tags$li("Started this blog"),
#     tags$li("Added a", tags$em('clear all'), 'button')),
#   tags$p("10.28.15"),
#   tags$ul(
#     tags$li("Restructured this bulletin board.")
#  ),
#        HTML("<hr color='red'/>"),
####################################

####################################
#FAQ
####################################
output$BlogResponses <- renderUI({   
  withTags(
  div(style = "background-color:#F8F8E9",
           h3("Responses to questions"),
           #       tags$ul(
           #          tags$li("We look forward to receiving feedback and questions. Please send questions and feedback to", tags$strong('nwfsc.fisheye@noaa.gov.'))
           #       ),
           div( class='date', style='height:45px;width:30px;font-family:Arial;font-weight:bold;background-color:#ffffff;text-align:center;border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                     HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>July</span><br />
                          <span class='day' style='font-size:16px;'>12</span><br />
                          <span class='year' style='font-size:11px;line-height: 10px;'>2017</span>")
           ),
           p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>What exactly is the proportion of revenue from catch share fishery showing?</span>")), 
           p(br(),
                  'Vessels participate in multiple fisheries. The presented values are the proportion of revenue from ALL catch share fisheries for the subsetted vessels. If you check the "Include Alaskan fisheries activities" box, then revenue from Alaska (for the subsetted vessels) will be included in the denominator.',
                  br()),
           

   div( class='date', style='height:45px;width:30px;font-family:Arial;font-weight:bold;background-color:#ffffff;text-align:center;border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                     HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Feb</span><br />
                          <span class='day' style='font-size:16px;'>17</span><br />
                          <span class='year' style='font-size:11px;line-height: 10px;'>2016</span>")
                     ),
           p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Have port groups been combined?</span>")), 
           p(br(),
                  'For many homeports, there are relatively few vessels that fish for groundfish. To maximize the amount of data we can display while retaining geographic variation, we have aggregated vessles into port groups.  For instance, vessels that reported Morro Bay and Monterey ports as their homeports have been grouped in a single homeport.',
                  br()),
           
           div( class='date', style='height:45px;width:30px;font-family:Arial;font-weight:bold;background-color:#ffffff;text-align:center;border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                     HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Feb</span><br />
                          <span class='day' style='font-size:16px;'>8</span><br />
                          <span class='year' style='font-size:11px;line-height: 10px;'>2016</span>")
                     ),
           p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Check out other FISHEyE applications. </span>")), 
           p(br(),
                  "This is the second FISHEyE application. The first application, the Net Revenue Explorer, can be accessed through the", a(href="https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye", 'FISHEyE homepage'),'.',
                  br()),
           
           hr())
  )#end withTags
  
})
####################################


####################################
#Default Plot Text
####################################
output$DefaultPlotText <- renderUI({
  if(PermitPlot()) return()
  withTags(
  div(style = "margin: 15px 15px 30px; width: 60%",
           h3("Summary Plots and Data"),
           p('The performance metrics in this web application provide a set of measures that can be used to gauge and track changes in the 
              West Coast groundfish trawl limited entry fishery  following implementation of the',
              a(href="http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html", 'West Coast Groundfish Trawl Catch Share Program ', target="_blank"),
              'in 2011. Select whether to view results for catcher vessels, motherships, catcher-processors, or first receivers and shorebased processors.
              Metrics are grouped into four broad indicator categories: vessel or processor characteristics, economic, labor, and other. Information on how metrics were chosen can be found in the',
              em('About'), 'tab.',
              strong('Information on the individual metrics can be found by clicking the blue information icon above the list of metrics'), 'or in the Definitions tab.' 
            ),
           p(strong("To get started, make at least one selection in each of the fields in the Control Panel.")),
           #          tags$br(),
           HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.8em; margin-top:15px; margin-bottom:-2px;font-size:11.5pt'>
                <b>Plot Options:</b></div>"),
           
          
           p(strong('Show Plot(s):'),'The', em('Show Plot(s)'), 'button returns the display to the plot output. Pre- and post- implementation of the catch share program 
                is distinguished with shading. You can select whether or not to show the variance around the mean or median values.'
             ),
            br(),
           img(src="indicators_example2.png", height=300),  #  img(src="EconInd4.png", height=350),       
           br(), 
           br(),
           p(strong('Show Data:'), 'The', em('Show Data'), 'button displays a data table that can be filtered using the Search box or the boxes at the bottom of the table.'),
           p(strong('Download Plots and Data:'),'Once selections have been made and a plot or data table is visible, use these buttons to download the plot and the data 
             used to generate the plot.'),
           p(strong('To view these instructions'), 'at any time, visit the', em('Instructions'), 'tab.')
      ) # End DIV
      ) #end withTags
})
####################################

####################################
#Get Started Text
####################################
output$GetStartedText <- renderUI({
  #if(PermitPlot()) return()
#  withTags(
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$p(strong(paste('Note this tutorial will disappear once you select a',input$CategorySelect,'. Selections can be made in any order.'))),
           tags$h4("1. Select a sector."),
           tags$p("Select whether to show results for catcher vessels, mothership vessels, catcher-processor vessels, or first receivers and shorebased processors."),
           tags$h4("2. Compare groups of vessels/processors or metrics"),
           tags$p('Choose whether to view a single metric for multiple vessel or processor groups or view multiple metrics for a single vessel or processor group.'), 
           tags$h4("3. Select general categories to view data by"),
           tags$p("First, select the vessel or processor grouping category (Fisheries, State, Homeport, Vessel length class, Production activities, Region, Processor size). 
                  Categories will depend upon the selected sector."),
           tags$p("Second, for catcher vessels, select whether to subset the data by whether", tags$a(href='WhitingFigure.png','vessels fished for whiting or not.', target='_blank'), 
                  "You can show results for all vessels, just vessels that fished for pacific whiting, and/or just vessels that did not fish for pacific whiting.
                  For first receivers and shorebased processors, select whether to show results for all processors, just processors that process whiting, and/or 
                  just processors that process non-whiting species. For motherships and catcher-processors, only the pacific whiting category is available."),
           tags$h4("4. Select one more more subgroups."),
           tags$p(strong('Once this selection is made, this tutorial will disappear. This selection can be made last.'), 
                  'Multiple subgroups of the selected vessel or processor grouping category (such as fishery) can be selected if, in step 1, you chose
                        to compare groups of vessels or processors. If you chose to compare metrics, only a single subgroup can be selected.'),
           tags$h4("5. Select an indicator category"),
           tags$p("This drop down menu divides metrics into four categories:",em('Vessel or processor characteristics,'), em("Economic,"), em("Labor,"), 'and',em('Other.'), 
             'The choice of metrics to be displayed depends on which indicator category is selected.'),
           tags$h4("6. Select a statistic and a metric"),
           tags$p('Which statistic is calculated depends on the metric. If a selected metric is not shown, try a different statistic.'),
           tags$h4('7. Select years')#,
          # tags$p('The number of years for which data is available and the choice to include activities in Alaskan fisheries depend upon the metric and sector selected.')
  )#End DIV
#  )#end withTags
})
####################################


####################################
#Case Study text
####################################
#1st paragraph
output$CaseStudyp1 <- renderUI({
  withTags(
         div(style = "margin: 15px 15px 30px; width: 90%",
           p(em('(All plots displayed can be generated using FISHEyE, see italics for directions on how to create figure.)')),
          p(style='line-height:70%',"We can measure 'profitability' with two general indicators:",
                 ul(style='line-height:75%', "Annual", strong("variable"), "cost net revenue (VCNR; a measure of shorter-term operating profit that includes only variable costs)"),
                 ul(style='line-height:75%',"Annual", strong("total"), "cost net revenue (TNCR; a longer-term measure of profitability that includes both fixed and variable costs)")),
          p("In the first five years of the catch share program, VCNR and TCNR have generally been greater from trawl gear non-whiting 
                    groundfish activities than from fixed gear activities. The only exception is 2011. The figure below shows VCNR for the median vessel (black line) and the range of VCNR  (25th to 75th percentiles; in grey). The percentiles are a statistical representation of vessels that make more than the lowest 25% of vessels and less than the highest 25%."), 
                 
          p("Although the median vessel reported a profit in all years for both fisheries activities, around 25% of vessels were not making an operating provit in their fixed gear operations for 2013-2015 (25th percentile of VCNR was negative). They may, however, have profits in other activities. The means illustrate a similar trend.",
                br(), em(style='font-size:85%', "Using the selection panel on the right, you can toggle between the median and mean outcomes (under 'Statistic'). "),br(),
                 "Note that we do not compare 2009-2010; the outcomes of fixed gear vessels fishing in these years are not directly comparable because they were fishing under the Nature Conservancy Exempted Fishing Permit." 
)
)
)#end withTags
})
#2nd paragraph
    output$CaseStudyp6 <- renderUI({
      withTags(
      div(style = "margin: 15px 15px 30px; width: 90%",
               h4('Further exploration: effort and profits per-day'),
          p("To more fully understand the differences between the two fisheries activities, we can also look at the amount of effort (days at sea) and per-day profit levels in each fishery.", 
          br(),em(style='font-size:85%',"See the plot below or use the selection panel on the right. In the selection panel, click on the Demographic option under 'Select an indicator category', then select Days at Sea under 'Select a metric', and select Median under 'Statistic'. The figure will show in the uppermost plot on this page."),br(),
          "Vessels in the trawl gear fishery spend more days fishing for groundfish than vessels with fixed gear. The distribution is also broader for trawl gear, meaning that there is greater variation in the number of days fished with trawl gear. Across the displayed range (25th to 75th percentiles), vessels spend about 40-75 days per year fishing groundfish with trawl gear, compared to about 25-40 days with fixed gear."),
          p("Per day at sea, the median VCNR is again higher for trawl gear activities, with the exception of 2011, when sablefish prices were very high. However, the distribution of outcomes is much larger for fixed gear activities. This means that there is more variation in operating profits between vessels in fixed gear than in trawl gear activities. For instance, in 2015, the majority of vessels (those within the 25th to 75th percentiles) in the trawl gear fishery made between $2000 and $5000 of VCNR per day fishing. For fixed gear activities, the vessels between the 25th and 75th percentile maade between -$300 and $5000 per day.", 
                 br(),em(style='font-size:85%',"To view VCNR per day, open the figure below or click on Economic under 'Select an indicator category' and select VCNR under 'Economic measures', and then select Median per vessel/day under 'Statistic'.")
    )
    )
      )#end withTags
  })
#Third paragraph
    output$CaseStudyp2 <- renderUI({
        withTags(div(style = "margin: 15px 15px 30px; width: 90%",
                  p(h4("Further exploration: Focusing on trawl gear, how has net revenue changed over time?")),
                  p("For  trawl gear non-whiting groundfish activities, the median VCNR", em('(see uppermost plot)'), "has been relatively stable at around $100,000 per year. In contrast, TCNR has increased over the time-period shown, although TCNR was negative for the lowest 25% of earners in some years", 
                         br(),em(style='font-size:85%','(click on TCNR in the panel on the right to see this figure in the uppermost plot above).'), br(),
                         "TCNR has increased more than VCNR because of consolidation, meaning that there has been a decrease in the number of vessels  fishing",br(),
em(style='font-size:85%',"(click on  Demographic under 'Select an indicator category' and then select the  Number of Vessels metric in the selection panel on the right to see this figure in the plot above)."),br(),
" As the number of vessels decreases, fewer vessels are catching a larger proportion of the catch and incurring fewer fixed costs, leading to greater overall efficiency. In addition to consolidation, these vessels are fishing fewer days in the groundfish trawl fishery, which likely leads to lower costs and greater efficiency. Although revenue and costs have changed relatively little on an annual basis, revenue per day of fishing has increased, meaning that the fleet is experiencing the same level of profitability despite fishing fewer days.", br(),em(style='font-size:85%',"To view this figure in the uppermost plot, click on Economic under 'Select an indicator category', Revenue under 'Economic measures', and Median per vessel/day under 'Statistic'.")
),

                  p("There is considerable heterogeneity within the catch share fishery. For instance, in 2015 the median revenue per vessel within the trawl gear fishery ranged between $132,000 (25th percentile) and $657,000 (75th percentile). To view the heterogeneity within the fishery, we group vessels annually into three categories (33.3rd, 66.6th and 100th percentiles) based on revenue earned", em(style='font-size:85%','(see plot below).'), "For the top third of revenue earners, median net revenue (VCNR or TCNR) per vessel has increased over time. For the middle revenue earners, median TCNR has fluctuated around $100,000 per vessel per year and VCNR has slightly increased from $41,000 in 2009 to $70,000 in 2015. However, the lower third of revenue earners have not experienced similar increases. Median VCNR per vessel has fluctuated around $29,000 per year and median TCNR per vessel has fluctuated around $0 per year. This plot shows that net revenue has increased more for the highest revenue earners than the lower revenue earners."), 
br(),em(style='font-size:85%',"This figure came from the Thirds Analysis in the "),
a(href="http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/",  " Net Revenue Explorer application.", target="_blank"))
)#end with tags
})

    output$CaseStudyp5 <- renderUI({            
    withTags(
      div(style = "margin: 15px 15px 30px; width: 90%",
             h4("Further exploration: changes in fishing costs using the Costs Explorer Application"),
             p("The first plot below shows fleet-wide average expense per dollar of revenue. It is shown for all fixed costs and for three individual fixed cost categories. 
             The statistic is calculated as the sum of the costs across the fleet divided by revenue summed over all vessels and is used to evaluate how costs affect net revenue.
             Net revenue tends to be lower when fleet-wide average cost per dollar of revenue is higher. For the groundfish trawl gear fishery, all fixed costs (darkest blue line) have decreased over time. Fishing gear and other fixed costs have remained relatively stable over time. On-board equipment (includes machinery and equipment not used to harvest or process fish) can include large costs such as motors and new vessels and has varied, and was highest in 2012. This corresponds with a dip in TCNR in 2012. Variable costs (shown in the figure on the right) fluctuated little over the timeframe observed, suggesting that changes in net revenue have resulted primarily from changes in revenue and on-board equipment costs.",
                    br(),em(style='font-size:85%',"This figure came from the  "),
                    a(href="http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/Costs/",  " Costs Explorer application.", target="_blank"))
      )            
             ) #end withTags 
    })
  

  output$CaseStudyFig2 <- renderUI({
    tags$img(src="TrawlThirds.png", height=325)
  })
  output$CaseStudyFig3 <- renderUI({  
    tags$img(src="TCNRTrawl.png", height=350)
  })
  output$CaseStudyFig3 <- renderUI({  
    tags$img(src="FixedTrawlThirds.png", height=320)
  })
  output$CaseStudyFig4 <- renderUI({  
    tags$img(src="TrawlCosts.png", height=325)
  })
  output$CaseStudyFig5 <- renderUI({  
    tags$img(src="TrawlDAS.png", height=525)
  })
####################################
  
####################################
# CS History Text
####################################
output$HistoryText <- renderUI({
  withTags(
    div(style = "margin: 15px 15px 30px; width: 60%",
        img(src="Timeline.png", height=475),
        br(),br(),
        p('The Groundfish Trawl Catch Share Program was implemented in 2011 with the goal to:',
          ul(em('Create and implement a capacity rationalization plan that increases net economic benefits, create individual economic stability, provide for full utilization of the trawl 
                sector allocation, consider environmental impacts, and achieve individual accountability of catch and bycatch. 	(Amendment 20 FEIS, page 5)')),
          'Data used in FISHEyE primarily comes from the Economic Data Collection (EDC), a mandatory annual cost/earnings survey for participating vessels. 
          Baseline data were collected in 2009 and 2010.',
        br(),br(),
        'Even with two years of baseline data, it is difficult to distinguish the direct effects of the catch share prorgram from contextual factors other than management 
        that have changed and may have affected data in both the baseline period and the catch share period. 
        For instance, the EDC baseline period was marked by historically low TAC for whiting, whereas TAC increased by 66%, on average, during the catch share period. 
        Anomalous ocean conditions, changing world markets, rebuilding species, high fuel prices in 2011-2012 and other management related costs are all other underlying factors 
        that have changed before and since the catch share program.',
        br(),br(),
      'The figure above shows several key factors that have changed along with management.', strong(h4('Click on the figure above to learn more.'))
      )#end Paragraph
    )#end DIV
  )#end withTags
})
########################################