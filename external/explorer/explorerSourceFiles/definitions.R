tags$div(style = "margin: 15px 15px 30px; width: 60%",    
  tags$div(
    p("The variables and their definitions used in 
      this application are sourced from the 2012 Economic Data Collection (EDC) report. 
      A copy of this and past reports can be found at:",
      tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data.cfm>",
      "www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data.cfm")) 
  ),  
  tags$div(style= "margin-top: 15px;",
           
   h4("Summary Variables"),
     tags$ul(
       tags$li("Fisheries:"),
         tags$p('Fisheries used for EDC reporting to characterise 
                the US West Coast Groudfish fishery. 
                The catch share program consists of cooperatives for the at-sea mothership (including
                catcher vessels and motherships) and catcher-processor fleets, 
                and an individual fishing quota (IFQ) program for the shorebased trawl fleet.
                This application only uses data from the Catcher Vessel sector.
                Many vessels that participate in the catch share program also 
                participate in other fisheries.'),
         p(tags$i('Catch Share fisheries include'),': At-sea Pacific whiting; 
                Shoreside Pacific whiting;
                Groundfish fixed gear with fixed gear endorsement; 
                Groundfish fixed gear with trawl endorsement;
                DTS* trawl with trawl endorsement;
                Non-whiting, Non-DTS* trawl with trawl endorsement.'),
         p(tags$i('Non-catch share fisheries include:'), 
                'Groundfish fixed gear with fixed gear endorsement;
                Crab; Shrimp; Other fisheries.'),
         p('*DTS = Dover, Thornyhead and Sole')
        ,
       tags$li("Vessel length class."),
         tags$p("Three classes of vessel size representing the range of catcher vessel size."),
       tags$li("Homeport:"),
         tags$p("The homeport reported by each vessel on the EDC survey."),
       tags$li("State:"),
         tags$p("The state corresponding to each homeport.")
      ),               
   h4("Economic measures"),
      tags$ul(
        tags$li("Revenue:"),    
          tags$p("There are several sources of earnings for vessels on the West Coast. 
                 The primary source is revenue from sale of fish. Ex-vessel revenue 
                 is available for all shoreside deliveries, but is not available 
                 for at-sea deliveries. EDC data are used for all at-sea delivery 
                 revenues. Additional sources of revenue include
                 revenue from sale or lease of permits, quota shares, and quota
                 pounds, and from other activities like chartering and research. 
                 See Table 8.1 of the 2012 EDC report for revenue source."),      
        tags$li("Variable cost:"),
          tags$p("Variable costs are costs which vary with the the level of participation,
             in a fishery and generally include items such as fuel and crew payments. 
             See Table 9.1 of the 2012 EDC report for the full listing of 
             variable cost categories"),
        tags$li("Fixed cost:"),
           tags$p("Fixed costs are costs that do not vary with the level of fishing participation. 
             The EDC tracks fixed costs for three categories: New and used vessel 
             and on-board equipment (Includes all electronics, safety equipment,
             and machinery not used to harvest fish, but not fishing gear or processing equipment), 
             Fishing gear (Includes nets, doors, traps, pots, cables, and fishing machinery used for the
             West Coast fisheries) and Processing Equipment (Includes any 
             equipment used to process or head and gut fish
             on-board the vessel)."),
        tags$li("Variable cost net revenue:"),
           tags$p("Variable cost net revenue is calculated as", 
             tags$i("West coast Revenue - West Coast variable cost"), 
               "Variable cost net revenue is useful to examine changes in fishery
               operations that are not so great as to affect fixed costs. 
               For example, the cost of fishing an additional day, or catching 
               an additional metric ton of fish, is better represented by only
               considering variable costs."),
        tags$li("Total cost net revenue:"),
          tags$p("Total cost net revenue is calculated as", 
             tags$i("West Coast Revenue - (West Coast variable cost + West Coast fixed cost)"),
             "Total cost net revenue is usually a better summary measure of
              financial gain or loss for an entire year, season, or fishery.")
      )  
  ),
 h4("Fished in AK"),
  tags$ul(
    tags$li(
      tags$p("Vessels that fished in Alaska can be filtered from the summary data.
             If this box is checked then all vessels will be included. If un-checked then
             vessels will be removed for years in which they participated in an Alaskan fishery.")
    )
  ),
 h4("Summary statistic"),
  tags$ul(
    tags$li("Total:"),
    tags$p("Values are aggregated by summing across all vessels."),
    tags$li("mean:"),
      tags$p("Values are aggregated by averaging across vessels."),
    tags$li("mean per day:"),
      tags$p("Averaging across vessels per day values"),
    tags$li("mean per ton:"),
      tags$p(" are aggregated by averaging across vessels per metric ton delivered figures")
  ),
 h4("N"),
 tags$ul(
   tags$li("N is the number of observations for each data value.")
  )
)
