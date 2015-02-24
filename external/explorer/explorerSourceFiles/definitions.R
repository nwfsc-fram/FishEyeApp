tags$div(style = "margin-top: 15px; margin-bottom: 15px; width: 60%",    
  tags$div(
    p("The variables and their definitions used in 
      this application are sourced from the 2012 Economic Data Collection (EDC) report. 
      A copy of this and past reports can be found at:",
      tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data.cfm>",
      "www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data.cfm")) 
  ),  
  tags$div(style= "margin-top: 15px",
           
    p(tags$strong("Summary Variables")),
     tags$ul(
       tags$li("Fisheries:"),
         tags$p("Fisheries used for EDC reporting  to characterise 
                the US West Coast Groudfish fishery. The commercial fishery has 
                four components: limited entry with a trawl endorsement,
                limited entry with a fixed gear endorsement, open access, and tribal.
                The catch share program consists of cooperatives for the at-sea mothership (including
                catcher vessels and motherships) and catcher-processor fleets, 
                and an individual fishing quota (IFQ) program for the shorebased trawl fleet.
                This application only uses data from the Catcher Vessel sector."),
       tags$li("Vessel length class."),
         tags$p("The distribution of vessel sizes that particicapte in the West Coast
                Catch Share program were evenly divided into three categories each
                with an eqaul number of vessels."),
       tags$li("Homeport:"),
         tags$p("The homeport reported by each vessel on the EDC survey."),
       tags$li("State:"),
         tags$p("The state corresponding to each homeport.")
      ),               
    p(tags$strong("Revenue/Cost types")),
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
  p(tags$strong("Fished in AK")),
  tags$ul(
    tags$li(
      tags$p("Vessels that fished in Alaska can be filtered from the summary data.
             If this box is checked then all vessels will be included. If un-checked then
             vessels will be removed for years in which they participated in an Alaskan fishery.")
    )
  ),
  p(tags$strong("Stat")),
  tags$ul(
    tags$li("sum:"),
      tags$p("Figures are aggregated by summing across all vessel in group"),
    tags$li("mean:"),
      tags$p("Figures are aggregated by averaging across vessels in group"),
    tags$li("mean per day:"),
      tags$p("Figures are aggregated by averaging across vessels per day figures"),
    tags$li("mean per ton:"),
      tags$p("Figures are aggregated by averaging across vessels per metric ton delivered figures")
  )
)