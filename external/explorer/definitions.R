tags$div(
  p(style="text-align:justify",'The variables used in this application are sourced from the Economic Data Collection (EDC) 2011 report.'),
  p(style="text-align:justify", HTML("<strong>Topics</strong>")),
  tags$ul(
    tags$li("Revenue:"),    
      tags$p("Ex-vessel revenue is available for all shoreside deliveries, but is not
            available for at-sea deliveries. EDC data are used for all at-sea delivery revenues. Additional sources of revenue include
            revenue from sale or lease of permits, quota shares, and quota
            pounds, and from other activities like chartering and research. See Table 8.1 of the 2012 EDC report for revenue source."),
    
    tags$li("Variable cost:"),
      tags$p("Variable costs vary with the level of fishery participation, and generally include items such
            as fuel and crew payments. See Table 9.1 of the 2012 EDC report for variable cost categories"),
    
    tags$li("Fixed cost:"),
      tags$p("Fixed costs are costs that do not vary with the level of fishing participation. The EDC tracks fixed costs for three categories: New and used vessel and on-board equipment (Includes all electronics, safety equipment,
          and machinery not used to harvest fish, but not fishing gear or processing equipment), Fishing gear (Includes nets, doors, traps, pots, cables, and fishing machinery used for the
          West Coast fisheries) and Processing Equipment (Includes any equipment used to process or head and gut fish
          on-board the vessel)."),
    
    tags$li("Variable cost net revenue:"),
      tags$p(HTML("Variable cost net revenue is calculated as: <i>West coast Revenue - West Coast variable cost</i>. Variable cost net revenue is useful to examine changes in fishery
          operations that are not so great as to affect fixed costs. For example, the cost of fishing an
          additional day, or catching an additional metric ton of fish, is better represented by only
          considering variable costs.")),
    
    tags$li("Total cost net revenue:"),
      tags$p(HTML("Total cost net revenue is calculated as <i>West Coast Revenue - (West Coast variable cost + West Coast fixed cost)</i>. Total cost net revenue is usually a better summary measure of
          financial gain or loss for an entire year, season, or fishery."))
  ),
  p(stype="text-align:justify", HTML("<strong>Categories</strong>")),
  tags$ul(
    tags$li("Fisheries:"),
      tags$p("The predominate fishing sectors included in EDC reporting."),
    tags$li("Vessel length class"),
      tags$p("A categorical variable for vessel length."),
    tags$li("Homeport:"),
      tags$p("The homeport reported by each vessel on the EDC survey."),
    tags$li("State:"),
      tags$p("The state corresponding to each homeport.")
    )
)