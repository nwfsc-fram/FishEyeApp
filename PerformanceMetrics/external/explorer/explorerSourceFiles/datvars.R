load("PerformanceMetrics/data/CVperfmetrics.RData") 
load("PerformanceMetrics/data/Mperfmetrics.RData") 
load("PerformanceMetrics/data/CPperfmetrics.RData")
load("PerformanceMetrics/data/FRperfmetrics.RData") 

currentyear <- 2019
currentyearFR <- 2018
nrcomponents <- c('Revenue', 'Variable costs', 'Fixed costs', 'Variable cost net revenue', 'Total cost net revenue')

# CV 
  datVars_cv <- with(
    CVperfmetrics,
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
        'Number of vessels',
        "Vessel length", 
        "Vessel replacement value",
        "Vessel market value",
        "Vessel horsepower",
        "Vessel fuel capacity",
        "Number of fisheries", 
        "Proportion of ex-vessel revenue from catch share fishery" = "Proportion of ex-vessel revenue from CS fishery", 
        "Revenue diversification"
      ), 
      ##Labor - crew metrics###
      METRIC2 = c(
        "Number of crew", 
        "Number of crew-days",
        'Crew payments',
        "Crew wage per year",
        "Crew wage per day",
        "Crew wage per dollar revenue",
        "Revenue per crew-day"
      ),
      # Labor - captain metrics #
      METRIC2a = c(
        'Captain wage per year',
        'Captain wage per day',
        'Captain wage per dollar revenue'
      ),
      ##Other metrics###
      METRIC3 = c(
        "Days at sea",
        'Trips',
        "Landed weight",
        "Fuel use per day", 
        "Annual fuel use",
        "Speed while fishing",
        "Gini coefficient", 
        "Share of landings by state",
        "Seasonality"
      ),
      ##When grouping by Metrics, don't include 'Share of landings by state'
      METRIC3a = c(
        "Days at sea", 
        'Trips',
        "Landed weight",
        "Fuel use per day", 
        "Annual fuel use",
        "Speed while fishing"
      ),
      COSTS = c(
        'All variable costs',
        'Buyback fees',
        'Labor',
        'Cost recovery fees',
        'Fuel',
        'Observers/EM', 
        'Other variable costs', 
        'All fixed costs',
        'Fishing gear',
        'On-board equipment',
        'Other fixed costs'),
      IMPACT = c(
        'Income impacts',
        'Employment impacts'
      )
    )
  )
  
save(datVars_cv, file = "PerformanceMetrics/data/datvars_cv.RData")  
  
# FR
  datVars_fr <- with(
    FRperfmetrics,
    list(
      YEAR = 2004:currentyearFR,
      NRlist = c('Revenue', 'Seafood sales revenue', 'Offload revenue', 'Custom processing and other revenue',
                 'Variable costs', 'Fixed costs', 'Variable cost net revenue', 'Total cost net revenue'),
      CATEGORY = c("Production activities" = "Fisheries", "Region", "Processor size"),
      whitingv = c(
        "All processors",
        "Whiting processors",
        "Non-whiting processors"
      ),
      STAT =  c(
        "Mean per processor",
        "Mean per processor/metric ton produced",
        "Mean per processor/dollar of revenue",
        "Median per processor",
        "Median per processor/metric ton produced",
        "Median per processor/dollar of revenue",
        "Industry-wide total",
        "Industry-wide average/metric ton produced",
        "Industry-wide average/dollar of revenue"
      ),
      ##Processor characteristic metrics##
      METRIC1 =  c(
        'Number of processors',
        "Number of species processed",
        "Number of species sold",
        "Revenue diversification",
        "Proportion of production value from West Coast groundfish",
        'Number of processors who fillet non-whiting groundfish'
      ),
      ##Labor metrics##
      METRIC2 = c(
        'Average number of production employees per month',
        'Max number of production employees per month',
        'Production employee payments',
        'Hourly compensation per production employee',
        'Ratio of wage to value added in production including custom processing'
      ),
      METRIC2a = c(
        'Number of non-production employees',
        'Non-production employee payments',
        'Annual compensation per non-production employee'
      ),
      ##Other metrics##
      METRIC3 = c(
        "Gini coefficient",
        'Percentage of purchases from non-vessel sources',
        'Percentage of production processed'),
      METRIC3a = c(
        "Gini coefficient",
        'Percentage of purchases from non-vessel sources',
        'Percentage of production processed'),
      COSTS = c(
        'All variable costs',
        'Fish purchases',
        'Additives',
        'Production Supplies',
        'Freight & trucking',
        'Labor',
        'Monitoring',
        'Taxes',
        'Offloading',
        'Off-site freezing & storage',
        'Packing materials',
        'Electricity',
        'Gas',
        'Waste & Byproduct Disposal',
        'Water',
        'Other variable costs',
        'All fixed costs',
        'Buildings',
        'Equipment',
        'Other fixed costs')
    )
  )

  save(datVars_fr, file = "PerformanceMetrics/data/datvars_fr.RData") 
  
# MS
  datVars_ms <- with(
    Mperfmetrics,
    list(
      YEAR = 2004:currentyear,
      NRlist = nrcomponents,
      CATEGORY = "Fisheries",
      inclAK = unique(inclAK),
      whitingv = "Whiting vessels",
      STAT =  c(
        "Mean per vessel",
        "Mean per vessel/day",
        "Mean per vessel/metric ton purchased",
        "Mean per vessel/metric ton produced",
        "Mean per vessel/dollar of revenue",
        "Median per vessel",
        "Median per vessel/day",
        "Median per vessel/metric ton purchased",
        "Median per vessel/metric ton produced",
        "Median per vessel/dollar of revenue",
        "Fleet-wide total",
        'Fleet-wide average/day',
        'Fleet-wide average/metric ton purchased',
        'Fleet-wide average/metric ton produced',
        'Fleet-wide average/dollar of revenue'
      ),
      ##Vessel characteristic metrics##
      METRIC1 =  c(
        'Number of vessels',
        "Vessel length",
        'Vessel replacement value',
        'Vessel market value',
        'Vessel horsepower',
        'Vessel fuel capacity',
        "Proportion of landings from catch share fishery" =
          "Proportion of landings from CS fishery"
      ),
      ##processing crew metrics##
      METRIC2 = c(
        'Number of processing crew',
        'Number of processing crew-days',
        'Processing crew payments',
        'Processing crew wage per year',
        'Processing crew wage per day',
        'Processing crew wage per dollar revenue'),
      ##non-processing crew metrics
      METRIC2a = c(
        'Number of non-processing crew',
        'Number of non-processing crew-days',
        'Non-processing crew payments',
        'Non-processing crew wage per year',
        'Non-processing crew wage per day',
        'Non-processing crew wage per dollar revenue'
      ),
      ##Other metrics##
      METRIC3 = c(
        'Days fishing, processing, and steaming in AK',
        'Days fishing, processing, and steaming on the WC',
        'Days offloading on the WC',
        'Days steaming between the WC and AK', 
        'Purchase weight (Alaska)',
        'Purchase weight (West Coast)',
        'Fuel use per day',
        "Gini coefficient",
        "Seasonality"
      ),
      ##When grouping by Metrics, don't include 'Seasonsality'
      METRIC3a = c(
        'Days fishing, processing, and steaming in AK',
        'Days fishing, processing, and steaming on the WC',
        'Days offloading on the WC',
        'Days steaming between the WC and AK', 
        'Purchase weight (Alaska)',
        'Purchase weight (West Coast)',
        'Fuel use per day',
        'Gini coefficient',
        'Seasonality'
      ),
      COSTS = c(
        "All variable costs",
        "Fish purchases",
        "Fuel",
        "Labor",
        "Observers",
        "Other variable costs",
        "All fixed costs",
        "Fishing gear",
        "On-board equipment",
        "Processing equipment",
        'Other fixed costs'),
      IMPACT = c("")
    )
  )
  
  save(datVars_ms, file = "PerformanceMetrics/data/datvars_ms.RData") 

# CP
  
  datVars_cp <- with(
    CPperfmetrics,
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
        "Mean per vessel/metric ton caught",
        "Mean per vessel/dollar of revenue",
        "Median per vessel",
        "Median per vessel/day",
        "Median per vessel/metric ton produced",
        "Median per vessel/metric ton caught",
        "Median per vessel/dollar of revenue",
        "Fleet-wide total",
        'Fleet-wide average/day',
        'Fleet-wide average/metric ton produced',
        'Fleet-wide average/metric ton caught',
        'Fleet-wide average/dollar of revenue'
      ),
      ##Vessel characteristic metrics##
      METRIC1 =  c(
        'Number of vessels',
        "Vessel length",
        'Vessel replacement value',
        'Vessel market value',
        'Vessel horsepower',
        'Vessel fuel capacity',
        "Proportion of landings from catch share fishery" =
          "Proportion of landings from CS fishery"
      ),
      ##processing crew metrics##
      METRIC2 = c(
        'Number of processing crew',
        'Number of processing crew-days',
        'Processing crew payments',
        'Processing crew wage per year',
        'Processing crew wage per day',
        'Processing crew wage per dollar revenue'),
      ##non-processing crew metrics
      METRIC2a = c(
        'Number of non-processing crew',
        'Number of non-processing crew-days',
        'Non-processing crew payments',
        'Non-processing crew wage per year',
        'Non-processing crew wage per day',
        'Non-processing crew wage per dollar revenue'
      ),
      ##Other metrics##
      METRIC3 = c(
        'Days fishing, processing, and steaming in AK',
        'Days fishing, processing, and steaming on the WC',
        'Days offloading on the WC',
        'Days steaming between the WC and AK',
        'Catch weight (Alaska)',
        'Catch weight (West Coast)',
        'Fuel use per day',
        "Gini coefficient",
        "Seasonality"
      ),
      ##When grouping by Metrics, don't include 'Seasonsality'
      METRIC3a = c(
        'Days fishing, processing, and steaming in AK',
        'Days fishing, processing, and steaming on the WC',
        'Days offloading on the WC',
        'Days steaming between the WC and AK',
        'Catch weight (Alaska)',
        'Catch weight (West Coast)',
        'Fuel use per day',
        'Gini coefficient',
        'Seasonality'
      ),
      COSTS = c(
        "All variable costs",
        'Cost recovery fees', 
        "Fuel",
        "Labor",
        "Observers",
        "Other variable costs",
        "All fixed costs",
        "Fishing gear",
        "On-board equipment",
        "Processing equipment",
        'Other fixed costs'),
      IMPACT = c("")
    )
  )
  save(datVars_cp, file = "PerformanceMetrics/data/datvars_cp.RData") 
  