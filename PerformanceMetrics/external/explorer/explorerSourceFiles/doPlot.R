doPlot <- function(dat, x, y) {
  if (PermitPlot()) {
    #removeNAs
    #dat <- subset(dat, is.na(dat$VALUE) == FALSE)

# create sort2 column ####
    dat$sort2 <- if (input$LayoutSelect != "Metrics") {
      if (input$Ind_sel == 'Other') {
        if (input$socSelect == 'Share of landings by state') {
          reorder(dat$AGID, dat$sort)
        } else {
          reorder(dat$VARIABLE, dat$sort)
        }
      } else {
        reorder(dat$VARIABLE, dat$sort)
      }
    } else {
      if (input$Ind_sel == "Economic") {
        reorder(dat$SHORTDESCR, dat$sort)
      } else {
        reorder(dat$METRIC, dat$sort)
      }
    }
    
    dat$thresh <-  if (input$Ind_sel == "Economic") {
      data.frame(dat %>% group_by(SHORTDESCR) %>% mutate(threshold = length(table(YEAR[YEAR <=
          2010])))) %>% subset(select = c(threshold))
    }    else if (input$Ind_sel != "Economic") {
      if (input$LayoutSelect == "Metrics") {
        if (input$PlotSelect == T & !is.na(max(dat$VARIANCE))) {
          data.frame(dat %>% group_by(METRIC) %>% mutate(
            threshold = max(VALUE, na.rm = T) + max(VARIANCE, na.rm = T) + max(VALUE, na.rm =
                T) / 10
          )) %>%
            subset(select = c(threshold))
        } else {
          data.frame(dat %>% group_by(METRIC) %>% mutate(threshold = max(VALUE, na.rm =
              T) + max(VALUE, na.rm = T) / 10)) %>% subset(select = c(threshold))
        }
      } else {
        0
      }
    }
    
    rectvars <-
      dat %>% distinct(sort2, YEAR) %>% group_by(sort2) %>% mutate(
        minx = min(as.numeric(YEAR)),
        xmaxscale = length(YEAR[YEAR < 2011]),
        maxx = max(YEAR)
      )  %>%
      subset(select = c(sort2, minx, xmaxscale, maxx)) %>% data.frame() %>% distinct %>%
      merge(dat %>% distinct(sort2, whitingv))
    
    
    #      if(input$PlotSelectOption=="Standard deviation or Median average deviation")
    dat$upper <-
      if (input$Ind_sel == "Economic") {
        if (input$AVE_MED == 'A') {
          dat$VALUE + dat$VARIANCE
        } else if (input$AVE_MED == 'T') {
          dat$VALUE
        } else {
          dat$q75
        }
      } else if (input$Ind_sel != "Economic") {
        if (input$AVE_MED2 == 'Mean') {
          dat$VALUE + dat$VARIANCE
        } else if (input$AVE_MED2 == 'Total') {
          dat$VALUE
        } else {
          dat$q75
        }
      }
    
    
    dat$lower <-
      #      if(input$PlotSelectOption=="Standard deviation or Median average deviation")
      if (input$Ind_sel == "Economic") {
        if (input$AVE_MED == 'A') {
          dat$VALUE - dat$VARIANCE
        } else  {
          dat$q25
        }
      } else if (input$Ind_sel != "Economic") {
        if (input$AVE_MED2 == 'Mean') {
          dat$VALUE - dat$VARIANCE
        } else  {
          dat$q25
        }
      }

    upper <- function() {
      #      if(input$PlotSelectOption=="Standard deviation or Median average deviation")
      if (input$Ind_sel == "Economic") {
        if (input$PlotSelect == T) {
          if (input$AVE_MED == 'A') {
            max(dat$VALUE + dat$VARIANCE)
          } else {
            max(dat$q75)
          }
        } else {
          max(dat$VALUE)
        }
      } else if (input$Ind_sel != "Economic") {
        if (input$AVE_MED2 == 'Mean' & input$PlotSelect == T) {
          max(dat$VALUE + dat$VARIANCE)
        } else if (input$AVE_MED2 == 'Median' &
            input$PlotSelect == T) {
          max(dat$q75)
        } else {
          max(dat$VALUE)
        }
      }
    }
    
    lower <- function() {
      #      if(input$PlotSelectOption=="Standard deviation or Median average deviation")
      if (input$Ind_sel == "Economic") {
        if (input$AVE_MED == 'A') {
          dat$VALUE - dat$VARIANCE
        } else  {
          dat$q25
        }
      } else if (input$Ind_sel != "Economic") {
        if (input$AVE_MED2 == 'Mean') {
          dat$VALUE - dat$VARIANCE
        } else  {
          dat$q25
        }
      }
    }
  
    yr <- function() {
      return(unique(as.numeric(dat$YEAR)))
    }
    print(yr())
    groupVar <- "whitingv"
    
# set colors for whiting/non-whiting/all lines ####
    colourThirds <- if (input$Sect_sel != "FR") {
      c(
        'Non-whiting vessels' = "#d7191c",
        'Whiting vessels' = "#2b83ba",
        'All vessels' = "#000000"
      )
    } else {
      c(
        'Non-whiting processors' = "#d7191c",
        'Whiting processors' = "#2b83ba",
        'All processors' = "#000000"
      )
    }

   # Plot header construction ####
    # title
    plot.title <- function() {
      if (input$Sect_sel == "CV") {
        return("Performance Metrics for West Coast Catcher Vessels")
      } else if (input$Sect_sel == "M") {
        return("Performance Metrics for West Coast Mothership Vessels")
      } else if (input$Sect_sel == "CP") {
        return("Performance Metrics for West Coast Catcher-Processor Vessels")
      } else if (input$Sect_sel == "FR") {
        return("Performance Metrics for West Coast First Receivers and Shorebased Processors")
      }
    }
    
gv <- function() {
      if (input$LayoutSelect != "Metrics") {
        if (input$Ind_sel == "Economic") {
          if (!input$Sect_sel %in% c("CV", "FR")) {
            sprintf(
              paste(
                "Economic measure:",
                dat$SHORTDESCR[1],
                "     Statistic: ",
                input$StatSelect
              )
            )
          } else{
            if (input$CategorySelect == "Fisheries") {
              sprintf(
                paste(
                  "Economic measure:",
                  dat$SHORTDESCR[1],
                  "     Statistic: ",
                  input$StatSelect
                )
              )
            } else {
              sprintf(
                paste(
                  "Economic measure:",
                  dat$SHORTDESCR[1],
                  "     Statistic: ",
                  input$StatSelect,
                  "    Summed across:",
                  input$inSelect
                )
              )
            }
          }
        }#end economic
        else {
          # if(input$MetricSelect[1]!='Number of vessels'&input$MetricSelect!="Share of landings by state"&input$MetricSelect!='Gini coefficient'&input$MetricSelect!='Herfindahl-Hirschman Index'&input$MetricSelect!='Seasonality'&input$MetricSelect!="Vessel length"){
          if (max(dat$metric_flag) == 0) {
            if (input$Ind_sel == "Vessel characteristics" ||
                input$Ind_sel == 'Processor characteristics') {
              if (!input$Sect_sel %in% c("CV", "FR")) {
                sprintf(
                  paste(
                    "Category:",
                    input$CategorySelect,
                    "     Metric: ",
                    input$demSelect,
                    "   Statistic:",
                    input$AVE_MED2
                  )
                )
              } else{
                if (input$CategorySelect == "Fisheries") {
                  sprintf(
                    paste(
                      "Category:",
                      input$CategorySelect,
                      "     Metric: ",
                      input$demSelect,
                      "   Statistic:",
                      input$AVE_MED2
                    )
                  )
                } else {
                  sprintf(
                    paste(
                      "Category:",
                      input$CategorySelect,
                      "     Metric: ",
                      input$demSelect,
                      "   Statistic:",
                      input$AVE_MED2,
                      "    Summed across:",
                      input$inSelect
                    )
                  )
                }
              } ##End Vessel characteristics
            } else if (input$Ind_sel == 'Labor') {
              if (!input$Sect_sel %in% c("CV", "FR")) {
                sprintf(
                  paste(
                    "Category:",
                    input$CategorySelect,
                    "     Metric: ",
                    input$demSelect,
                    "   Statistic:",
                    input$AVE_MED2
                  )
                )
              } else{
                if (input$CategorySelect == "Fisheries") {
                  sprintf(
                    paste(
                      "Category:",
                      input$CategorySelect,
                      "     Metric: ",
                      input$demSelect,
                      "   Statistic:",
                      input$AVE_MED2
                    )
                  )
                } else {
                  sprintf(
                    paste(
                      "Category:",
                      input$CategorySelect,
                      "     Metric: ",
                      input$demSelect,
                      "   Statistic:",
                      input$AVE_MED2,
                      "    Summed across:",
                      input$inSelect
                    )
                  )
                }
              } ##End Crew
            } else if (input$Ind_sel == "Other") {
              if (input$socSelect == "Share of landings by state")  {
                if (input$CategorySelect != "Fisheries") {
                  sprintf(
                    paste(
                      "Variable:",
                      input$VariableSelect,
                      "     Metric: ",
                      input$socSelect,
                      '  Statistic: Percentage   Summed across:',
                      input$inSelect
                    )
                  )
                } else {
                  sprintf(
                    paste(
                      "Variable:",
                      input$VariableSelect,
                      "     Metric: ",
                      input$socSelect,
                      "  Statistic: Percentage"
                    )
                  )
                }
              } else {
                if (input$Sect_sel == "CV" &
                    input$CategorySelect != "Fisheries" |
                    input$Sect_sel == 'FR' & input$CategorySelect != "Fisheries") {
                  sprintf(
                    paste(
                      "Category:",
                      input$CategorySelect,
                      "     Metric: ",
                      input$socSelect,
                      "   Statistic:",
                      input$AVE_MED2,
                      "    Summed across:",
                      input$inSelect
                    )
                  )
                } else {
                  sprintf(
                    paste(
                      "Category:",
                      input$CategorySelect,
                      "     Metric: ",
                      input$socSelect,
                      "   Statistic:",
                      input$AVE_MED2
                    )
                  )
                }
              }
            }
          }# end normal cases
          else {
            if (input$Ind_sel == "Vessel characteristics" ||
                input$Ind_sel == 'Processor characteristics') {
              if (input$demSelect %in% c("Number of vessels", "Number of processors")) {
                if (input$Sect_sel == "CV" &
                    input$CategorySelect != "Fisheries" |
                    input$Sect_sel == 'FR' & input$CategorySelect != "Fisheries") {
                  sprintf(
                    paste(
                      "Category:",
                      input$CategorySelect,
                      "     Metric: ",
                      input$demSelect,
                      '  Statistic: Total     Summed across:',
                      input$inSelect
                    )
                  )
                } else {
                  sprintf(
                    paste(
                      "Category:",
                      input$CategorySelect,
                      "     Metric: ",
                      input$demSelect,
                      "  Statistic: Total"
                    )
                  )
                }
              } else if (input$demSelect == "Vessel length")  {
                if (input$Sect_sel == "CV" &
                    input$CategorySelect != "Fisheries" |
                    input$Sect_sel == 'FR' & input$CategorySelect != "Fisheries") {
                  sprintf(
                    paste(
                      "Category:",
                      input$CategorySelect,
                      "     Metric: ",
                      input$demSelect,
                      '  Statistic: Average maximum length      Summed across:',
                      input$inSelect
                    )
                  )
                } else {
                  sprintf(
                    paste(
                      "Category:",
                      input$CategorySelect,
                      "     Metric: ",
                      input$demSelect,
                      "  Statistic: Average maximum length"
                    )
                  )
                }
              }
              else {
                if (input$Sect_sel == 'CV' &
                    input$CategorySelect != "Fisheries" |
                    input$Sect_sel == 'FR' & input$CategorySelect != "Fisheries") {
                  sprintf(
                    paste(
                      "Category:",
                      input$CategorySelect,
                      "     Metric: ",
                      input$demSelect,
                      '  Statistic: Index value',
                      "    Summed across:",
                      input$inSelect
                    )
                  )
                } else {
                  sprintf(
                    paste(
                      "Category:",
                      input$CategorySelect,
                      "     Metric: ",
                      input$demSelect,
                      '  Statistic: Index value'
                    )
                  )
                }
              }
            }
            else if (input$Ind_sel == "Other") {
              if (input$socSelect == "Seasonality") {
                if (input$Sect_sel == 'CV' &
                    input$CategorySelect != "Fisheries" |
                    input$Sect_sel == 'FR' & input$CategorySelect != "Fisheries") {
                  sprintf(
                    paste(
                      "Category:",
                      input$CategorySelect,
                      "     Metric: ",
                      input$socSelect,
                      '  Statistic: Day of year  Summed across:',
                      input$inSelect
                    )
                  )
                } else {
                  sprintf(
                    paste(
                      "Category:",
                      input$CategorySelect,
                      "     Metric: ",
                      input$socSelect,
                      "  Statistic: Day of year"
                    )
                  )
                }
              } else if (input$socSelect == "Share of landings by state")  {
                if (input$CategorySelect != "Fisheries") {
                  sprintf(
                    paste(
                      "Variable:",
                      input$VariableSelect,
                      "     Metric: ",
                      input$socSelect,
                      '  Statistic: Percentage   Summed across:',
                      input$inSelect
                    )
                  )
                } else {
                  sprintf(
                    paste(
                      "Variable:",
                      input$VariableSelect,
                      "     Metric: ",
                      input$socSelect,
                      "  Statistic: Percentage"
                    )
                  )
                }
              }
              else {
                if (input$Sect_sel == 'CV' &
                    input$CategorySelect != "Fisheries" |
                    input$Sect_sel == 'FR' & input$CategorySelect != "Fisheries") {
                  sprintf(
                    paste(
                      "Category:",
                      input$CategorySelect,
                      "     Metric: ",
                      input$socSelect,
                      '  Statistic: Index value',
                      "    Summed across:",
                      input$inSelect
                    )
                  )
                } else {
                  sprintf(
                    paste(
                      "Category:",
                      input$CategorySelect,
                      "     Metric: ",
                      input$socSelect,
                      '  Statistic: Index value'
                    )
                  )
                }
              }
            }
            
          } #END HERE
        } #END NOT ECONOMIC
      } #end compare vessel groupings
      else {
        if (input$Ind_sel == "Economic") {
          if (input$Sect_sel == 'CV' &
              input$CategorySelect != "Fisheries" |
              input$Sect_sel == 'FR' & input$CategorySelect != "Fisheries") {
            sprintf(
              paste(
                input$CategorySelect,
                ":",
                input$VariableSelect,
                "     Statistic: ",
                input$StatSelect,
                "    Summed across:",
                input$inSelect
              )
            )
          } else {
            sprintf(
              paste(
                input$CategorySelect,
                ":",
                input$VariableSelect,
                "     Statistic: ",
                input$StatSelect
              )
            )
          }
        } else {
          if (max(dat$metric_flag == 0)) {
            if (input$Sect_sel == 'CV' &
                input$CategorySelect != "Fisheries" |
                input$Sect_sel == 'FR' & input$CategorySelect != "Fisheries") {
              sprintf(
                paste(
                  input$CategorySelect,
                  ":",
                  input$VariableSelect,
                  " Statistic:",
                  input$AVE_MED2,
                  "  Summed across:",
                  input$inSelect
                )
              )
            } else {
              sprintf(
                paste(
                  input$CategorySelect,
                  ":",
                  input$VariableSelect,
                  " Statistic:",
                  input$AVE_MED2
                )
              )
            }
          }
          else {
            if (input$Sect_sel == 'CV' &
                input$CategorySelect != "Fisheries" |
                input$Sect_sel == 'FR' & input$CategorySelect != "Fisheries") {
              sprintf(
                paste(
                  input$CategorySelect,
                  ":",
                  input$VariableSelect,
                  "  Summed across:",
                  input$inSelect,
                  "  Statistic:",
                  input$AVE_MED2
                )
              )
            } else {
              sprintf(
                paste(
                  input$CategorySelect,
                  ":",
                  input$VariableSelect,
                  '  Statistic:',
                  input$AVE_MED2
                )
              )
            }
            
          }
        }
      }
    }
    
    # combine title and gv for printing
    main <- function() {
      bquote(atop(.(plot.title()), .(gv())))
    }

    # y-axis label ####
    ylab <- function() {
      if (input$Ind_sel == "Economic") {
        if (input$StatSelect != 'Mean per vessel/metric ton caught' &
            input$StatSelect != 'Median per vessel/metric ton caught' &
            input$StatSelect != 'Fleet-wide average/metric ton caught' &
            input$StatSelect != 'Mean per processor/metric ton produced' &
            input$StatSelect != 'Median per processor/metric ton produced' &
            input$StatSelect != 'Industry-wide average/metric ton produced') {
          paste("Thousands of",
            currentyear,
            "$",
            "(",
            input$StatSelect,
            ")")
        } else {
          paste(currentyear, "$", "(", input$StatSelect, ")")
        }
      } else if (input$Ind_sel == "Other") {
        if (input$LayoutSelect != 'Metrics') {
          if (input$socSelect == "Revenue per crew-day" |
              input$socSelect == "Revenue per position-day") {
            paste("Thousands of",
              currentyear,
              "$",
              "(",
              input$AVE_MED2,
              ")")
          }  else if (input$socSelect == "Seasonality") {
            expression(bold("Day of year when 50% of catch was landed"))
          }  else if (input$socSelect == "Share of landings by state") {
            expression(bold("Share of landings (% of revenue)"))
          }  else if (input$socSelect == "Hourly compensation") {
            expression(bold("Hourly compensation ($)"))
          }  else if (input$socSelect == "Gini coefficient") {
            expression(bold("Gini coefficient (0 - 1)"))
          } else if (input$socSelect == "Fuel use per day") {
            expression(bold("Fuel use per day (in gallons)"))
          } else if (input$socSelect == 'Speed while fishing') {
            expression(bold("Speed while fishing (in knots)"))
          } else {
            input$socSelect
          }
        } else {
          expression(bold('Scale and units depend upon metric'))
        }
      } else if (input$Ind_sel == "Vessel characteristics" ||
                 input$Ind_sel == 'Processor characteristics') {
        if (input$LayoutSelect != 'Metrics') {
          if (input$demSelect == "Proportion of revenue from CS fishery") {
            expression(bold("Proportion of revenue from catch share fishery"))
          }  else if (input$demSelect == "Number of fisheries") {
            expression(bold("Number of fisheries"))
          }  else if (input$demSelect == "Vessel length") {
            expression(bold("Vessel length (in feet)"))
          }  else {
            input$demSelect
          }
        } else {
          expression(bold('Scale and units depend upon metric'))
        }
      } else if (input$Ind_sel == 'Labor') {
        if (input$LayoutSelect != 'Metrics') {
          if(input$crewSelect == "Crew wage per day") {
            paste("Thousands of",
                  currentyear,
                  "$",
                  "(",
                  input$AVE_MED2,
                  ")")
          } else {
            input$crewSelect
          }
      } else {
        expression(bold('Scale and units depend upon metric'))
      }
        }
    }
    
    
    # confidentiality messages ####
    supp_obs <- function() {
      "\n
      \nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality."
    }
    conf_mess <- function() {
      "\n
      \nSee the confidentiality section under the ABOUT tab for more information."
    }
    supp_whiting <- function() {
      if (input$Sect_sel != "FR") {
        "\n
        \nYour selection would reveal confidential data because the categories selected are additive. \nIn these cases, only results for 'All vessels' have been shown."
      } else {
        "\n
        \nYour selection would reveal confidential data because the categories selected are additive. \nIn these cases, only results for 'All processors' have been shown."
      }
      }
    supp_obs_whiting <- function() {
      if (input$Sect_sel != 'FR') {
        "\n
        \nAdditionally, your selection would reveal confidential data because the categories selected are additive. \nIn these cases, only results for 'All vessels' have been shown."
      } else {
        "\n
        \nAdditionally, your selection would reveal confidential data because the categories selected are additive. \nIn these cases, only results for 'All processors' have been shown."
      }
      }
    supp_metric <- function() {
      "\n
      \nSome metrics may not be shown because the selected statistic is not calculated for that metric."
      #For the Gini Coefficient, the index value is shown, regardless of the statistic selectd.\n
      #   For number of vessels, only the total number of vessels is shown. For seasonality, the day when 50% of catch was landded is always shown.
      # \nFor Total Vessel length, we show maximum length. To protect confidentiality this value is the average of the longest three vessels."
    }
    
    # x-axis label ####
    # xlab is actually "notes"
    xlab <- function() {
      if (input$LayoutSelect != "Metrics") {
        if (input$Ind_sel == "Economic") {
          if (max(dat$conf) == 0) {
            if (max(dat$flag) == 0) {
              ""
            } else {
              paste(supp_obs())
            }
          } else {
            if (max(dat$flag) == 0) {
              paste(supp_whiting(), conf_mess())
            }  else {
              paste(supp_obs(), supp_obs_whiting(), conf_mess())
            }
          }
        }
        else if (input$Ind_sel == "Vessel characteristics" ||
                 input$Ind_sel == 'Processor characteristics') {
          if (input$demSelect == "Number of fisheries" |
              input$demSelect == "Proportion of revenue from CS fishery") {
            if (max(dat$conf) == 0) {
              if (max(dat$flag) == 0) {
                if (input$CategorySelect == "Fisheries" & input$Sect_sel == "CV") {
                  paste(
                    "For individual fisheries and the",
                    input$demSelect,
                    "metric, we show all activities for vessels that fished in the selected fisheries, \nnot just their activity in the selected fishery. \nFor example, the",
                    if (length(input$VariableSelect) > 2) {
                      input$VariableSelect[3]
                    } else if (length(input$VariableSelect) == 2) {
                      input$VariableSelect[2]
                    } else {
                      ""
                    },
                    "plot above shows the",
                    input$AVE_MED2,
                    input$demSelect,
                    "for\n all vessels that fished for",
                    if (length(input$VariableSelect) > 2) {
                      input$VariableSelect[3]
                    } else if (length(input$VariableSelect) == 2) {
                      input$VariableSelect[2]
                    } else {
                      input$VariableSelect[1]
                    },
                    "."
                  )
                } else {
                  ""
                }
              } else {
                if (input$CategorySelect == "Fisheries" & input$Sect_sel == "CV") {
                  paste(
                    "For individual fisheries and the",
                    input$demSelect,
                    "metric, we show all activities for vessels that fished in the selected fisheries, \nnot just their activity in the selected fishery. \nFor example, the",
                    if (length(input$VariableSelect) > 2) {
                      input$VariableSelect[3]
                    } else if (length(input$VariableSelect) == 2) {
                      input$VariableSelect[2]
                    } else {
                      ""
                    },
                    "plot above shows the",
                    input$AVE_MED2,
                    input$demSelect,
                    "for\n all vessels that fished for",
                    if (length(input$VariableSelect) > 2) {
                      input$VariableSelect[3]
                    } else if (length(input$VariableSelect) == 2) {
                      input$VariableSelect[2]
                    } else {
                      input$VariableSelect[1]
                    },
                    ".",
                    supp_obs()
                  )
                } else {
                  paste(supp_obs())
                }
              }
            } else {
              if (max(dat$flag) == 0) {
                if (input$CategorySelect == "Fisheries" & input$Sect_sel == "CV") {
                  paste(
                    "For individual fisheries and the",
                    input$demSelect,
                    "metric, we show all activities for vessels that fished in the selected fisheries, \nnot just their activity in the selected fishery. \nFor example, the",
                    if (length(input$VariableSelect) > 2) {
                      input$VariableSelect[3]
                    } else if (length(input$VariableSelect) == 2) {
                      input$VariableSelect[2]
                    } else {
                      ""
                    },
                    "plot above shows the",
                    input$AVE_MED2,
                    input$demSelect,
                    "for\n all vessels that fished for",
                    if (length(input$VariableSelect) > 2) {
                      input$VariableSelect[3]
                    } else if (length(input$VariableSelect) == 2) {
                      input$VariableSelect[2]
                    } else {
                      input$VariableSelect[1]
                    },
                    ".",
                    supp_whiting(),
                    conf_mess()
                  )
                } else{
                  paste(supp_whiting(), conf_mess())
                }
              } else {
                if (input$CategorySelect == "Fisheries" & input$Sect_sel == "CV") {
                  paste(
                    "For individual fisheries and the",
                    input$demSelect,
                    "metric, we show all activities for vessels that fished in the selected fisheries, \nnot just their activity in the selected fishery. \nFor example, the",
                    if (length(input$VariableSelect) > 2) {
                      input$VariableSelect[3]
                    } else if (length(input$VariableSelect) == 2) {
                      input$VariableSelect[2]
                    } else {
                      ""
                    },
                    "plot above shows the",
                    input$AVE_MED2,
                    input$demSelect,
                    "for\n all vessels that fished for",
                    if (length(input$VariableSelect) > 2) {
                      input$VariableSelect[3]
                    } else if (length(input$VariableSelect) == 2) {
                      input$VariableSelect[2]
                    } else {
                      input$VariableSelect[1]
                    },
                    ".",
                    supp_obs(),
                    supp_obs_whiting(),
                    conf_mess()
                  )
                } else {
                  paste(supp_obs(),
                    supp_obs_whiting(),
                    conf_mess())
                }
              }
            }
          } else  {
            if (max(dat$conf) == 0) {
              if (max(dat$flag) == 0) {
                ""
              } else {
                paste(supp_obs())
              }
            } else {
              if (max(dat$flag) == 0) {
                paste(supp_whiting(), conf_mess())
              }  else {
                paste(supp_obs(), supp_obs_whiting(), conf_mess())
              }
            }
          }
        } else if (input$Ind_sel == "Other") {
          if (input$socSelect == "Share of landings by state") {
            if (max(dat$conf) == 0) {
              if (max(dat$flag) == 0) {
                if (input$CategorySelect == "State" |
                    input$CategorySelect == "Homeport") {
                  paste(
                    "For the",
                    input$socSelect,
                    "metric, we show all activities for vessels that homeported in the selected",
                    input$CategorySelect,
                    ", \nnot just their activity in the selected",
                    input$CategorySelect,
                    ".\nFor example, the plots above show the",
                    input$socSelect,
                    "for vessels that homeported in",
                    input$VariableSelect,
                    "."
                  )
                } else {
                  ""
                }
              } else {
                if (input$CategorySelect == "State" |
                    input$CategorySelect == "Homeport") {
                  paste(
                    "For the",
                    input$socSelect,
                    "metric, we show all activities for vessels that homeported in the selected",
                    input$CategorySelect,
                    ", \nnot just their activity in the selected",
                    input$CategorySelect,
                    ".\nFor example, the plots above show the",
                    input$socSelect,
                    "for vessels that homeported in",
                    input$VariableSelect,
                    ".",
                    supp_obs()
                  )
                } else {
                  paste(supp_obs())
                }
              }
            } else {
              if (max(dat$flag) == 0) {
                if (input$CategorySelect == "State" |
                    input$CategorySelect == "Homeport") {
                  paste(
                    "For the",
                    input$socSelect,
                    "metric, we show all activities for vessels that homeported in the selected",
                    input$CategorySelect,
                    ", \nnot just their activity in the selected",
                    input$CategorySelect,
                    ".\nFor example, the plots above show the",
                    input$socSelect,
                    "for vessels that homeported in",
                    input$VariableSelect,
                    ".",
                    supp_whiting(),
                    conf_mess()
                  )
                } else {
                  paste(supp_whiting(), conf_mess())
                }
              }  else {
                if (input$CategorySelect == "State" |
                    input$CategorySelect == "Homeport") {
                  paste(
                    "For the",
                    input$socSelect,
                    "metric, we show all activities for vessels that homeported in the selected",
                    input$CategorySelect,
                    ", \nnot just their activity in the selected",
                    input$CategorySelect,
                    ".\nFor example, the plots above show the",
                    input$socSelect,
                    "for vessels that homeported in",
                    input$VariableSelect,
                    ".",
                    supp_obs(),
                    supp_obs_whiting(),
                    conf_mess()
                  )
                } else {
                  paste(supp_obs(),
                    supp_obs_whiting(),
                    conf_mess())
                }
              }
            }
          } else {
            if (max(dat$conf) == 0) {
              if (max(dat$flag) == 0) {
                ""
              } else {
                paste(supp_obs())
              }
            } else {
              if (max(dat$flag) == 0) {
                paste(supp_whiting(), conf_mess())
              }  else {
                paste(supp_obs(), supp_obs_whiting(), conf_mess())
              }
            }
          }
        }
      } else {
        if (max(dat$conf) == 0) {
          if (max(dat$flag) == 0) {
            if (max(dat$metric_flag) == 1) {
              paste(supp_metric())
            } else {
              ""
            }
          } else if (max(dat$flag) == 1) {
            if (max(dat$metric_flag) == 1) {
              paste(supp_metric(), supp_obs())
            } else {
              paste(supp_obs())
            }
          }
        } else if (max(dat$conf) == 1) {
          if (max(dat$flag == 0)) {
            if (max(dat$metric_flag) == 1) {
              paste(supp_metric(),
                supp_obs(),
                supp_obs_whiting(),
                conf_mess())
            } else {
              paste(supp_whiting(), conf_mess())
            }
          } else if (max(dat$flag) == 1) {
            if (max(dat$metric_flag) == 1) {
              paste(supp_metric(),
                supp_obs(),
                supp_obs_whiting(),
                conf_mess())
            } else {
              paste(supp_obs(), supp_obs_whiting(), conf_mess())
            }
          }
        }
      }
    }#end x label function
    
    # Scaling factor for text size ####
    scale_text <- function() {
      if (input$Ind_sel != "Economic") {
        #     if (min(dat$YEAR)<2009) {
        #      return(1.2)
        #     }  else {
        return(1.1)
        #  }
      } else {
        #   b <- table(table(dat$SHORTDESCR)>1)[[1]]
        #   if(b == 1 | b ==3) {
        return(1.2)
        #   } else {
        #     return(1.2)
        #  }
      }
    }
    
    # scaling factor for geom text size ####
    scale_geom_text <- function() {
      if (any(dat$VALUE > 0, na.rm = T)) {
        return(max(dat$VALUE, na.rm = T))
      } else {
        return(0)
      }
    }

    # format data for graph and add to graph ####
    # special data for seasonality plot
  #  print(paste0(seasonality, 1))
    if(input$Ind_sel == 'Other' & input$LayoutSelect != 'Metrics') {
        # and seasonality is selected
        if(input$socSelect =="Seasonality") {
          ssn <- mutate(dat, 
            VALUE = as.Date(VALUE, origin = "2014-01-01", format = "%Y-%m-%d"),
            sort2 = reorder(VARIABLE, sort))
          g <- ggplot(ssn, aes_string(x = x, y = y , group = groupVar), environment =
              environment()) 
          # otherwise normal plot:
        } else {
          dat <- dat[order(dat$sort), ]
          g <-
            # I think this is where the NAs are getting removed which causes lines to be connected through suppressed/missing values #removeNAs
            ggplot(dat, aes_string(x = x, y = y , group = groupVar), environment =
                     environment()) #+coord_cartesian(xlim = c(0, length(table(dat$YEAR))+1))
        }
      } else {
      dat <- dat[order(dat$sort), ]
      g <-
      # I think this is where the NAs are getting removed which causes lines to be connected through suppressed/missing values #removeNAs
        ggplot(dat, aes_string(x = x, y = y , group = groupVar), environment =
            environment()) #+coord_cartesian(xlim = c(0, length(table(dat$YEAR))+1))
    }
    
    # add lines and points to the plot ####
    g <- g + geom_line(aes_string(colour = groupVar), size = 1.5) +
      geom_point(aes_string(colour = groupVar), size = 4)

    
    # add 'data variability' band ####
    if (input$PlotSelect == T & is.na(max(dat$VARIANCE)) == F & !exists('ssn')) {
      g <-
        g + geom_ribbon(aes(
          ymax = upper,
          ymin = lower,
          fill = whitingv
        ), alpha = .25)#show.legend = FALSE,
    } else {
      g <- g
    }

   # if(length(unique(ssn$VARIABLE)) > 1 ) browser()
    #----- define facet -----#####
    if (input$LayoutSelect != 'Metrics') {

      g <- g + facet_wrap( ~ sort2, ncol = 2)

    } else {
      g <- g + facet_wrap( ~ sort2, scales = 'free_y', ncol = 2)
    }
    
    #----- Define grey shading and Non-CS/CS labels ------####
    # choose label text size ####
    labeltext <- ifelse(input$tabs == 'Panel1', 7, 5)

    # geom_rect (define the grey boxes for pre-catch shares) ####
    geom_rect_fun <- function(ymin_val = -Inf, ymax_val = Inf) {
      geom_rect(
            aes(
              xmin = -Inf,
              xmax = table(yr() <= 2010)[[2]] + .5,
              ymin = ymin_val,
              ymax = ymax_val
            ),
            alpha = .05,
            fill = "grey50"
          )
    }
    
    geom_rect4seasonality <- geom_rect(
      aes(
              xmin = -Inf,
              xmax = table(yr() <= 2010)[[2]] + .5,
              ymin = structure(-Inf, class = "Date"),
              ymax = structure(Inf, class = "Date")
            ),
            alpha = .05,
            fill = "grey50"
    )
    
    # geom_text function ####
    
    geom_text_fun <- function(x_val, y_val, label_val, vjust_val = .5) {
      
      geom_text(
        aes(
              x = x_val,
              y = y_val,
              vjust = vjust_val,
              label = label_val,
              family = "serif",
              fontface = "italic"
            ),
            hjust = 0,
            color = "grey20",
            size = labeltext / scale_text()
          )
      
    }

    # set rect and text for plots with both CS and non-CS years ####
    # otherwise no rect or text
    # the original code for the geom_text* are commented out at the bottom of the doc
    # if there are years shown before and after implementation of catch shares
    if (length(yr()) > 1 & min(yr()) < 2011 & max(yr()) > 2010) {
      # if the "Group by vessels" display is chosen
      if (input$LayoutSelect != 'Metrics') {
        # if seasonality is clicked
        if(input$Ind_sel == 'Other') {
        # and seasonality is selected
        if(input$socSelect =="Seasonality") {
        g <- g + geom_rect_fun(
          ymin_val = structure(-Inf, class = "Date"),
          ymax_val = structure(Inf, class = "Date"))
        g <- g + geom_text_fun(
             x_val = table(yr() <= 2010)[[2]] / 3.5,
             y_val = min(as.Date(upper(), origin = "2014-01-01")),
             label_val = "Pre-catch shares")
        g <- g + geom_text_fun(
             x_val = table(yr() <= 2010)[[2]] + table(yr() > 2010)[[2]] / 1.5,
             y_val = min(as.Date(upper(), origin = "2014-01-01")),
             label_val = "Catch shares")
        # for all other variables
      }} else {
        g <- g + geom_rect_fun()
          # geom_text1
        g <- g + geom_text_fun(
            x_val = table(yr() <= 2010)[[2]] / 3.5,
            y_val = max(upper()) + scale_geom_text()/5,
            label_val = "Pre-catch shares")
        g <- g +
            # geom_text3
            geom_text_fun(
              x_val = table(yr() <= 2010)[[2]] + table(yr() > 2010)[[2]] / 1.5,
              y_val = max(upper()) + scale_geom_text() / 5,
              label_val = "Catch shares")
      } 
        } else { # Compare by metrics
        g <- g + geom_rect_fun()
        g <- g +
          # geom_text4
          geom_text_fun(
              x_val = table(yr() <= 2010)[[2]] / 3.5,
              y_val = Inf,
              label_val = "Pre-catch shares",
          vjust_val = 1.5)
          g <- g +
            # geom_text6
            geom_text_fun(
              x_val = table(yr() <= 2010)[[2]] + table(yr() > 2010)[[2]] / 1.5,
              y_val = Inf,
              label_val = "Catch shares",
          vjust_val = 1.5)

        } } else {
    # end of rect/text for cs/non-cs, no CS box required for plots with only one "kind" of year
      g <- g
    }
  
    # set colors for the three lines (whiting vessels, non-whiting vessels, all vessels) ####
    g <-
      g + scale_fill_manual(values = colourThirds) + scale_colour_manual(values = colourThirds) #+ scale_x_discrete('YEAR2', drop=FALSE)
    
    # add y axis line ####
    g <- g + geom_hline(yintercept = 0)
    
    #---- define labels (not sure what this does------####
    if (input$tabs == 'Panel1') {
      g <- g + labs(y = ylab(),
        x = xlab(),
        title = main())
    } else {
      g <- g + labs(y = ylab(),
        x = '',
        title = main())
    }
    
    if (input$tabs == 'Panel1') {
      strptextsize <- 18
    } else {
      strptextsize <- 14
    }

    # Define and add THEME to g####
    g <- g + theme(
      plot.title = element_text(
        vjust = 1,
        hjust = .5,
        size = rel(1.5),
        colour = "grey25",
        family = "sans",
        face = "bold"
      ),
      #
      # plot.title = element_text(, vjust = 1),
      panel.background = element_rect(fill = "white"),
      #      panel.spacing = unit(c(0.5, 0.5, 1, 0.5), "cm"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(
        family = "sans",
        size = strptextsize,
        color = "grey25",
        vjust = 1
      ),
      strip.background = element_rect(fill = "lightgrey"),
      axis.ticks = element_blank(),
      axis.title.x = element_text(
        size = rel(1.1),
        hjust = 0,
        face = "italic",
        vjust = -1,
        colour = "grey25"
      ),
      axis.title.y = element_text(
        size = rel(1.2),
        vjust = 2,
        colour = "grey25"
      ),
      axis.line.x = element_line(
        size = 2,
        colour = "black",
        linetype = "solid"
      ),
      axis.text = element_text(size = 12),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(
        family = "sans",
        color = "grey25",
        face = "bold",
        size = 12
      ),
      legend.title = element_blank()
      #  text = element_text(family="sans", color = "red", size=rel(1.3))
    )

    ############################################################################################################
    
    #################################################################################################################################
    #ggplotly(g)
    print(g)
    
    
    } else
      plot(
        0,
        0,
        type = "n",
        axes = F,
        xlab = "",
        ylab = ""
      )
  #  print(head(dat))
}


# geom_text1 <- geom_text(
    #         aes(
    #           x = table(yr() <= 2010)[[2]] / 3.5,
    #           y = max(upper()) + scale_geom_text() / 5,
    #           label = "Pre-catch shares",
    #           family = "serif",
    #           fontface = "italic"
    #         ),
    #         hjust = 0,
    #         color = "grey20",
    #         size = labeltext / scale_text()
    #       )
    # 
    #     geom_text3 <- geom_text(
    #           aes(
    #             x = table(yr() <= 2010)[[2]] + table(yr() > 2010)[[2]] / 1.5,
    #             y = max(upper()) + scale_geom_text() / 5,
    #             label = "Catch shares",
    #             family = "serif",
    #             fontface = "italic"
    #           ),
    #           hjust = 0,
    #           color = "grey20",
    #           size = labeltext / scale_text()
    #         )
    # 
    #     geom_text4 <- geom_text(
    #         aes(
    #           x = table(yr() <= 2010)[[2]] / 3.5,
    #           y = Inf,
    #           vjust = 1.5,
    #           label = "Pre-catch shares",
    #           family = "serif",
    #           fontface = "italic"
    #         ),
    #         hjust = 0,
    #         color = "grey20",
    #         size = labeltext / scale_text()
    #       )
    # 
    # 
    #         
    # geom_text6 <-  geom_text(
    #           aes(
    #             x = table(yr() <= 2010)[[2]] + table(yr() > 2010)[[2]] / 1.5,
    #             y = Inf,
    #             vjust = 1.5,
    #             label = "Catch shares"
    #           ),
    #           hjust = 0,
    #           family = "serif",
    #           fontface = "italic",
    #           color = "grey20",
    #           size = labeltext / scale_text()
    #         )    
    # 
    # 
    # 
    # geom_text2a <- geom_text(
    #           aes(
    #             x = table(yr() <= 2010)[[2]] + table(yr() > 2010)[[2]] / 1.5,
    #             y = max(upper()) + scale_geom_text() / 5,
    #             label = "Catch"
    #           ),
    #           hjust = 0,
    #           family = "serif",
    #           fontface = "italic",
    #           color = "grey20",
    #           size = labeltext / scale_text()
    #          )
    # geom_text2b <- geom_text(
    #           aes(
    #             x = table(yr() <= 2010)[[2]] + table(yr() > 2010)[[2]] / 1.5,
    #             y = max(upper()) - max(upper()) / 100,
    #             label = "shares"
    #           ),
    #           hjust = 0,
    #           family = "serif",
    #           fontface = "italic",
    #           color = "grey20",
    #           size = labeltext / scale_text()
    #         )
    # 
    # 
    # 
    # 
    # geom_text5a <-  geom_text(
    #           aes(
    #             x = table(yr() <= 2010)[[2]] + table(yr() > 2010)[[2]] / 1.5,
    #             y = Inf,
    #             vjust = 1.5,
    #             label = "Catch"
    #           ),
    #           hjust = 0,
    #           family = "serif",
    #           fontface = "italic",
    #           color = "grey20",
    #           size = labeltext / scale_text()
    #        )
    #  
    # 
    # geom_text5b <- geom_text(
    #           aes(
    #             x = table(yr() <= 2010)[[2]] + table(yr() > 2010)[[2]] / 1.5,
    #             y = Inf,
    #             vjust = 1.5,
    #             label = "shares"
    #           ),
    #           hjust = 0,
    #           family = "serif",
    #           fontface = "italic",
    #           color = "grey20",
    #           size = labeltext / scale_text()
    #         )
