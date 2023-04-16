## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  fig.height=5, fig.width=8, 
  message=FALSE, warning=FALSE,
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(USgas)

data("usgas")

str(usgas)

head(usgas)

## -----------------------------------------------------------------------------
unique(usgas$state)

## -----------------------------------------------------------------------------
us_agg <- usgas[which(usgas$state == "U.S."),]

head(us_agg)

## -----------------------------------------------------------------------------
library(plotly)

plot_ly(data = us_agg,
        x = ~ date,
        y = ~ y,
        color = ~ process,
        type = "scatter",
        mode = "line") |> 
  layout(title = "US Monthly Consumption of Natural Gas by End Use",
         yaxis = list(title = "MMCF"),
         xaxis = list(title = "Source: EIA Website"),
         legend = list(x = 0,
                       y = 0.95))

## -----------------------------------------------------------------------------
ne <- c("Connecticut", "Maine", "Massachusetts",
        "New Hampshire", "Rhode Island", "Vermont")
ne_gas <-  usgas[which(usgas$state %in% ne),]

head(ne_gas)


## -----------------------------------------------------------------------------
ne_gas[which(ne_gas$process == "Residential Consumption"),] |>
  plot_ly(x = ~ date,
          y = ~ y,
          color = ~ state,
          type = "scatter",
          mode = "line") |> 
  layout(title = "Monthly Residential Consumption of Natural Gas in New England",
         yaxis = list(title = "MMCF"),
         xaxis = list(title = "Source: EIA Website"),
         legend = list(x = 0,
                       y = 1))

