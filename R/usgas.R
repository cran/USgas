#' US Monthly Consumption of Natural Gas by End Use
#' @description The US monthly consumption of natural gas by end-use and state between 1973 (US aggregate, state level since 1989) and 2022.
#'
#' Units: Million Cubic Feet
#'
#'
#' @format A data.frame with 6 variables.
#' \describe{
#'   \item{date}{A Date, the month and year of the observation (the day set by default to 1st of the month)}
#'   \item{process}{The process type description}
#'   \item{state}{The US state name}
#'   \item{state_abb}{the US state abbreviation}
#'   \item{y}{A numeric, the monthly natural gas residential consumption in a million cubic feet}
#'   }
#' @source US Energy Information Administration (EIA) \href{https://www.eia.gov/}{website}.
#' @keywords datasets timeseries natural gas us state
#' @details The dataset contains monthly summary of the consumption of natural gas
#' by end-use in the US by state and total aggregate level. The data is available
#' for the state level between January 1989 and December 2022, and for the US level
#' between January 1973 and Dec 2022. It includes the following end-use categories:
#'
#' - Commercial Consumption
#' - Delivered to Consumers
#' - Electric Power Consumption
#' - Industrial Consumption
#' - Lease and Plant Fuel Consumption
#' - Pipeline Fuel Consumption
#' - Residential Consumption
#' - Vehicle Fuel Consumption
#'
#' @examples
#'
#' library(plotly)
#'
#' data("usgas")
#'
#' head(usgas)
#'
#' # Plot the US consumption
#'
#' us_df <- usgas[which(usgas$state == "U.S."), ]
#'
#' plot_ly(data = us_df,
#'         x = ~ date,
#'         y = ~ y,
#'         color = ~ process,
#'         type = "scatter",
#'         mode = "line") |>
#'   layout(title = "US Monthly Consumption by End Use",
#'          yaxis = list(title = "MMCF"),
#'          xaxis = list(title = "Source: EIA Website"),
#'          legend = list(x = 0, y = 1.05),
#'          margin = list(l = 50, r = 50, b = 70, t = 60))
#'
#'
#' # Plot the California consumption
#'
#' ca_df <- usgas[which(usgas$state == "California"), ]
#'
#' plot_ly(data = ca_df,
#'         x = ~ date,
#'         y = ~ y,
#'         color = ~ process,
#'         type = "scatter",
#'         mode = "line") |>
#'   layout(title = "California Monthly Consumption by End Use",
#'          yaxis = list(title = "MMCF"),
#'          xaxis = list(title = "Source: EIA Website"),
#'          legend = list(x = 0, y = 1.05),
#'          margin = list(l = 50, r = 50, b = 70, t = 60))
#'
"usgas"
