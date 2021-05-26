## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  fig.height=5, fig.width=8, 
  message=FALSE, warning=FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(USgas)

## -----------------------------------------------------------------------------
data("us_total")

head(us_total)

## -----------------------------------------------------------------------------
unique(us_total$state)

## -----------------------------------------------------------------------------
us_agg <- us_total[which(us_total$state == "U.S."),]

us_agg <- us_agg[order(us_agg$year), ]

head(us_agg)

## -----------------------------------------------------------------------------
at_x <- seq(from = 1950,
            to = 2020,
            by = 10)

at_y <- pretty(us_agg$y)[c(3, 5, 7)]

plot(us_agg$year, us_agg$y,
     col = "#1f77b4",
     type = "l",
     frame.plot = FALSE,
     axes = FALSE,
     panel.first = abline(h = at_y, col = "grey80"),
     main = "US Total Natural Gas Consumption",
     xlab = "Source: https://www.eia.gov/",
     ylab = "Million Cubic Feet")

mtext(side =1, text = format(at_x, nsmall=0), at = at_x,
      col = "grey20", line = 1, cex = 0.8)

mtext(side =2, text = format(at_y, scientific = FALSE), at = at_y,
      col = "grey20", line = 1, cex = 0.8)

## -----------------------------------------------------------------------------
ne <- c("Connecticut", "Maine", "Massachusetts",
        "New Hampshire", "Rhode Island", "Vermont")
ne_gas <-  us_total[which(us_total$state %in% ne),]

ne_wide <- reshape(ne_gas, v.names = "y", idvar = "year",
                   timevar = "state", direction = "wide")
ne_wide <- ne_wide[order(ne_wide$year), ]

names(ne_wide) <- c("year",ne)

## -----------------------------------------------------------------------------
at_x <- seq(from = 2000, to = 2020, by = 5)

at_y <- pretty(ne_gas$y)[c(2, 4, 6)]

## -----------------------------------------------------------------------------
# plot the first series
plot(ne_wide$year, ne_wide$Connecticut,
     type = "l",
     col = "#073b4c",
     frame.plot = FALSE,
     axes = FALSE,
     panel.first = abline(h = c(at_y), col = "grey80"),
     main = "New England Annual Natural Gas Consumption by State",
     cex.main = 1.2, font.main = 1, col.main = "black",
     xlab = "Source: https://www.eia.gov/",
     font.axis = 1, cex.lab= 1,
     ylab = "Million Cubic Feet",
     ylim = c(min(ne_gas$y, na.rm = TRUE), max(ne_gas$y, na.rm = TRUE)),
     xlim = c(min(ne_gas$year), max(ne_gas$year) + 3))

# Add the 5 other series
lines(ne_wide$year, ne_wide$Maine, col = "#1f77b4")
lines(ne_wide$year, ne_wide$Massachusetts, col = "#118ab2")
lines(ne_wide$year, ne_wide$`New Hampshire`, col = "#06d6a0")
lines(ne_wide$year, ne_wide$`Rhode Island`, col = "#ffd166")
lines(ne_wide$year, ne_wide$Vermont, col = "#ef476f")

# Add the y and x axis ticks

mtext(side =1, text = format(at_x, nsmall=0), at = at_x,
      col = "grey20", line = 1, cex = 0.8)

mtext(side =2, text = format(at_y, scientific = FALSE), at = at_y,
      col = "grey20", line = 1, cex = 0.8)

# Add text 
text(max(ne_wide$year) + 2,
     tail(ne_wide$Connecticut,1),
     "Connecticut",
     col = "#073b4c",
     cex = 0.7)

text(max(ne_wide$year) + 2,
     tail(ne_wide$Maine,1) * 0.95,
     "Maine",
     col = "#1f77b4",
     cex = 0.7)

text(max(ne_wide$year) + 2,
     tail(ne_wide$Massachusetts,1),
     "Massachusetts",
     col = "#118ab2",
     cex = 0.7)

text(max(ne_wide$year) + 2,
     tail(ne_wide$`New Hampshire`,1) * 1.1,
     "New Hampshire",
     col = "#06d6a0",
     cex = 0.7)

text(max(ne_wide$year) + 2,
     tail(ne_wide$`Rhode Island`,1) * 1.05,
     "Rhode Island",
     col = "#ffd166",
     cex = 0.7)

text(max(ne_wide$year) + 2,
     tail(ne_wide$Vermont,1),
     "Vermont",
     col = "#ef476f",
     cex = 0.7)

## -----------------------------------------------------------------------------
data("us_monthly")

head(us_monthly)

## -----------------------------------------------------------------------------
at_x <- seq.Date(from = as.Date("2000-01-01"),
                 to = as.Date("2020-01-01"),
                 by = "5 years")

at_y <- pretty(us_monthly$y)[c(2, 4, 6)]

plot(us_monthly$date, us_monthly$y,
     col = "#1f77b4",
     type = "l",
     frame.plot = FALSE,
     axes = FALSE,
     panel.first = abline(h = at_y, col = "grey80"),
     main = "US Total Natural Gas Consumption",
     xlab = "Source: https://www.eia.gov/",
     ylab = "Million Cubic Feet")



mtext(side =1, text = format(at_x, format = "%Y"), at = at_x,
      col = "grey20", line = 1, cex = 0.8)

mtext(side =2, text = format(at_y, scientific = FALSE), at = at_y,
      col = "grey20", line = 1, cex = 0.8)

## -----------------------------------------------------------------------------
data("us_residential")

head(us_residential)

## -----------------------------------------------------------------------------
unique(us_residential$state)

## -----------------------------------------------------------------------------
wc_gas <- us_residential[which(us_residential$state %in%
                             c("Alaska", "California",
                               "Oregon", "Washington")),]

# Reshape to wide
wc_wide <- reshape(wc_gas, v.names = "y", idvar = "date",
                   timevar = "state", direction = "wide")

names(wc_wide) <- c("date","Alaska", "California",
                    "Oregon", "Washington")
# Reorder the data
wc_wide <- wc_wide[order(wc_wide$date), ]

# Set the plot y and x axis ticks
at_x <- seq.Date(from = as.Date("1990-01-01"),
                 to = as.Date("2020-01-01"),
                 length.out = 4)

at_y <- pretty(wc_gas$y)[c(2, 4, 6)]

# plot the first series
plot(wc_wide$date, wc_wide$Alaska,
     type = "l",
     frame.plot = FALSE,
     axes = FALSE,
     panel.first = abline(h = c(at_y), col = "grey80"),
     main = "West Cost Natural Gas Residential Consumption",
     cex.main = 1.2, font.main = 1, col.main = "black",
     xlab = "Source: https://www.eia.gov/",
     font.axis = 1, cex.lab= 1,
     ylab = "Million Cubic Feet",
     ylim = c(min(wc_gas$y, na.rm = TRUE), max(wc_gas$y, na.rm = TRUE)))

# Add the 3 other series
lines(wc_wide$date, wc_wide$California, col = "#1f77b4")
lines(wc_wide$date, wc_wide$Oregon, col = "#457b9d")
lines(wc_wide$date, wc_wide$Washington, col = "#ef476f")

# Add the y and x axis ticks
mtext(side =1, text = format(at_x, format = "%Y %b"), at = at_x,
      col = "grey20", line = 1, cex = 0.8)

mtext(side =2, text = format(at_y, scientific = FALSE), at = at_y,
      col = "grey20", line = 1, cex = 0.8)

text(tail(wc_wide$date, 10)[1],
     max(wc_gas$y, na.rm = TRUE),
     "California",
     col = "#1f77b4",
     cex = 0.6)

text(tail(wc_wide$date, 10)[1],
     max(wc_gas$y, na.rm = TRUE) - 5000,
     "Washington",
     col = "#ef476f",
     cex = 0.6)

text(tail(wc_wide$date, 10)[1],
     max(wc_gas$y, na.rm = TRUE) - 10000,
     "Oregon",
     col = "#457b9d",
     cex = 0.6)

text(tail(wc_wide$date, 10)[1],
     max(wc_gas$y, na.rm = TRUE) - 15000,
     "Alaska",
     col = "black",
     cex = 0.6)

