

library(readr)
flow_data <- read_csv("Reading-flow-15min-Qualified.csv")


# Checking observations with negative values (can set as NA as threshold u > 0)
any(flow_data$value < 0, na.rm = False)
sum(flow_data$value < 0, na.rm = TRUE)
subset(flow_data, value)

max(which(is.na(flow_data$value)))

dates <- as.POSIXct(flow_data$dateTime)
missing <- is.na(flow_data$value)


plot(dates, flow_data$value, type = "p", pch = 5, cex = 0.25,
     ylim = range(c(-50, flow_data$value), na.rm = TRUE),
     xlab = "Date", ylab = "Flow")


points(dates[missing], rep(-50, sum(missing)),
       col = "red", pch = 5, cex = 0.25)


flow_data_new <- subset(
  flow_data,
  as.POSIXct(dateTime) >= as.POSIXct("2005-01-01")
  
)


dates <- as.POSIXct(flow_data_new$dateTime)
missing <- is.na(flow_data_new$value)

plot(dates, flow_data_new$value,
     pch = 16, cex = 0.3,
     ylim = range(c(-50, flow_data_new$value), na.rm = TRUE),
     xlab = "Date", ylab = "Flow")

points(dates[missing], rep(-50, sum(missing)),
       col = "red", pch = 16, cex = 0.3)

View(Reading_flow_15min_Qualified)


dates <- as.POSIXct(flow_data$dateTime)
missing <- is.na(flow_data$value)

plot(dates, flow_data$value,
     pch = 16, cex = 0.3, xaxt = "n",
     ylim = range(c(-50, flow_data$value), na.rm = TRUE),
     xlab = "Date", ylab = "Flow")

ticks <- seq(as.POSIXct("1990-01-01"),
             max(dates, na.rm = TRUE), by = "5 years")

axis(1, at = ticks, labels = format(ticks, "%Y"))

points(dates[missing], rep(-50, sum(missing)),
       col = "red", pch = 16, cex = 0.3)