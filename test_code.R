#########################################################################################################
# Actual Coding NOW###


########### Exploratory Analysis ##################

library(readr)
library(ggplot2)
library(zoo)

# Load the data
flow_data <- read_csv("Reading-flow-15min-Qualified.csv")

# Convert dates and keep data from 2005
flow_data$dateTime <- as.POSIXct(flow_data$dateTime)

flow_data_2005 <- subset(
  flow_data,
  dateTime >= as.POSIXct("2005-01-01")
)

flow_data_2005 <- flow_data_2005[
  order(flow_data_2005$dateTime),
]


# Basic checks
head(flow_data_2005)
str(flow_data_2005)

range(flow_data_2005$dateTime)
summary(flow_data_2005$value)

# Number of observations
nrow(flow_data_2005)

# Number and percentage missing
sum(is.na(flow_data_2005$value))
mean(is.na(flow_data_2005$value)) * 100

######## Checking missing values NAsof full dataset####
# Number and percentage missing
sum(is.na(flow_data$value))
mean(is.na(flow_data$value)) * 100

# Rows before 2005
before_2005 <- as.POSIXct(flow_data$dateTime) <
  as.POSIXct("2005-01-01")

# Number of missing readings before 2005
missing_before_2005 <- sum(
  before_2005 & is.na(flow_data$value),
  na.rm = TRUE
)

missing_before_2005

# As a percentage of ALL observations in the full dataset
missing_before_2005 / nrow(flow_data) * 100

# As a percentage of ALL missing readings in the full dataset
missing_before_2005 / sum(is.na(flow_data$value)) * 100


#####################



# Negative observations: inspect before deciding what to do
sum(flow_data_2005$value < 0, na.rm = TRUE)
subset(flow_data_2005, value < 0)

# Setting negative observations as NA as we are only concerned with peak events 
flow_data_2005$value[which(flow_data_2005$value < 0)] <- NA

# Quality categories
table(flow_data_2005$quality, useNA = "ifany")


##############################################################

# Plot flow, coloured by quality
ggplot(flow_data_2005,
       aes(dateTime, value, colour = quality)) +
  geom_point(size = 0.4, na.rm = TRUE) +
  scale_colour_manual(values = c(
    "Good" = "black",
    "Estimated" = "blue",
    "Suspect" = "darkorange",
    "Unchecked" = "purple",
    "Missing" = "red"
  )) +
  scale_x_datetime(
    date_breaks = "5 years",
    date_labels = "%Y"
  ) +
  labs(x = "Date", y = "Flow", colour = "Quality") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(flow_data_2005,
       aes(dateTime, value, colour = quality)) +
  geom_point(size = 0.4, na.rm = TRUE) +
  geom_rug(
    data = subset(flow_data_2005, is.na(value)),
    aes(x = dateTime),
    inherit.aes = FALSE,
    sides = "b",
    colour = "red",
    alpha = 0.4
  ) +
  scale_colour_manual(values = c(
    "Good" = "black",
    "Estimated" = "blue",
    "Suspect" = "darkorange",
    "Unchecked" = "purple",
    "Missing" = "red"
  )) +
  scale_x_datetime(
    date_breaks = "5 years",
    date_labels = "%Y"
  ) +
  labs(
    x = "Date", y = "Flow", colour = "Quality",
    caption = "Red marks along the bottom indicate missing readings."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
#########################################################
suspect <- which(flow_data_2005$quality == "Suspect")

# Number of suspect readings
length(suspect)

# Number above our initial threshold
sum(flow_data_2005$value[suspect] > 60, na.rm = TRUE)

# Inspect them
View(flow_data_2005[suspect, ])

# Keep a version with suspect readings retained
flow_with_suspect <- flow_data_2005

# Mark suspect readings as missing in the main dataset
suspect <- which(flow_data_2005$quality == "Suspect")
flow_data_2005$value[suspect] <- NA

# Recalculate interpolation after this change
flow_data_2005$flow_filled <- zoo::na.approx(
  flow_data_2005$value,
  x = as.numeric(flow_data_2005$dateTime),
  maxgap = 2,
  na.rm = FALSE
)

flow_data_2005$interpolated <-
  is.na(flow_data_2005$value) &
  !is.na(flow_data_2005$flow_filled)


# Checking number of missing values again

# Number and percentage missing
sum(is.na(flow_data_2005$value))
mean(is.na(flow_data_2005$value)) * 100

############################################

# Percentage missing in each year
year <- format(flow_data_2005$dateTime, "%Y")

missing_by_year <- aggregate(
  list(percent_missing = is.na(flow_data_2005$value) * 100),
  by = list(year = year),
  FUN = mean
)

print(missing_by_year)

ggplot(missing_by_year, aes(year, percent_missing)) +
  geom_col(fill = "steelblue") +
  labs(x = "Year", y = "Missing observations (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Lengths of consecutive missing gaps

stopifnot(all(
  as.numeric(diff(flow_data_2005$dateTime), units = "mins") == 15
))
runs <- rle(is.na(flow_data_2005$value))

gap_lengths <- runs$lengths[runs$values]
gap_minutes <- gap_lengths * 15

if (length(gap_minutes) > 0) {
  
  # Longest gap
  print(max(gap_lengths))          # Number of missing readings
  print(max(gap_minutes) / 60)     # Hours
  print(max(gap_minutes) / 1440)   # Days
  
  gap_category <- cut(
    gap_minutes,
    breaks = c(
      0, 15, 30, 60, 180, 360,
      720, 1440, 4320, 10080, Inf
    ),
    labels = c(
      "15 min", "15–30 min", "30–60 min",
      "1–3 h", "3–6 h", "6–12 h",
      "12–24 h", "1–3 days", "3–7 days", ">7 days"
    ),
    right = TRUE
  )
  
  gap_counts <- as.data.frame(table(gap_category))
  names(gap_counts) <- c("duration", "number_of_gaps")
  
  print(gap_counts)
  
  ggplot(gap_counts, aes(duration, number_of_gaps)) +
    geom_col(fill = "lightblue", colour = "black") +
    labs(x = "Gap duration", y = "Number of gaps") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

############ Checking typical changes ######################
flow <- flow_data_2005$value
u <- 60

starts <- seq_len(length(flow) - 4)

complete <- rep(TRUE, length(starts))
high_flow <- rep(FALSE, length(starts))

for (k in 0:4) {
  complete <- complete & !is.na(flow[starts + k])
  high_flow <- high_flow | flow[starts + k] > u
}

# Complete one-hour windows
all_starts <- starts[complete]

# Complete windows containing flow above 60
high_starts <- starts[complete & !is.na(high_flow) & high_flow]

change_all <- abs(flow[all_starts + 4] - flow[all_starts])
change_high <- abs(flow[high_starts + 4] - flow[high_starts])

# Typical changes across all flow levels
summary(change_all)

# Typical changes during high flow
summary(change_high)

# 95% of high-flow changes are below this value
quantile(change_high, 0.95)

#################################################

#################### Interpolation ############################

# Fill gaps of up to four missing readings (one hour)
flow_data_2005$flow_filled <- na.approx(
  flow_data_2005$value,
  x = as.numeric(flow_data_2005$dateTime),
  maxgap = 4,
  na.rm = FALSE
)

# Mark which readings were filled
flow_data_2005$interpolated <-
  is.na(flow_data_2005$value) &
  !is.na(flow_data_2005$flow_filled)

# Number filled and number still missing
sum(flow_data_2005$interpolated)
sum(is.na(flow_data_2005$flow_filled))


# Plot the flow with interpolated readings in orange
ggplot(flow_data_2005, aes(dateTime, flow_filled)) +
  geom_point(size = 0.3, na.rm = TRUE) +
  geom_point(
    data = subset(flow_data_2005, interpolated),
    colour = "orange",
    size = 0.6
  ) +
  scale_x_datetime(
    date_breaks = "5 years",
    date_labels = "%Y"
  ) +
  labs(
    x = "Date",
    y = "Flow",
    caption = "Orange points show interpolated readings."
  ) +
  theme_minimal()



#################### Peaks over threshold ####################

dat <- flow_data_2005

find_events <- function(dat, u, separation_days = 3) {
  
  flow <- dat$flow_filled
  dates <- dat$dateTime
  
  # Find runs above u
  above <- !is.na(flow) & flow > u
  runs <- rle(above)
  
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  
  starts <- starts[runs$values]
  ends <- ends[runs$values]
  
  if (length(starts) == 0) return(data.frame())
  
  # Flag runs touching missing data or the record boundary
  padded <- c(NA, flow, NA)
  
  uncertain <- is.na(padded[starts]) |
    is.na(padded[ends + 2])
  
  group <- integer(length(starts))
  group[1] <- 1
  
  # Decide whether neighbouring runs belong to the same flood
  if (length(starts) > 1) {
    for (i in 2:length(starts)) {
      
      between <- seq.int(ends[i - 1] + 1, starts[i] - 1)
      
      if (anyNA(flow[between])) {
        
        # Don't join across an unresolved gap
        uncertain[c(i - 1, i)] <- TRUE
        separate <- TRUE
        
      } else {
        
        days_below <- length(between) / 96
        
        separate <- days_below >= separation_days &&
          min(flow[between]) <= 0.75 * u
      }
      
      group[i] <- group[i - 1] + as.integer(separate)
    }
  }
  
  # Keep one maximum for each event
  events <- lapply(split(seq_along(starts), group), function(g) {
    
    first <- starts[min(g)]
    last <- ends[max(g)]
    rows <- first:last
    peak <- rows[which.max(flow[rows])]
    
    data.frame(
      first_above = dates[first],
      last_above = dates[last],
      peak_time = dates[peak],
      max_flow = flow[peak],
      n_interpolated = sum(dat$interpolated[rows]),
      uncertain = any(uncertain[g])
    )
  })
  
  events <- do.call(rbind, events)
  rownames(events) <- NULL
  events
}


# Observation time, excluding NAs left after interpolation
observed_years <- sum(!is.na(dat$flow_filled)) /
  (96 * 365.25)

# Try thresholds from 40 to 200
thresholds <- seq(40, 200, by = 5)

pot_results <- do.call(rbind, lapply(thresholds, function(u) {
  
  events <- find_events(dat, u)
  
  if (nrow(events) == 0) {
    retained <- 0
    flagged <- 0
  } else {
    retained <- sum(!events$uncertain)
    flagged <- sum(events$uncertain)
  }
  
  data.frame(
    threshold = u,
    retained_events = retained,
    flagged_events = flagged,
    events_per_year = retained / observed_years
  )
}))

print(pot_results)


# Plot event rate against threshold
ggplot(pot_results, aes(threshold, events_per_year)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = c(3, 5),
             colour = "red", linetype = "dashed") +
  labs(x = "Flow threshold",
       y = "Retained events per observed year") +
  theme_minimal()


# Thresholds giving 3–5 events per year
suitable <- subset(
  pot_results,
  events_per_year >= 3 & events_per_year <= 5
)

print(suitable)


# For now, try the threshold giving a rate closest to four
if (nrow(suitable) > 0) {
  
  chosen_u <- suitable$threshold[
    which.min(abs(suitable$events_per_year - 4))
  ]
  
  events <- find_events(dat, chosen_u)
  retained_events <- subset(events, !uncertain)
  
  print(chosen_u)
  View(retained_events)
  
} else {
  message("No suitable threshold found. Try a wider threshold range.")
}