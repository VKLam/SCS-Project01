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



################# POT: OLD EVENT DEFINITION (commented out) ####################
## Replaced by the Robson & Reed method below. This version counted every
## unbroken run above u as an event, so one flood crossing u many times (gauge
## jitter near the threshold) was counted many times, and it discarded any event
## interrupted by an NA (including floods with Suspect readings set to NA).
# ################# POT#######################
# 
# u <- 148
# dat <- flow_data_2005
# 
# # Fill gaps of up to four missing readings
# dat$flow_filled <- zoo::na.approx(
#   dat$value,
#   x = as.numeric(dat$dateTime),
#   maxgap = 4,
#   na.rm = FALSE
# )
# 
# flow <- dat$flow_filled
# dates <- dat$dateTime
# 
# events <- data.frame(
#   start_time = as.POSIXct(character()),
#   end_time = as.POSIXct(character()),
#   peak_time = as.POSIXct(character()),
#   max_flow = numeric()
# )
# 
# in_event <- FALSE
# ready <- FALSE
# discarded <- 0
# 
# for (i in seq_along(flow)) {
#   
#   if (is.na(flow[i])) {
#     
#     # A gap left after interpolation interrupts the event
#     if (in_event) {
#       discarded <- discarded + 1
#     }
#     
#     in_event <- FALSE
#     ready <- FALSE
#     
#   } else if (flow[i] <= u) {
#     
#     # The event finishes when flow returns to u or below
#     if (in_event) {
#       
#       events[nrow(events) + 1, ] <- list(
#         dates[start],
#         dates[i],
#         dates[peak],
#         flow[peak]
#       )
#     }
#     
#     in_event <- FALSE
#     ready <- TRUE
#     
#   } else {
#     
#     # Start an event only after observing flow at/below u
#     if (!in_event && ready) {
#       start <- i
#       peak <- i
#       in_event <- TRUE
#     }
#     
#     if (in_event && flow[i] > flow[peak]) {
#       peak <- i
#     }
#   }
# }
# 
# # An event still open at the end is incomplete
# incomplete_at_end <- in_event
# 
# # Results
# nrow(events)
# discarded
# incomplete_at_end
# 
# View(events)
# 
# 
# # Approximate observed years and event rate
# observed_years <- sum(!is.na(flow)) / (96 * 365.25)
# 
# data.frame(
#   threshold = u,
#   events_counted = nrow(events),
#   events_interrupted_by_gaps = discarded,
#   observed_years = observed_years,
#   events_per_year = nrow(events) / observed_years,
#   minimum_for_3_per_year = ceiling(3 * observed_years),
#   maximum_for_5_per_year = floor(5 * observed_years)
# )
# 
# 
# ###
# 
# 
# flow <- flow_data_2005$flow_filled
# 
# # Observation time, excluding remaining NAs
# observed_years <- sum(!is.na(flow)) / (96 * 365.25)
# 
# 
# 
# 
# get_runs <- function(flow, u) {
#   
#   events <- data.frame(
#     start = integer(),
#     end = integer(),
#     peak = integer()
#   )
#   
#   ready <- FALSE
#   in_event <- FALSE
#   
#   for (i in seq_along(flow)) {
#     
#     if (is.na(flow[i])) {
#       
#       # Discard an interrupted event
#       ready <- FALSE
#       in_event <- FALSE
#       
#     } else if (flow[i] <= u) {
#       
#       # Finish the event
#       if (in_event) {
#         rows <- start:(i - 1)
#         peak <- rows[which.max(flow[rows])]
#         
#         events[nrow(events) + 1, ] <- list(
#           start, i - 1, peak
#         )
#       }
#       
#       ready <- TRUE
#       in_event <- FALSE
#       
#     } else if (ready && !in_event) {
#       
#       # Start a run above u
#       start <- i
#       in_event <- TRUE
#     }
#   }
#   
#   events
# }
# 
# thresholds <- seq(40, 200, by = 1)
# 
# results <- data.frame(
#   threshold = thresholds,
#   number_of_events = integer(length(thresholds))
# )
# 
# for (i in seq_along(thresholds)) {
#   
#   events <- get_runs(flow, u = thresholds[i])
#   
#   results$number_of_events[i] <- nrow(events)
# }
# 
# results$events_per_year <-
#   results$number_of_events / observed_years
# 
# View(results)
# print(results)
# 
# ggplot(results, aes(threshold, events_per_year)) +
#   geom_line() +
#   geom_point() +
#   geom_hline(
#     yintercept = c(3, 5),
#     colour = "red",
#     linetype = "dashed"
#   ) +
#   labs(
#     x = "Flow threshold",
#     y = "Events per observed year"
#   ) +
#   theme_minimal()
# 
# subset(results, events_per_year >= 3 & events_per_year <= 5)

################# POT: ROBSON & REED EVENT DEFINITION ##########################
## Uses extract_events() from pot_functions.R (Robson & Reed 1999):
##   1. Candidate peaks = highest point of each run where the 3 h smoothed flow > u
##   2. Two neighbouring peaks are independent only if they are at least
##      sep_factor x (mean time to rise) apart AND the flow between them drops
##      below trough_frac x the first peak; otherwise the smaller peak is dropped
##   3. Start = last daily-scale trough before the peak; peak flow = highest RAW
##      reading within +/- 3 h of the smoothed peak

source("pot_functions.R")

u           <- 60            # threshold, m3/s
step_min    <- 15
per_h       <- 60 / step_min # 4 readings per hour
max_gap_h   <- 1             # interpolate gaps up to 1 hour (4 readings)
sep_factor  <- 3             # peaks >= 3 x mean time to rise apart
trough_frac <- 1/3           # trough below 1/3 of the first peak

## Build the data frame extract_events() needs.
## Suspect readings are KEPT here (flow_with_suspect, made before they were set
## to NA above): many of the largest floods are flagged Suspect, so removing them
## would bias the peaks down. The effect of removing them is tested at the end.
rr <- data.frame(
  dateTime = flow_with_suspect$dateTime,
  value    = flow_with_suspect$value,            # negatives already set to NA
  quality  = flow_with_suspect$quality
)

## Hydrological year (Oct-Sep, labelled by the year it ends) and season
yr <- as.integer(format(rr$dateTime, "%Y"))
mo <- as.integer(format(rr$dateTime, "%m"))
rr$hydro_year <- yr + as.integer(mo >= 10)
rr$season     <- ifelse(mo %in% c(10:12, 1:3), "winter (Oct-Mar)", "summer (Apr-Sep)")

## Interpolate gaps up to max_gap_h; longer gaps stay NA
rr$flow <- zoo::na.approx(rr$value, maxgap = max_gap_h * per_h, na.rm = FALSE)

## Rolling medians: 3 h to locate peaks, 24 h to locate the start / end of events.
## NAs are ignored inside the window; the result is NA only if the whole window is NA.
roll_med <- function(x, hours) {
  k <- 2 * round(hours * per_h / 2) + 1          # odd window length
  zoo::rollapply(x, k, function(z) if (all(is.na(z))) NA_real_ else median(z, na.rm = TRUE),
                 fill = NA, align = "center")
}
rr$flow_smooth <- roll_med(rr$flow, 3)           # takes about a minute
rr$flow_day    <- roll_med(rr$flow, 24)

## Extract the events
ex <- extract_events(rr, u = u, sep_factor = sep_factor, trough_frac = trough_frac)
events_rr <- ex$events

## Results
nrow(events_rr)                                  # number of independent events
ex$rate                                          # events per year
ex$settings$T_rise_h                             # mean time to rise (hours)
ex$settings$min_sep_h                            # minimum separation between peaks (hours)
ex$n_candidates                                  # candidate peaks before declustering
ex$removed                                       # removed by the time rule / trough rule
sum(events_rr$clean)                             # events with no unfilled gap near start or peak

## Events per hydrological year
table(factor(events_rr$hydro_year, levels = sort(unique(rr$hydro_year))))

View(events_rr)

## Plot: flow with the event peaks marked
ggplot(rr, aes(dateTime, flow)) +
  geom_point(size = 0.2, colour = "grey40", na.rm = TRUE) +
  geom_hline(yintercept = u, colour = "red", linetype = "dashed") +
  geom_point(data = events_rr, aes(t_peak, q_peak), colour = "blue", size = 2) +
  scale_x_datetime(date_breaks = "5 years", date_labels = "%Y") +
  labs(x = "Date", y = "Flow (m3/s)",
       caption = "Blue points: independent event peaks (Robson & Reed). Red line: threshold u.") +
  theme_minimal()


## ---- Events per year for a range of thresholds -------------------------------
## Rolling medians are reused, so each threshold only re-runs the declustering.
thresholds <- seq(40, 200, by = 5)

results_rr <- do.call(rbind, lapply(thresholds, function(uu) {
  e <- tryCatch(extract_events(rr, u = uu, sep_factor = sep_factor, trough_frac = trough_frac),
                error = function(err) NULL)
  if (is.null(e)) return(data.frame(threshold = uu, number_of_events = NA,
                                    events_per_year = NA, min_sep_h = NA))
  data.frame(threshold        = uu,
             number_of_events = nrow(e$events),
             events_per_year  = e$rate,
             min_sep_h        = e$settings$min_sep_h)
}))

print(results_rr)

ggplot(results_rr, aes(threshold, events_per_year)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  geom_hline(yintercept = c(3, 5), colour = "red", linetype = "dashed") +
  labs(x = "Flow threshold (m3/s)", y = "Independent events per year") +
  theme_minimal()

subset(results_rr, events_per_year >= 3 & events_per_year <= 5)


## ---- Sensitivity: what if Suspect readings are removed? ----------------------
ex_nosus <- extract_events(rr, u = u, sep_factor = sep_factor, trough_frac = trough_frac,
                           drop_suspect = TRUE)
c(with_suspect = nrow(events_rr), without_suspect = nrow(ex_nosus$events))
