## =============================================================================
##  pot_functions.R  --  Peaks-Over-Threshold event extraction and helpers
##  SCS Project 1, Part 1.  Sourced by the main Rmd.  Base R + zoo only.
## =============================================================================
##
##  extract_events()  is the single function the whole project runs through.
##  Changing a threshold, an independence rule or the start-of-peak definition
##  is one argument, so the sensitivity analysis is a loop, not new code.
##
##  Algorithm (Robson & Reed 1999, as in Lecture 2):
##    1. Candidate peaks: each maximal run of flow_smooth > u is a "cluster";
##       its candidate peak is the index of the cluster's maximum.
##    2. Independence: two adjacent candidates are INDEPENDENT only if
##         (a) they are at least sep_factor x T_rise apart in time, where T_rise
##             is the average time to rise (trough -> peak) estimated from at
##             least n_rise_min clean, well-separated peaks, AND
##         (b) the lowest flow (trough) between them is below trough_frac x the
##             FIRST peak's flow ("at least two-thirds lower").
##       If a pair fails either test the smaller peak is dropped and the test
##       is repeated with the next neighbour, until every adjacent pair passes.
##    3. Start of peak (chosen by start_rule):
##         "trough": the minimum of flow_smooth between the previous
##                   independent peak (or the start of the record) and this one.
##         "rise":   the last time before the peak at which the flow was still
##                   falling over a rise_h-hour window, i.e. the onset of the
##                   sustained rise.  Never earlier than the trough.
##    4. End of peak: minimum of flow_smooth between this peak and the next, or
##       the first time flow recedes to the start level, whichever is earlier.
##    5. Summaries: the peak time is the maximum of the 3 h-smoothed series;
##       q_peak is the highest RAW recorded `value` within +/- 3 h of it (never
##       an interpolated or smoothed number); time_to_peak_h = t_peak - t_start.
##       n_suspect / n_missing count flagged readings between start and peak.
## -----------------------------------------------------------------------------

## ---- small helpers ----------------------------------------------------------

## Index of the minimum of x[a:b], returned as an index into x (NA-safe).
which_min_between <- function(x, a, b) {
  if (b < a) return(NA_integer_)
  seg <- x[a:b]
  if (all(is.na(seg))) return(NA_integer_)
  a + which.min(seg) - 1L
}

## Clusters of consecutive TRUE in a logical vector -> data.frame(start, end)
runs_of <- function(flag) {
  flag[is.na(flag)] <- FALSE
  r <- rle(flag)
  ends   <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  data.frame(start = starts[r$values], end = ends[r$values])
}

## ---- step 1: candidate peaks ------------------------------------------------

candidate_peaks <- function(s, u) {
  cl <- runs_of(s > u)
  if (nrow(cl) == 0) return(integer(0))
  vapply(seq_len(nrow(cl)), function(i) {
    cl$start[i] + which.max(s[cl$start[i]:cl$end[i]]) - 1L
  }, integer(1))
}

## ---- step 2: independence filter --------------------------------------------

## Drop the smaller of any adjacent pair that fails either rule.  Returns the
## surviving peak indices and how many were removed by each rule.
decluster <- function(s, peaks, min_sep_n, trough_frac) {
  removed <- c(time = 0L, trough = 0L)
  peaks <- sort(peaks)
  repeat {
    if (length(peaks) < 2) break
    dropped <- FALSE
    for (i in seq_len(length(peaks) - 1)) {
      p1 <- peaks[i]; p2 <- peaks[i + 1]
      sep_ok <- (p2 - p1) >= min_sep_n
      trough <- min(s[p1:p2], na.rm = TRUE)
      trough_ok <- trough < trough_frac * s[p1]
      if (!sep_ok || !trough_ok) {
        if (!sep_ok) removed["time"] <- removed["time"] + 1L else removed["trough"] <- removed["trough"] + 1L
        ## keep the larger peak (by smoothed flow), drop the other
        peaks <- peaks[-(if (s[p1] >= s[p2]) i + 1 else i)]
        dropped <- TRUE
        break                                   # restart scan with the new neighbour
      }
    }
    if (!dropped) break
  }
  list(peaks = peaks, removed = removed)
}

## ---- step 3: start of peak --------------------------------------------------

## Why "local": the Thames at Reading is baseflow-dominated, so the overall
## minimum between two independent floods is usually the previous SUMMER low,
## months earlier.  Using that would give times to peak of 100+ days that say
## nothing about the hydrograph shape.  Both rules below therefore look for the
## start of THIS rise, within a bounded look-back, and never cross the previous
## independent peak.
##   "trough": the last point before the peak that is the lowest value within
##             +/- win_n readings of itself (a genuine turning point at the
##             daily scale, not 15-minute jitter).
##   "rise":   the last point before the peak at which flow was still falling
##             over the previous win_n readings (onset of the sustained rise).
start_of_peak <- function(s, p, prev_bound, rule = c("trough", "rise"),
                          win_n = 96L, lookback_n = 60L * 96L) {
  rule <- match.arg(rule)
  lo   <- max(prev_bound, p - lookback_n, win_n + 1L)
  if (p - lo < 2L) return(lo)
  idx  <- seq(p - 1L, lo, by = -1L)               # walk backwards from the peak
  if (rule == "trough") {
    seg  <- s[(lo - win_n):(p + win_n)]
    seg[is.na(seg)] <- Inf                        # gaps can never be the trough
    rmin <- zoo::rollapply(seg, 2L * win_n + 1L, min, align = "center")   # min of +/- win_n
    ## rmin[j] corresponds to index lo + j - 1
    is_tr <- seg[(win_n + 1L):(length(seg) - win_n)] <= rmin
    cand  <- which(is_tr[(idx - lo + 1L)])
    if (length(cand) == 0) return(which_min_between(s, lo, p))
    return(idx[cand[1]])
  } else {
    d <- s[idx] - s[idx - win_n]
    d[is.na(d)] <- 1                              # treat gaps as "still rising"
    f <- which(d <= 0)[1]
    if (is.na(f)) return(which_min_between(s, lo, p))
    return(idx[f])
  }
}

## ---- the main function ------------------------------------------------------

extract_events <- function(df, u,
                           sep_factor   = 3,          # Robson & Reed: >= 3 x mean time to rise
                           trough_frac  = 1/3,        # trough must be below 1/3 of first peak
                           start_rule   = c("trough", "rise"),
                           win_h        = 24,         # window defining a trough / sustained rise (hours)
                           lookback_d   = 60,         # never look further back than this for the start
                           fixed_sep_h  = NULL,       # if given, replaces sep_factor x T_rise
                           n_rise_min   = 5,          # peaks needed to estimate T_rise
                           first_pass_sep_d = 7,      # crude separation used ONLY to estimate T_rise
                           drop_suspect = FALSE,      # sensitivity: treat Suspect as missing
                           step_min     = 15) {
  start_rule <- match.arg(start_rule)
  per_h <- 60 / step_min
  s   <- df$flow_smooth          # 3 h median: locates the peak
  sd  <- df$flow_day             # 24 h median: locates turning points (start / end).  A
                                 # daily-scale series ignores the 6-hour steps that gate
                                 # operations at Reading weir put on a rising limb.
  v   <- df$value
  q   <- as.character(df$quality)
  if (drop_suspect) { v[q == "Suspect"] <- NA; s[q == "Suspect"] <- NA; sd[q == "Suspect"] <- NA }
  n   <- length(s)

  ## 1. candidates
  cand <- candidate_peaks(s, u)
  if (length(cand) < 2) stop("fewer than two exceedance clusters at u = ", u)

  ## 2a. estimate the mean time to rise from clean, well-separated peaks
  ##     (first pass with a crude fixed separation, troughs below u)
  if (is.null(fixed_sep_h)) {
    fp   <- decluster(s, cand, min_sep_n = first_pass_sep_d * 24 * per_h, trough_frac = trough_frac)$peaks
    rise <- vapply(seq_along(fp), function(i) {
      prev <- if (i == 1) 1L else fp[i - 1]
      st   <- start_of_peak(sd, fp[i], prev, start_rule,
                            win_n = as.integer(win_h * per_h), lookback_n = as.integer(lookback_d * 24 * per_h))
      if (is.na(st)) NA_real_ else (fp[i] - st) / per_h
    }, numeric(1))
    rise <- rise[!is.na(rise)]
    if (length(rise) < n_rise_min) stop("fewer than ", n_rise_min, " clean peaks to estimate time to rise")
    T_rise_h  <- mean(rise)
    min_sep_h <- sep_factor * T_rise_h
  } else {
    T_rise_h  <- NA_real_
    min_sep_h <- fixed_sep_h
  }

  ## 2b. full independence filter
  dc    <- decluster(s, cand, min_sep_n = min_sep_h * per_h, trough_frac = trough_frac)
  peaks <- dc$peaks

  ## 3-5. build the event table
  ev <- lapply(seq_along(peaks), function(i) {
    p    <- peaks[i]
    prev <- if (i == 1) 1L else peaks[i - 1]
    nxt  <- if (i == length(peaks)) n else peaks[i + 1]
    st   <- start_of_peak(sd, p, prev, start_rule,
                          win_n = as.integer(win_h * per_h), lookback_n = as.integer(lookback_d * 24 * per_h))
    ## end: next trough, or when flow first recedes to the start level
    en   <- which_min_between(sd, p, nxt)
    rec  <- which(sd[p:nxt] <= sd[st])[1]
    if (!is.na(rec)) en <- min(en, p + rec - 1L)
    ## Peak TIME = maximum of the 3 h-smoothed series between start and end
    ## (robust to a single spurious 15-min reading).  Max FLOW = highest
    ## recorded reading within +/- 3 h of that time, so the value is always a
    ## real reading, never a smoothed or interpolated one.
    if (is.na(st)) return(NULL)
    seg_s <- s[st:en]
    if (all(is.na(seg_s))) return(NULL)
    p_s   <- st + which.max(seg_s) - 1L
    w     <- max(1L, p_s - 3L * per_h):min(n, p_s + 3L * per_h)
    if (all(is.na(v[w]))) return(NULL)
    q_peak <- max(v[w], na.rm = TRUE)
    p_raw  <- w[which.max(v[w])]
    data.frame(
      event_id       = i,
      t_start        = df$dateTime[st],
      t_peak         = df$dateTime[p_raw],
      t_end          = df$dateTime[en],
      q_start        = sd[st],
      q_peak         = q_peak,
      time_to_peak_h = (p_raw - st) / per_h,
      duration_h     = (en - st) / per_h,
      hydro_year     = df$hydro_year[p_raw],
      season         = df$season[p_raw],
      n_suspect      = sum(q[st:p_raw] == "Suspect"),
      n_missing      = sum(is.na(df$flow[st:p_raw])),      # readings in UNFILLED gaps during the rise
      peak_flag      = q[p_raw],
      ## clean = the two things we measure are not sitting next to an UNFILLED
      ## gap (> max_gap_h, i.e. still NA in df$flow): none within +/- 1 day of
      ## the start or of the peak.  Short filled gaps and gaps in the middle of
      ## a multi-day rise change neither summary.
      clean          = !is.na(sd[st]) &&
                       !anyNA(df$flow[max(1, st - 24 * per_h):min(n, st + 24 * per_h)]) &&
                       !anyNA(df$flow[max(1, p_raw - 24 * per_h):min(n, p_raw + 24 * per_h)]),
      i_start = st, i_peak = p_raw, i_end = en
    )
  })
  ev <- do.call(rbind, ev)
  ev$event_id <- seq_len(nrow(ev))

  years <- as.numeric(difftime(max(df$dateTime), min(df$dateTime), units = "days")) / 365.25
  list(events = ev,
       settings = list(u = u, T_rise_h = T_rise_h, min_sep_h = min_sep_h,
                       trough_frac = trough_frac, start_rule = start_rule,
                       win_h = win_h, drop_suspect = drop_suspect),
       n_candidates = length(cand), removed = dc$removed,
       rate = nrow(ev) / years, years = years)
}

## Full hydrograph of each event (for Part 2).  Returns a list of data frames.
event_hydrographs <- function(df, events) {
  lapply(seq_len(nrow(events)), function(i) {
    idx <- events$i_start[i]:events$i_end[i]
    data.frame(event_id = events$event_id[i],
               dateTime = df$dateTime[idx],
               hours_from_start = (idx - events$i_start[i]) * 0.25,
               flow = df$value[idx], quality = df$quality[idx])
  })
}

## ---- GPD helpers (ismev gives the fit; these give what ismev does not) -------

## m-year return level and delta-method 95% CI from an ismev gpd.fit object.
##   x_m = u + (sigma/xi) * ((m * lambda)^xi - 1),   lambda = events per year
gpd_return_level <- function(fit, u, lambda, m) {
  sig <- fit$mle[1]; xi <- fit$mle[2]; V <- fit$cov
  t(sapply(m, function(mm) {
    ml <- mm * lambda
    if (abs(xi) < 1e-8) {
      x  <- u + sig * log(ml)
      g  <- c(log(ml), sig * log(ml)^2 / 2)
    } else {
      x  <- u + sig / xi * (ml^xi - 1)
      g  <- c((ml^xi - 1) / xi,
              -sig / xi^2 * (ml^xi - 1) + sig / xi * ml^xi * log(ml))
    }
    se <- sqrt(as.numeric(t(g) %*% V %*% g))
    c(return_period = mm, level = x, lower = x - 1.96 * se, upper = x + 1.96 * se)
  }))
}
