
nestFinder <- function(dat, # Data (already subset for a burst; has to include id, burst, date, long, lat)
                       buffer, # Size of the buffer to compute revisitation
                       sea.start, # Earliest date to be considered within the breeding season
                       sea.end, # Latest date to be considered within the breeding season
                       nest.cycle, # Duration of nesting cycle
                       min.d.fix, # Minimum number of fixes for a day to be retained (data quality control)
                       min.consec, # Minimum number of consecutive days for a loc to be considered
                       min.top.att.perc, # Minimum % of fixes at a location on the day with max attendance (same as above but %)
                       min.days.att.perc, # Minimum % of days spent at location (between first and last visit)
                       min.pts, # Minimum number of points within a buffer to be considered as a candidate
                       discard_overlapping=TRUE) {

  burst.id <- unique(dat$burst)

  # Print current burst
  cat("****************************\n")
  cat(paste0("Processing ", burst.id, "\n"))
  cat("****************************\n\n")

  # Order by date and assign location id
  dat <- dat %>%
    arrange(date) %>%
    mutate(loc_id = 1:nrow(.)) %>%
    select(loc_id, everything())

  # Handle dates
  dat <- date_handler(dat, sea.start, sea.end)

  # Calculate distance matrix
  cat("Calculating distance matrix... ")
  dmat <- dist_mat(dat)
  cat("Done.\n")

  # Get potential nest candidates
  cat("Finding candidate nest locations... ")
  cands <- get_candidates(dm = dmat, buffer = buffer, min.pts = min.pts)
  cat("Done.\n")

  # Remove distance matrix to free up RAM
  rm(dmat)
  gc()

  # Summarize candidates
  cands_count <- candidate_summary(cands)

  # Join group ID back to the original data
  dat <- left_join(dat, cands, by = "loc_id")

  # Save computation time: discard group IDs that appear on days < min.consec
  keepers <- dat %>%
    group_by(group_id, reldate) %>%
    tally() %>%
    filter(n >= min.consec) %>%
    pull(group_id) %>%
    unique()

  # Subset data for group_ids of interest
  sub <- dat %>%
    filter(group_id %in% keepers)

  # Calculate revisitation stats
  cat("Calculating revisitation patterns... ")
  daily_stats <- revisit_stats(sub, sea.start, sea.end, min.d.fix)
  cat("Done.\n")

  # Filter group_ids that satisfy input criteria and add coordinates
  results <- daily_stats %>%
    filter(!is.na(attempt_start),
           !is.na(attempt_end),
           consec_days >= min.consec,
           perc_days_vis >= min.days.att.perc,
           perc_top_vis >= min.top.att.perc) %>%
    left_join(select(dat, loc_id, long, lat), by = c("group_id" = "loc_id")) %>%
    mutate(attempt_start = julian_to_date(sea.start, yr = min(year(sub$date))) + attempt_start) %>%
    mutate(attempt_end = julian_to_date(sea.start, yr = min(year(sub$date))) + attempt_end) %>%
    mutate(burst = burst.id) %>%
    select(burst,
           loc_id = group_id,
           long,
           lat,
           first_date,
           last_date,
           attempt_start,
           attempt_end,
           tot_vis,
           days_vis,
           consec_days,
           perc_days_vis,
           perc_top_vis) %>%
    arrange(desc(tot_vis))

  # Optional: deal with temporally overlapping attempts
  if (discard_overlapping) {

    results <- choose_overlapping(results)

  }

  cat("\nProcess completed!\n\n")
  return(results)

}

# Function to apply 'nestFinder()' to multiple bursts consecutively

nestR <- function(rawdata, # Has to include id, burst, date, long, lat
                  buffer, # Size of the buffer to compute revisitation
                  sea.start, # Earliest date to be considered within the breeding season
                  sea.end, # Latest date to be considered within the breeding season
                  nest.cycle, # Duration of nesting cycle
                  min.d.fix, # Minimum number of fixes for a day to be retained (data quality control)
                  min.consec, # Minimum number of consecutive days for a loc to be considered
                  min.top.att.perc, # Minimum % of fixes at a location on the day with max attendance (same as above but %)
                  min.days.att.perc, # Minimum % of days spent at location (between first and last visit)
                  min.pts, # Minimum number of points within a buffer to be considered as a candidate
                  discard_overlapping=TRUE) {

  #Record the start time
  start_time <- Sys.time()

  # Create a folder to store the outputs
  #Temporary folder name
  temp_name <- paste0("output/temp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  dir.create(temp_name, showWarnings = FALSE)

  # Create a vector of bursts to loop through
  bursts <- unique(rawdata$burst)

  for (i in 1:length(bursts)) {

    burst_id <- bursts[i]

    cat("****************************\n")
    cat(paste0("Burst ", i, " of ", length(bursts), "\n"))

    dat <- rawdata %>%
      filter(burst==burst_id)

    nests <- nestFinder(dat, # Data (already subset for a burst; has to include id, burst, date, long, lat)
                        buffer, # Size of the buffer to compute revisitation
                        sea.start, # Earliest date to be considered within the breeding season
                        sea.end, # Latest date to be considered within the breeding season
                        nest.cycle, # Duration of nesting cycle
                        min.d.fix, # Minimum number of fixes for a day to be retained (data quality control)
                        min.consec, # Minimum number of consecutive days for a loc to be considered
                        min.top.att.perc, # Minimum % of fixes at a location on the day with max attendance (same as above but %)
                        min.days.att.perc, # Minimum % of days spent at location (between first and last visit)
                        min.pts, # Minimum number of points within a buffer to be considered as a candidate
                        discard_overlapping=TRUE)

    saveRDS(nests, paste0(temp_name, "/nests_", burst_id, ".rds"))

    rm(nests)
    gc()

  }

  cat("\nCompleted processing of all bursts.\n")

  # At the end
  # Combine results
  files <- list.files(temp_name, full.names = TRUE)
  results <- data.frame()
  for (f in files) {
    temp <- readRDS(f)
    results <- rbind(results, temp)
  }
  # Delete temporary folder and files within it
  file.remove(files)
  file.remove(temp_name)

  #Record the end time
  end_time <- Sys.time()

  #Calculate the difference
  total_time <-  difftime(end_time, start_time, units = "min") %>%
    as.numeric() %>%
    round(1)

  #Report time
  cat(paste0("\nTotal processing time: ", total_time, " minutes.\n"))

  # Return results
  return(results)
}
