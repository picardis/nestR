#' Find nest locations from GPS data
#'
#' \code{find_nests} finds nest locations from GPS data based on patterns of
#' location revisitation
#' @details Data passed to the argument \code{gps_data} needs to be split in
#' individual-years labelled each as a separate \code{burst}. We recommend
#' dividing the data so that seasonal nesting activities are contained within
#' single bursts. Cutting data at a day that is not likely to overlap with
#' nesting is best.
#'
#' Data must include the following columns: a burst identifier (\code{burst}),
#' date-time (\code{date}), and long/lat coordinates (\code{long}, \code{lat}).
#'
#' Patterns of revisitation to repeatedly visited locations are used to
#' identify potential nesting locations. Due to both movement and GPS error,
#' recorded points around recurrently visited locations are expected to be
#' scattered around the true revisited location. To account for this
#' scattering, the user defines a \code{buffer} value which will be used
#' to group points falling within a buffer distance from each other.
#'
#' When grouping, several points peripheral to a true revisited location
#' may compete in grouping points around them. We term these 'competing
#' points'. If the buffers of two points do not overlap, those points are
#' not competing. Among competing points, only one point is selected, chosen
#' as the one that incorporates the most other points within its buffer.
#' A top candidate is selected for each cluster of competing points, i.e., one
#' representative for each cluster around a revisited location.
#'
#' To speed up calculations, the user can define \code{min_pts} as the minimum
#' number of points that need to fall within the buffer for a point to be
#' considered as a potential nest candidate. This discards isolated points from
#' consideration as revisited locations.
#'
#' The arguments \code{sea_start} and \code{sea_end} are used to delimit the
#' nesting season. The user can pass either a Julian day or a date. If
#' inputting dates, the year can be a dummy year which will get automatically
#' updated each time to the correct year for the current burst. If working
#' with a species for which the temporal limits of the nesting season are not
#' well-defined, the user can input a range of dates that covers the entire
#' year. Nonetheless, we recommend ensuring that \code{sea_start} and
#' \code{sea_end} are set so that nesting attempts are not split between
#' bursts. For example, for a species that nests from October to September,
#' enter October 1st as start date and September 30th as end date and not,
#' for example, January 1st-December 31st.
#'
#' The argument \code{nest_cycle} is the duration (in days) of a complete
#' nesting attempt, i.e., the time necessary for an individual to successfully
#' complete reproduction.
#'
#' Once recurrently visited locations are identified, the function computes,
#' for each of them:
#'
#' \itemize{
#'
#' \item the first and last day when the location was visited;
#' \item the total number of visits;
#' \item the number of days in which it was visited;
#' \item the percent of days visited between the days of first and last visit;
#' \item the attendance (\% of fixes at the location) on the day with the
#' most visits;
#' \item the longest series of consecutive days visited;
#' \item the estimated start and end dates of the nesting attempt.
#'
#'  }
#'
#' On days when no visit was recorded, two cases are possible: either the nest
#' was truly not visited, or visits were missed. On days with few fixes, there
#' is a higher chance of missing a visit given that it happened. Missed visit
#' detections can interrupt an otherwise continuos strike of days visited.
#' To counteract possible issues due to missed visit detections, the user can
#' define \code{min_d_fix} as the minimum number of fixes that have to be
#' available in a day with no visits for that day to be retained when counting
#' consecutive days visited. If a day with no visits and fewer fixes than
#' \code{min_d_fix} interrupts a sequence of consecutive days visited, it
#' does not get considered and the sequence gets counted as uninterrupted.
#'
#' The remaining arguments are used to filter results. The user can set
#' minimum values for each of the following revisitation statistics:
#'
#' \itemize{
#'
#' \item the longest series of consecutive days visited (\code{min_consec});
#' \item the attendance (\% of fixes at the location) on the day with the
#' most visits (\code{min_top_att});
#' \item the percent of days visited between the days of first and last visit
#' \code{min_days_att};
#'
#'  }
#'
#' Among candidate nests, only those whose values for the above parameters
#' exceed the user-defined minima are returned.
#'
#' If the results include any temporally overlapping nesting attempts, the
#' user can opt to only keep one among those. If \code{discard_overlapping}
#' is set to \code{TRUE} (default), only the candidate nest with the most
#' visits is kept among temporally overlapping ones, and the others get
#' discarded. This is based on the rationale that an individual cannot
#' simultaneously nest at more than one location. The location that is visited
#' the most is assumed to be the most likely true nest. On the other hand,
#' setting \code{discard_overlapping} to \code{FALSE} retains all candidate
#' nests in the results.
#'
#' After identifying all nests that correspond to the criteria defined in
#' input, the function appends a new column to the original GPS data that
#' flags fixes recorded at a nest with the location identifier of that nest.
#' The result is a history of nest visits for each burst.
#'
#' @param gps_data \code{data.frame} of movement data. Needs to include burst,
#' date, long, lat
#' @param buffer Size of the buffer to compute location revisitation
#' @param min_pts Minimum number of points within a buffer
#' @param sea_start Character string. Earliest date to be considered within the
#' breeding season. Month and day, format \code{"mm-dd"}
#' @param sea_end Character string. Latest date to be considered within the
#' breeding season. Month and day, format \code{"mm-dd"}
#' @param nest_cycle Duration of nesting cycle
#' @param min_d_fix Minimum number of fixes for a day to be retained if no
#' nest visit was recorded
#' @param min_consec Minimum number of consecutive days visited
#' @param min_top_att Minimum percent of fixes at a location on the day
#' with maximum attendance
#' @param min_days_att Minimum percent of days spent at a location
#' between first and last visit
#' @param discard_overlapping If results include temporally overlapping
#' attempts, select only one among those? Defaults to \code{TRUE}.
#' @return Returns a \code{list} of two elements: first, `nests`, a
#' \code{data.frame} of nest locations and associated revisitation stats;
#' second, `visits`, a \code{data.frame} of nest revisitation histories.
#'
#' @export
find_nests <- function(gps_data,
                  buffer,
                  min_pts,
                  sea_start,
                  sea_end,
                  nest_cycle,
                  min_d_fix,
                  min_consec,
                  min_top_att,
                  min_days_att,
                  discard_overlapping = TRUE) {

  # Check format of input data
  check_input(gps_data)

  # Record the start time
  start_time <- Sys.time()

  # Create a folder to store the outputs
  # Temporary folder name
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  temp_name <- paste0("output/temp_", timestamp)
  dir.create(temp_name, showWarnings = FALSE, recursive = TRUE)

  # Initialize error log
  cat("Error log \n\n", file = paste0("output/errorlog", timestamp, ".txt"))

  # Create a vector of bursts to loop through
  bursts <- unique(gps_data$burst)

  for (i in 1:length(bursts)) {

    burst_id <- bursts[i]

    cat("****************************\n")
    cat(paste0("Burst ", i, " of ", length(bursts), "\n"))

    dat <- gps_data %>%
      dplyr::filter(.data$burst == burst_id)

    # Print current burst
    cat("****************************\n")
    cat(paste0("Processing ", burst_id, "\n"))
    cat("****************************\n\n")

    # Order by date and assign location id
    dat <- dat %>%
      dplyr::arrange(.data$date) %>%
      dplyr::mutate(loc_id = 1:nrow(.)) %>%
      dplyr::select(.data$loc_id, dplyr::everything())

    # Handle dates
    dates_out <- date_handler(dat, sea_start, sea_end)

    # dates_out is a list; pull the data from it
    dat <- dates_out$dat

    # Handle cases where there are no data within the nesting season
    if (nrow(dat) == 0) {

      cat(paste0("Burst ", burst_id,
                 ": no data available within the nesting season\n"),
          file = paste0("output/errorlog", timestamp, ".txt"),
          append = TRUE)

      next()

      }

    # Calculate distance matrix
    cat("Calculating distance matrix... ")
    dmat <- dist_mat(dat)
    cat("Done.\n")

    # Get potential nest candidates
    cat("Finding candidate nest locations... ")
    cands <- get_candidates(dm = dmat, buffer = buffer, min_pts = min_pts)
    cat("Done.\n")

    # Handle cases where there are no candidates
    if (nrow(cands) == 0) {

      cat(paste0("Burst ", burst_id,
                 ": no revisited locations found\n"),
          file = paste0("output/errorlog", timestamp, ".txt"),
          append = TRUE)

      next()

    }

    # Remove distance matrix to free up RAM
    rm(dmat)
    gc()

    # Summarize candidates
    cands_count <- candidate_summary(cands)

    # Join group ID back to the original data
    dat <- dplyr::left_join(dat, cands, by = "loc_id")

    # Save computation time: discard group IDs that appear on < 2 days
    keepers <- dat %>%
      dplyr::group_by(.data$group_id) %>%
      dplyr::summarize(n = dplyr::n_distinct(.data$reldate)) %>%
      dplyr::filter(.data$n >= 2) %>%
      dplyr::pull(.data$group_id) %>%
      unique()

    # Subset data for group_ids of interest
    sub <- dat %>%
      dplyr::filter(.data$group_id %in% keepers)

    # Handle cases where there are no keepers
    if (nrow(sub) == 0) {

      cat(paste0("Burst ", burst_id,
                 ": no locations revisited for more than ",
                 2,
                 " days\n"),
          file = paste0("output/errorlog", timestamp, ".txt"),
          append = TRUE)

      next()

    }

    # Calculate revisitation stats
    cat("Calculating revisitation patterns... ")
    daily_stats <- revisit_stats(dat = dat,
                                 sub = sub,
                                 sea_start = sea_start,
                                 sea_end = sea_end,
                                 min_d_fix = min_d_fix,
                                 min_consec = min_consec,
                                 nest_cycle = nest_cycle)
    cat("Done.\n")

    # Filter group_ids that satisfy input criteria and add coordinates
    nests <- daily_stats %>%
      dplyr::filter(!is.na(.data$attempt_start),
             !is.na(.data$attempt_end),
             .data$consec_days >= min_consec,
             .data$perc_days_vis >= min_days_att,
             .data$perc_top_vis >= min_top_att) %>%
      dplyr::left_join(dplyr::select(.data$dat,
                                     .data$loc_id,
                                     .data$long,
                                     .data$lat),
                       by = c("group_id" = "loc_id")) %>%
      dplyr::mutate(attempt_start = lubridate::ymd(dates_out$actual_start) +
                      attempt_start) %>%
      dplyr::mutate(attempt_end = lubridate::ymd(dates_out$actual_start) +
               attempt_end) %>%
      dplyr::mutate(burst = burst_id) %>%
      dplyr::select(.data$burst,
             loc_id = .data$group_id,
             .data$long,
             .data$lat,
             .data$first_date,
             .data$last_date,
             .data$attempt_start,
             .data$attempt_end,
             .data$tot_vis,
             .data$days_vis,
             .data$consec_days,
             .data$perc_days_vis,
             .data$perc_top_vis) %>%
      dplyr::arrange(dplyr::desc(.data$tot_vis))

    # Handle cases where no nests passed the filter
    if (nrow(nests) == 0) {

      cat(paste0("Burst ", burst_id,
                 ": no locations found for the specified set of parameters\n"),
          file = paste0("output/errorlog", timestamp, ".txt"),
          append = TRUE)

      next()

    }

    # Optional: deal with temporally overlapping attempts
    if (discard_overlapping) {

      nests <- choose_overlapping(nests)

    }

    # Format visit history data.frame
    visits <- dat %>%
      dplyr::mutate(group_id = dplyr::case_when(
        .data$group_id %in% nests$loc_id ~ .data$group_id,
        TRUE ~ 0L
      )) %>%
      dplyr::select(.data$burst,
                    .data$date,
                    .data$long,
                    .data$lat,
                    loc_id = .data$group_id)

    cat("\nProcess completed!\n\n")

    saveRDS(nests, paste0(temp_name, "/nests_", burst_id, ".rds"))
    saveRDS(visits, paste0(temp_name, "/visits_", burst_id, ".rds"))

    rm(nests)
    rm(visits)
    gc()

  }

  cat("\nCompleted processing of all bursts.\n")

  # At the end
  # Combine results on nests
  files_nests <- list.files(temp_name, pattern = "nests_", full.names = TRUE)
  nests <- data.frame()
  for (f in files_nests) {
    temp <- readRDS(f)
    nests <- rbind(nests, temp)
  }
  # Combine results on loc2group
  files_visits <- list.files(temp_name, pattern = "visits_", full.names = TRUE)
  visits <- data.frame()
  for (f in files_visits) {
    temp <- readRDS(f)
    visits <- rbind(visits, temp)
  }
  # Combine all
  results <- list(nests, visits)
  names(results) <- c("nests", "visits")
  # Delete temporary folder and files within it
  file.remove(files_nests)
  file.remove(files_visits)
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
