
#' Perform sensitivity analysis of buffer size for nest detection
#'
#' \code{compare_buffers} compares nest identification results obtained
#' using buffers of different size. This function is useful to perform
#' sensitivity analyses of nest-detection results as the chosen buffer
#' size varies. This helps both to demonstrate robustness of results
#' independent of buffer size as well as to choose an appropriate buffer
#' size for the species and data at hand. Users can specify a set of
#' buffers that they wish to compare. Then, \code{compare_buffers} applies
#' the same procedure as \code{\link{find_nests}} to the data using each
#' of the different buffers. For the sake of keeping computation time at a
#' manageable level, if the full dataset is large, it is recommended to
#' only use a subset of data to test buffers on. If data on known nest
#' locations are provided, \code{compare_buffers} also computes the
#' following performance metrics for each buffer size:
#'
#' \itemize{
#'
#' \item Positive predictive value, i.e., the proportion of identified nests
#' that are known to be true nests;
#' \item Sensitivity, i.e., the proportion of known nests that were successfully
#' identified;
#' \item False negative rate, i.e., the proportion of known nests that we failed
#' to identify.
#' }
#'
#' The false positive rate, i.e., the proportion of spurious nests among
#' those identified, is not computed because it would require the assumption
#' that any nests that are detected but are not known are spurious, which
#' is not reasonable in most situations. Users can evaluate which buffer size
#' is more appropriate based on the performance metrics. Regardless of
#' whether data on known nest locations are provided, \code{compare_buffers}
#' computes the total number of nests found for each buffer size as well as
#' the number of nests found for each burst. If the user provides data on known
#' nests, they can also specify a value of spatial tolerance to use as a cutoff
#' to establish whether a nest was correctly identified: since coordinates of
#' detected nests rarely match exactly the actual (known) coordinates of the
#' nest, users can define which distance between real and estimated location
#' they are willing to accept at most. If no data on true nests are
#' available, these numbers can provide an indication of whether a given
#' buffer size likely results in over- or underestimation of the number of
#' nests. \code{compare_buffers} takes almost exactly the same arguments as
#' \code{\link{find_nests}}, except for the argument `buffer` which is here
#' replaced with `buffers` and for the additional arguments `known_coords`
#' and `sp_tol`.
#'
#' @param gps_data \code{data.frame} of movement data. Needs to include burst,
#' date, long, lat
#' @param buffers A vector of buffer sizes
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
#' @param known_coords \code{data.frame} of coordinates for known nests. Needs
#' to include burst, long, lat.
#' @param sp_tol Integer. Spatial tolerance value: maximum distance tolerated
#' between the estimated location of a nest and its actual (known) location.
#' Defaults to 100 meters.
#' @return Returns a \code{list} of variable length including:
#'
#' \itemize{
#'
#' \item As many data frames of nest locations as the number of buffers
#' specified (formatted like `nests` in the output of
#' \code{\link{find_nests}});
#' \item A \code{data.frame} reporting the total number of nests identified
#' with each buffer size;
#' \item A \code{data.frame} reporting the total number of nests identified
#' with each buffer size for each burst;
#' \item If `known_coords` is provided, a \code{data.frame} reporting the
#' positive predictive value obtained with each buffer size;
#' \item If `known_coords` is provided, a \code{data.frame} reporting the
#' sensitivity obtained with each buffer size;
#' \item If `known_coords` is provided, a \code{data.frame} reporting the
#' false negative rate obtained with each buffer size;
#' }
#'
#' @export
compare_buffers <- function(gps_data,
                            buffers,
                            known_coords,
                            sp_tol = 100,
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
      dplyr::filter(burst==burst_id)

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
    cat("Finding candidate nest locations for each buffer size... ")
    cands_list <- get_candidates_multi(dmat = dmat,
                                       buffers = buffers,
                                       min_pts = min_pts)
    cat("Done.\n")

    # Remove distance matrix to free up RAM
    rm(dmat)
    gc()

    # Create list to save results for each buffer
    res_buffer <- list()

    for (k in 1:length(cands_list)) {

    # Need to start with a fresh 'dat' at every k
    dat_buff <- dat

    # Handle cases where there are no candidates
    if (nrow(cands_list[[k]]) == 0) {

      cat(paste0("Burst ", burst_id,
                 ": no revisited locations found\n"),
          file = paste0("output/errorlog", timestamp, ".txt"),
          append = TRUE)

      next()

    }

    # Summarize candidates
    cands_count <- candidate_summary(cands = cands_list[[k]])

    # Join group ID back to the original data
    dat_buff <- dplyr::left_join(dat, cands_list[[k]], by = "loc_id")

    # Save computation time: discard group IDs that appear on < 2 days
    keepers <- dat_buff %>%
      dplyr::group_by(.data$group_id, .data$reldate) %>%
      dplyr::tally() %>%
      dplyr::filter(.data$n >= 2) %>%
      dplyr::pull(.data$group_id) %>%
      unique()

    # Subset data for group_ids of interest
    sub <- dat_buff %>%
      dplyr::filter(.data$group_id %in% keepers)

    # Handle cases where there are no keepers
    if (nrow(sub) == 0) {

      cat(paste0("Burst ", burst_id,
                 ": no locations revisited for more than ",
                 min_consec,
                 " days\n"),
          file = paste0("output/errorlog", timestamp, ".txt"),
          append = TRUE)

      next()

    }

    # Calculate revisitation stats
    cat(paste0("Calculating revisitation patterns for buffer ", k, " of ",
               length(buffers), "... "))
    daily_stats <- revisit_stats(dat = dat_buff,
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
      dplyr::left_join(dplyr::select(dat_buff,
                                     .data$loc_id,
                                     .data$long,
                                     .data$lat),
                       by = c("group_id" = "loc_id")) %>%
      dplyr::mutate(attempt_start = ymd(dates_out$actual_start) +
                      .data$attempt_start) %>%
      dplyr::mutate(attempt_end = ymd(dates_out$actual_start) +
                      .data$attempt_end) %>%
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
      dplyr::arrange(desc(.data$tot_vis))

    # Handle cases where no nests passed the filter
    if (nrow(sub) == 0) {

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

    # Store results for the current buffer
    res_buffer[[k]] <- nests

    names(res_buffer[k]) <- buffers[k]

    }

    cat("\nProcess completed!\n\n")

    saveRDS(res_buffer, paste0(temp_name, "/buffer_comparison_", burst_id, ".rds"))

    rm(res_buffer)
    gc()

  }

  cat("\nCompleted processing of all bursts.\n")

  # At the end
  # Combine results
  # Each file (per burst) is a list with an element per buffer.
  # Convert into a list with an element per buffer where each element is a data.frame for all bursts.
  files_buffers <- list.files(temp_name, pattern = "buffer_comparison_", full.names = TRUE)
  buffercomp <- as.list(rep(NA, length.out = length(buffers)))

  for (f in 1:length(files_buffers)) {

      temp <- readRDS(files_buffers[f])

      # Handle situation where list of results is completely empty
    if(length(buffercomp) == length(temp)) {

      temp <- lapply(temp, function(x) {
        if (is.null(x)) { # handle situation where one element is empty
          y <- data.frame(burst = NA,
                          loc_id = NA,
                          long = NA,
                          lat = NA,
                          first_date = NA,
                          last_date = NA,
                          attempt_start = NA,
                          attempt_end = NA,
                          tot_vis = NA,
                          days_vis = NA,
                          consec_days = NA,
                          perc_days_vis = NA,
                          perc_top_vis = NA)
          return(y)
        } else if (nrow(x) == 0) { # handle situation where no nest was kept after filtering
          y <- data.frame(burst = NA,
                          loc_id = NA,
                          long = NA,
                          lat = NA,
                          first_date = NA,
                          last_date = NA,
                          attempt_start = NA,
                          attempt_end = NA,
                          tot_vis = NA,
                          days_vis = NA,
                          consec_days = NA,
                          perc_days_vis = NA,
                          perc_top_vis = NA)
          return(y)
        } else {
          return(x)
        }
      })

      buffercomp <- purrr::map2(buffercomp, temp, rbind)
    }
  }

  buffercomp <- lapply(buffercomp, FUN = function(x){
    x <- x[complete.cases(x),]
    if(nrow(x) > 0) {
      rownames(x) <- 1:nrow(x)
    }
    return(x)
  })
  names(buffercomp) <- buffers


  # Delete temporary folder and files within it
  file.remove(files_buffers)
  file.remove(temp_name)

  # Count results and compute performance metrics
  cat("\nComputing metrics...\n")

  #Total number of nests
  tot_nests <- data.frame()
  for (n in 1:(length(buffercomp))) {

    temp <- cbind.data.frame(buffer_size = names(buffercomp[n]),
                             n = nrow(buffercomp[[n]]))

    tot_nests <- rbind(tot_nests, temp)
  }

  #Number of nests per individual
  nests_per_ind <- data.frame()
  for (n in 1:(length(buffercomp))) {
    if (nrow(buffercomp[[n]] > 0)) {
    temp <- buffercomp[[n]] %>%
      dplyr::group_by(.data$burst) %>%
      dplyr::tally() %>%
      as.data.frame() %>%
      cbind(buffer_size = names(buffercomp)[n]) %>%
      dplyr::select(.data$burst, .data$buffer_size, .data$n)
    nests_per_ind <- rbind(nests_per_ind, temp) %>%
      dplyr::arrange(.data$burst)
  }}

  #If known nests are provided, also return performance metrics
  if (!is.null(known_coords)) {

  # Fix column names
  names(known_coords) <- c("burst", "true_long", "true_lat")
  # Change burst to character if it is not already
  known_coords$burst <- as.character(known_coords$burst)

  #Get rid of bursts that don't have at least 'min_consec' days of data
  enough <- gps_data %>%
    dplyr::group_by(.data$burst) %>%
    dplyr::summarize(days_data = length(unique(
      lubridate::as_date(.data$date)
      ))
      ) %>%
    dplyr::filter(.data$days_data >= min_consec) %>%
    dplyr::pull(.data$burst)
  known_coords <- known_coords %>%
    dplyr::filter(.data$burst %in% enough)
  buffercomp_copy <- lapply(buffercomp, FUN = function(x){
    y <- x[x$burst %in% enough,]
    return(y)
  })

  #Positive predictive value
  # Number of known nests found
  ppv_num <- unlist(lapply(buffercomp_copy, FUN = function(x) {
    if (nrow(x > 0)) {
      y <- x %>%
      dplyr::left_join(known_coords, by = "burst") %>%
      dplyr::mutate(dist = geosphere::distGeo(cbind(.data$true_long,
                                                    .data$true_lat),
                                              cbind(.data$long,
                                                    .data$lat))) %>%
      dplyr::filter(.data$dist <= sp_tol) %>%
      nrow()
    return(y)
  }}))

  # Total number of nests found
  ppv_den <- unlist(lapply(buffercomp_copy, FUN = function(x) {
    if (nrow(x > 0)) {
      y <- x %>%
      dplyr::filter(.data$burst %in% known_coords$burst) %>%
      nrow()}}))
  # PPV
  ppv <- data.frame(ppv = ppv_num/ppv_den*100)
  ppv <- cbind(ppv, buffer_size = rownames(ppv)) %>%
    dplyr::select(.data$buffer_size, .data$ppv)
  rownames(ppv) <- NULL

  # Sensitivity
  # Numbr of known nests found
  sen_num <- ppv_num
  # Number of nests we expected to find
  sen_den <- rep(nrow(known_coords), length(ppv_num))
  # Sensitivity
  sens <- data.frame(sens = sen_num/sen_den*100)
  sens <- cbind(sens, buffer_size = rownames(sens)) %>%
    dplyr::select(.data$buffer_size, .data$sens)
  rownames(sens) <- NULL

    # False negatives
    # Number of known nests we failed to find
    fn_num <- sen_den - sen_num
    # Number of nests we expected to find
    fn_den <- sen_den
    # False negative rate
    fn <- data.frame(fn = fn_num/fn_den*100)
    fn <- cbind(fn, buffer_size = rownames(fn)) %>%
      dplyr::select(.data$buffer_size, .data$fn)
    rownames(fn) <- NULL

  }

  # Append performance metrics to output
  buffercomp$tot_nests <- tot_nests
  buffercomp$nests_per_ind <- nests_per_ind
  if (!is.null(known_coords)) {
    buffercomp$ppv <- ppv
    buffercomp$sens <- sens
    buffercomp$fn <- fn
  }

  #Record the end time
  end_time <- Sys.time()

  #Calculate the difference
  total_time <-  difftime(end_time, start_time, units = "min") %>%
    as.numeric() %>%
    round(1)

  #Report time
  cat(paste0("\nTotal processing time: ", total_time, " minutes.\n"))

  # Return results
  return(buffercomp)

}

#' Compute nest-detection performance metrics
#'
#' \code{perf_metrics} calculates nest detection performance metrics
#' based on the results of \code{\link{find_nests}} and data on known
#' nest locations. The user defines the spatial tolerance threshold between
#' the estimated location of a nest and its real (known) location. The
#' performance metrics are defined as follows:
#'
#' \itemize{
#'
#' \item Positive predictive value, i.e., the proportion of identified nests
#' that are known to be true nests;
#' \item Sensitivity, i.e., the proportion of known nests that were successfully
#' identified;
#' \item False negative rate, i.e., the proportion of known nests that we failed
#' to identify.
#' }
#'
#' The false positive rate, i.e., the proportion of spurious nests among
#' those identified, is not computed because it would require the assumption
#' that any nests that are detected but are not known are spurious, which
#' is not reasonable in most situations.
#'
#' @param gps_data Original \code{data.frame} of movement data. Needs to include
#' burst, date, long, lat
#' @param nest_info Output of \code{find_nests}
#' @param known_coords \code{data.frame} of coordinates for known nests. Needs
#' to include burst, long, lat
#' @param min_consec The minimum number of consecutive days visited that was
#' used when running \code{find_nests}
#' @param sp_tol Integer. Spatial tolerance value: maximum distance tolerated
#' between the estimated location of a nest and its actual (known) location.
#' Defaults to 100 meters.
#'
#' @export
perf_metrics <- function(gps_data,
                         nest_info,
                         known_coords,
                         min_consec,
                         sp_tol = 100){

  #Grab nests from output of find_nests()
  dat <- nest_info$nests

  # Rename coordinate columns in known_coords
  names(known_coords) <- c("burst", "true_long", "true_lat")

  #Get rid of bursts that don't have at least 'min_consec' days of data
  enough <- gps_data %>%
    dplyr::group_by(.data$burst) %>%
    dplyr::summarize(days_data = length(unique(
      lubridate::as_date(.data$date)
      ))
      ) %>%
    dplyr::filter(.data$days_data >= min_consec) %>%
    dplyr::pull(.data$burst)
  known_coords <- known_coords %>%
    dplyr::filter(.data$burst %in% enough)
  dat_copy <- dat[dat$burst %in% enough,]

  #Positive predictive value
  # Number of known nests found
  ppv_num <- dat %>%
    dplyr::left_join(known_coords, by = "burst") %>%
    dplyr::mutate(dist = geosphere::distGeo(cbind(.data$true_long,
                                                  .data$true_lat),
                                            cbind(.data$long,
                                                  .data$lat))) %>%
    dplyr::filter(.data$dist <= sp_tol) %>%
    nrow()
  # Total number of nests found
  ppv_den <- dat %>%
    dplyr::filter(.data$burst %in% known_coords$burst) %>%
    nrow()
  # PPV
  ppv <- ppv_num/ppv_den*100

  # Sensitivity
  # Numbr of known nests found
  sen_num <- ppv_num
  # Number of nests we expected to find
  sen_den <- nrow(known_coords)
  # Sensitivity
  sens <- sen_num/sen_den*100

  # False negatives
  # Number of known nests we failed to find
  fn_num <- sen_den - sen_num
  # Number of nests we expected to find
  fn_den <- sen_den
  # False negative rate
  fn <- fn_num/fn_den*100

  # Bundle results
  res <- list(ppv, sens, fn)
  names(res) <- c("ppv", "sens", "fn")

  # Return results
  return(res)

}
