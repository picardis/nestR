#' Run-length encoding of nest visits
#'
#' \code{visit_rle} calculates the run-length encoding of visits based on
#' the daily history of revisitation of a candidate nest.
#'
#' @details Used with \code{lapply} inside function \code{revisit_stats}.
#' Performs run-length encoding of nest visits and formats as
#' \code{data.frame} for later use.
#' @param x \code{data.frame} of daily revisitation history of a
#' candidate nest
#' @return Returns \code{data.frame} of run-length encoding of nest visits.
#' @export
visit_rle <- function(x){
  # Calculate rle
  rl <- rle(x[["visited"]])
  # Convert to data.frame
  rl_df <- data.frame(lengths=rl$lengths, values=rl$values,
                      end = cumsum(rl$lengths)) %>%
    mutate(start = end - lengths + 1) %>%
    select(lengths, values, start, end)
  return(rl_df)
}

#' Calculate consecutive days visited
#'
#' \code{rle_to_consec} calculates the longest sequence of consecutive days a
#' candidate nest was visited.
#'
#' @details Used with \code{lapply} inside function \code{revisit_stats}.
#' Computes longest series of consecutive days a candidate nest was visited.
#' Takes as input the output of \code{visit_rle}.
#' @param rl_df \code{data.frame} of run-length encoding of nest visits
#' @return Returns maximum number of consecutive days when the candidate
#' nest was visited.
#' @export
rle_to_consec <- function(rl_df) {
  rl_df <- rl_df  %>%
    filter(values) %>%
    filter(lengths == max(lengths))
  return(max(rl_df$lengths))
}

#' Determine start and end dates of nesting attempt
#'
#' \code{attempt_limits} Determines the start and end dates of the potential
#' nesting attempt based on the patterns of revisitation of the candidate nest.
#'
#' @details Used with \code{lapply} inside function \code{revisit_stats}.
#'
#' The function uses a moving window of size \code{nest_cycle} to find the
#' most likely time range of the nesting attempt.
#'
#' Attendance is expected to be maximum during the initial phase of nesting,
#' so we assume that the longest series of consecutive days corresponds to
#' the beginnig of the nesting attempt. Therefore, the moving window starts
#' at the beginning of the first series of consecutive days longer than
#' \code{min_consec}. This helps discard any early visits to the nest before
#' the actual start of the attempt.
#'
#' After that, the function slides a moving window of size \code{nest_cycle}
#' to the data until it hits the last visit. The window that encompasses the
#' highest number of visits is selected as most likely delimiting the nesting
#' attempt.
#'
#' If the last nest visit is on the end date of the window or later,
#' the end of the attempt is set at the end date of the window (which is
#' the start date + \code{nest_cycle}). This helps discard any later visits to
#' the nest after the attempt is already concluded.
#'
#' If the last nest visit is earlier than the ending date of the window, the
#' end of the attempt is set at the date of the last visit.
#'
#' @param x \code{data.frame} of daily revisitation history of a
#' candidate nest
#' @param min_consec Integer. Minimum number of consecutive days a location
#' needs to be visited to be considered as a candidate nest. See Details
#' @param nest_cycle Integer. Duration (in days) of a complete nesting cycle
#' @return Returns \code{data.frame} with start and end dates of the
#' attempt and number of nest visits within it.
#' @export
attempt_limits <- function(x, min_consec, nest_cycle){

  # Calculate the rle
  rl_df <- visit_rle(x)

  # Find the starting point of the moving window
  mw_start_check <- rl_df %>%
    filter(values) %>%
    filter(lengths > min_consec)

  # If there are no durations > min_consec, return NAs for this group_id
  if(nrow(mw_start_check) == 0){
    mw_res <- data.frame(group_id = unique(x$group_id),
                         attempt_start = NA,
                         attempt_end = NA)
    return(mw_res)
  }

  mw_start_ind <- mw_start_check %>%
    pull(start) %>%
    min()

  # Get the corresponding reldate
  mw_start <- min(x$reldate[mw_start_ind])

  # Moving window

  #Max reldate in the data
  max_reldate <- max(x$reldate)

  # Initialize results data.frame
  mw_res <- data.frame(group_id = unique(x$group_id), attempt_start = mw_start) %>%
    mutate(attempt_end = attempt_start + nest_cycle - 1)

  # Number of visits in initial window
  mw_res$n_visits <- x %>%
    filter(between(reldate, mw_res$attempt_start, mw_res$attempt_end)) %>%
    pull(n_visits) %>%
    sum()

  # Check to see if window can be slid at all
  if (mw_res$attempt_end > max_reldate) {

    # Set attempt_end to the final day
    mw_res <- mw_res %>%
      mutate(attempt_end = max_reldate)
    #Return the result
    return(mw_res)

  } else {

    # Slide the window
    while (max(mw_res$attempt_end) < max_reldate){
      # Prepare new row
      mw_newrow <- last(mw_res) %>%
        mutate(attempt_start = attempt_start + 1,
               attempt_end = attempt_end + 1,
               n_visits = NA)
      # Calculate the number of visits
      mw_newrow$n_visits <- x %>%
        filter(between(reldate, mw_newrow$attempt_start, mw_newrow$attempt_end)) %>%
        pull(n_visits) %>%
        sum()
      # Combine results
      mw_res <- rbind(mw_res, mw_newrow)
    } #End while()

    # Now pick the window with the maximum number of visits
    mw_res <- mw_res %>%
      filter(n_visits == max(n_visits)) %>%
      slice(1)

    #Return result
    return(mw_res)
  }
}


#' Calculate revisitation patterns
#'
#' \code{revisit_stats} calculates patterns of revisitation at candidate nests.
#'
#' @details This is a wrapper function that calls \code{visit_rle},
#' \code{rle_to_consec}, and \code{attempt_limits}.
#'
#' For each candidate nest, the function computes the first and last day when
#' the location was visited, the total number of visits, the number of days in
#' which it was visited, the percent of days with a visit, the attendance on
#' the day with the most visits (percent locations at the nest over the total
#' number of fixes on that day), the longest series of consecutive days
#' visited, and the estimated start and end dates of the nesting attempt.
#'
#' On days when no visit was recorded, two cases are possible: either the nest
#' was truly not visited, or visits were missed. On days with few fixes, there
#' is a higher chance of missing a visit given that it happened. Missed visit
#' detections can interrupt an otherwise continuos strike of days visited.
#' To counteract possible issues due to missed visit detections, the user can
#' define \code{min_d_fix} to set a minimum number of fixes that have to be
#' available in a day with no visits for that day to be retained when counting
#' consecutive days visited. If a day with no visits and fewer fixes than
#' \code{min_d_fix} interrupts a sequence of consecutive days visited, it
#' does not get considered and the sequence gets counted as uninterrupted.
#'
#' @param sub \code{data.frame}. Subset of movement data corresponding to
#' candidate nests
#' @param sea_start Integer (if Julian day) or date in which the nesting season
#' starts
#' @param sea_end Integer (if Julian day) or date in which the nesting season
#' ends
#' @param min_d_fix Integer. Minimum number of fixes in a day for that day to
#' be counted as not visited if a visit was not observed
#' @return Returns \code{data.frame} with revisitation statistics for each
#' candidate nest.
#' @export
revisit_stats <- function(sub,
                          sea_start,
                          sea_end,
                          min_d_fix,
                          min_consec,
                          nest_cycle){

  # Sequence of all the possible days in the season
  all_days <- 0:max(sub$reldate)

  # Initialize output
  out <- data.frame(group_id = sort(unique(sub$group_id)))

  # Number of fixes per day
  daily_fixes <- sub %>%
    group_by(reldate) %>%
    summarize(n_fixes = n()) %>%
    select(reldate, n_fixes) %>%
    arrange(reldate)

  # Day of first and last visit
  first_vis <- sub %>%
    group_by(group_id) %>%
    summarize(first_date=as_date(min(date)), first_reldate=min(reldate))
  last_vis <- sub %>%
    group_by(group_id) %>%
    summarize(last_date=as_date(max(date)), last_reldate=max(reldate))

  # Join to output
  out <- out %>%
    left_join(first_vis, by = "group_id") %>%
    left_join(last_vis, by = "group_id")

  # Total number of visits
  tot_visits <- sub %>%
    group_by(group_id) %>%
    summarize(tot_vis = n())

  # Number of days visited
  days_visited <- sub %>%
    group_by(group_id) %>%
    summarize(days_vis = n_distinct(reldate))

  #Join to output
  out <- out %>%
    left_join(tot_visits, by = "group_id") %>%
    left_join(days_visited, by = "group_id")

  # Percent days visited between first and last
  out <- out %>%
    mutate(perc_days_vis =
             round(days_vis*100/(last_reldate - first_reldate + 1), 2))

  # Count daily visits
  daily_visits <- sub %>%
    group_by(group_id, reldate) %>%
    summarize(n_visits = n())

  # Create data.frame of the range of days a group_id appears
  group_id_range <- data.frame(group_id = rep(out$group_id, each=length(all_days)),
                               reldate = rep(0:max(sub$reldate), length(out$group_id))
  ) %>%
    left_join(first_vis, by = "group_id") %>%
    left_join(last_vis, by = "group_id") %>%
    filter(between(reldate, first_reldate, last_reldate)) %>%
    select(group_id, reldate)

  # Combine with daily_visits
  daily_visits <- group_id_range %>%
    left_join(daily_visits, by = c("group_id", "reldate")) %>%
    mutate(n_visits = case_when(
      is.na(n_visits) ~ 0L,
      TRUE ~ n_visits
    )) %>%
    left_join(daily_fixes, by = "reldate") %>%
    mutate(n_fixes = case_when(
      is.na(n_fixes) ~ 0L,
      TRUE ~ n_fixes
    ))

  # Find day with most visits
  top_day_visits <- daily_visits %>%
    group_by(group_id) %>%
    filter(n_visits == max(n_visits)) %>%
    mutate(percent_vis = round(n_visits*100/n_fixes, 2)) %>%
    summarize(perc_top_vis = max(percent_vis))

  # Join with output
  out <- out %>%
    left_join(top_day_visits, by = "group_id")

  # For calculating consecutive visits, drop any days without the
  # minimum number of required fixes where a visit is not recorded
  filtered_dv <- daily_visits %>%
    filter(n_visits > 0 | n_fixes >= min_d_fix) %>%
    mutate(visited = n_visits > 0)

  # For each group_id, calculate the maximum number of consecutive days with a visit
  # Split into list by group_id
  filtered_list <- split(filtered_dv, filtered_dv$group_id)
  # lapply() custom function 'visit_rle()'
  filtered_rle <- lapply(filtered_list, visit_rle)
  # lapply() custom function 'rle_to_consec()'
  max_days_list <- lapply(filtered_rle, rle_to_consec)
  # Combine results in data.frame
  consec_days <- data.frame(group_id = as.integer(names(max_days_list)),
                            consec_days = unlist(max_days_list))

  # Join with output
  out <- out %>%
    left_join(consec_days, by = "group_id")

  # Compute start and end dates of nesting attempt
  start_end <- lapply(filtered_list, attempt_limits,
                      min_consec = min_consec,
                      nest_cycle = nest_cycle) %>%
    bind_rows()

  # Join with output
  out <- out %>%
    left_join(start_end, by = "group_id")

  # Return output
  return(out)
}


#' Handle overlapping attempts
#'
#' \code{choose_overlapping} selects top candidate nesting attempt among those
#' that are temporally overlapping.
#'
#' @details Within the function \code{nest_finder}, \code{choose_overlapping}
#' is used when \code{discard_overlapping = TRUE}.
#'
#' If the list of nest candidates includes temporally overlapping nesting
#' attempts, only the candidate with the most visits is kept and the others
#' get discarded. This is based on the rationale that an individual cannot
#' simultaneously nest at more than one location. The location that is
#' visited the most is assumed to be the most likely true nest.
#'
#' @param attempts \code{data.frame} of revisitation patterns of candidate
#' nests
#' @return Returns \code{data.frame} of revisitation patterns filtered to
#' only include non-temporally overlapping candidate nests
#' @export
choose_overlapping <- function(attempts) {

  # Initialize field to mark attempts to keep
  discard_df <- data.frame(loc_id = attempts$loc_id, keep = NA)

  while(nrow(discard_df) > 0) {

    discard_df$keep[1] <- TRUE

    current <- discard_df$loc_id[1]
    current_start <- attempts %>%
      filter(loc_id == current) %>%
      pull(attempt_start)
    current_end <- attempts %>%
      filter(loc_id == current) %>%
      pull(attempt_end)

    toss <- attempts %>%
      filter((between(attempt_start, current_start, current_end) |
                between(attempt_end, current_start, current_end)) &
               loc_id != current) %>%
      pull(loc_id)

    attempts <- attempts %>%
      filter(!(loc_id %in% toss))

    discard_df <- discard_df %>%
      mutate(keep = case_when(
        loc_id %in% toss ~ FALSE,
        TRUE ~ keep
      )) %>%
      filter(is.na(keep))
  }

  return(attempts)

}
