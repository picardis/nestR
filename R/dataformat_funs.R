#' Check input data format
#'
#' \code{check_input} checks that the data input in \code{find_nests} is in
#' the correct format.
#'
#' @details The function checks that the input data includes burst, date-time,
#' and lat/long coordinates.
#' @param gps_data \code{data.frame} of GPS data
#' @export
check_input <- function(gps_data) {

  # Check that all the fields are there
  if (any(!c(exists("burst", where = gps_data),
             exists("date", where = gps_data),
             exists("long", where = gps_data),
             exists("lat", where = gps_data)))) {

    stop("Data does not include required fields or column names are different.
         Check that gps_data includes burst, date, long, and lat.")

  }

  # Check that coordinates are in the correct projection
  if (any(abs(gps_data$long) > 180) |
      any(abs(gps_data$lat) > 90)) {

    stop("Coordinates exceed the range of long/lat.
         Check that coordinates are in long/lat format.")
  }

}

#' Format data for nesting outcome estimation
#'
#' \code{format_attempts} takes as input the output of \code{find_nests} and
#' formats it for input in \code{estimate_outcomes}.
#'
#' @details bla
#' @param nest_info Output of \code{find_nests}
#' @param nest_cycle Duration of nesting cycle
#' @export
format_attempts <- function(nest_info,
                            nest_cycle) {

  # Create unique attempt identifier
  attempts <- nest_info$nests %>%
    mutate(attempt_id = paste0(burst, "_", loc_id))

  # Initialize output

  # Matrix 1: number of fixes per day
  mat_fix <- matrix(NA, nrow = nrow(attempts), ncol = nest_cycle)

  # Matrix 2: number of visits per day
  mat_vis <- matrix(NA, nrow = nrow(attempts), ncol = nest_cycle)

  # Set up rownames
  rownames(mat_fix) <- rownames(mat_vis) <- 1:nrow(mat_fix)

  # Loop over attempts
  for (i in 1:nrow(attempts)) {

    # Select current attempt
    att <- attempts[i,]

    # Data on nest revisits
    visits <- nest_info$visits %>%
      filter(burst == att$burst)

    # Cut between attempt start and end of nesting cycle
    visits <- visits %>%
      filter(between(date,
                     att$attempt_start,
                     att$attempt_start + nest_cycle))

    # Count daily fixes within attempt
    fix <- visits %>%
      group_by(date = as_date(date)) %>%
      tally()

    # Count daily visits within attempt
    vis <- visits %>%
      filter(loc_id == att$loc_id) %>%
      group_by(date = as_date(date)) %>%
      tally()

    # Initialize visit history
    history <- data.frame(
      date = as_date(att$attempt_start:(att$attempt_start + nest_cycle - 1)))

    # Join n of fixes and visits
    history <- left_join(history, fix, by = "date")
    history <- left_join(history, vis, by = "date")
    names(history) <- c("date", "fix", "vis")

    # Replace NAs with zeroes
    history <- history %>%
      mutate(fix = case_when(
        is.na(fix) ~ as.integer(0),
        TRUE ~ fix
      )) %>%
      mutate(vis = case_when(
        is.na(vis) ~ as.integer(0),
        TRUE ~ vis
      ))

    # Plug values into matrices
    mat_fix[i,] <- history$fix
    mat_vis[i,] <- history$vis

    # Name rows with attempt ID
    rownames(mat_fix)[i] <- att$attempt_id
    rownames(mat_vis)[i] <- att$attempt_id

  }

  mats <- list(mat_fix, mat_vis)
  names(mats) <- c("fixes", "visits")

  return(mats)

}
