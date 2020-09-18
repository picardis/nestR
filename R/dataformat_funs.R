#' Check input data format
#'
#' \code{check_input} checks that the data input in \code{find_nests} is in
#' the correct format.
#'
#' @details The function checks that the input data includes burst, date-time,
#' and lat/long coordinates.
#' @param gps_data \code{data.frame} of GPS data
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
#' @details The history of nest revisitation in the `visits` data frame
#' in output from \code{find_nests} gets formatted as a matrix indicating,
#' for each day, the number of GPS points at the nest. This is the `visits`
#' matrix that \code{format_attempts} will output. Concurrently, another
#' matrix is created, `fixes`, indicating the number of GPS points available
#' on each day.
#' @param nest_info Output of \code{find_nests}
#' @param nest_cycle Duration of nesting cycle
#' @return A \code{list} with two matrices: `fixes`, a matrix of GPS fixes
#' available on each day of the attempt; and `visits`, a matrix of nest
#' visits on each day of the attempt.
#' @export
format_attempts <- function(nest_info,
                            nest_cycle) {

  # Create unique attempt identifier
  attempts <- nest_info$nests %>%
    as.data.frame() %>%
    dplyr::mutate(attempt_id = paste0(burst, "_", loc_id))

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
      dplyr::filter(burst == att$burst)

    # Cut between attempt start and end of nesting cycle
    visits <- visits %>%
      dplyr::filter(dplyr::between(date,
                     att$attempt_start,
                     att$attempt_start + nest_cycle))

    # Count daily fixes within attempt
    fix <- visits %>%
      dplyr::group_by(date = lubridate::as_date(date)) %>%
      dplyr::tally()

    # Count daily visits within attempt
    vis <- visits %>%
      dplyr::filter(loc_id == att$loc_id) %>%
      dplyr::group_by(date = lubridate::as_date(date)) %>%
      dplyr::tally()

    # Initialize visit history
    history <- data.frame(
      date = lubridate::as_date(
        att$attempt_start:(att$attempt_start + nest_cycle - 1)))

    # Join n of fixes and visits
    history <- dplyr::left_join(history, fix, by = "date")
    history <- dplyr::left_join(history, vis, by = "date")
    names(history) <- c("date", "fix", "vis")

    # Replace NAs with zeroes
    history <- history %>%
      dplyr::mutate(fix = case_when(
        is.na(fix) ~ as.integer(0),
        TRUE ~ fix
      )) %>%
      dplyr::mutate(vis = case_when(
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
