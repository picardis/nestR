#' Process dates and subset data within nesting season
#'
#' \code{date_handler} finds the ymd starting and ending dates of the nesting
#' season based on the dates given in input (as Julian day or a character
#' string indicating month and day), and returns the subset of
#' data that falls within the range. Inputting month and day is the recommended
#' option.
#' @param dat \code{data.frame} of movement data for a single burst. Needs to include
#' burst, date, long, lat
#' @param sea_start Character string for month and day
#' ("mm-dd") in which the nesting season starts
#' @param sea_end Character string for month and day
#' ("mm-dd") in which the nesting season ends
#' @return Returns subset of data comprised within the nesting season
date_handler <- function(dat, sea_start, sea_end) {

    sea_start <- lubridate::ymd(paste0("1990-", sea_start))
    sea_end <- lubridate::ymd(paste0("1990-", sea_end))

  # Determine the actual year
  if (sea_start > sea_end) {
    # Then the year will change between start and end
    s_year <- min(lubridate::year(dat$date))
    e_year <- s_year + 1
    # If this is the case, BUT the animal had no locations until > January 1st,
    # then this will return the wrong year -- handling this below
  } else {
    # The start and end year are the same
    s_year <- unique(lubridate::year(dat$date))
    e_year <- s_year
  }

  # Set dummy dates
  start_dummy <- end_dummy <- lubridate::ymd("1990-01-01")

  #Update the years of the dummy dates
  lubridate::year(start_dummy) <- s_year
  lubridate::year(end_dummy) <- e_year

  # Overwrite month
  lubridate::month(start_dummy) <- lubridate::month(sea_start)

  # Overwrite day
  lubridate::day(start_dummy) <- lubridate::day(sea_start)

  # Overwrite month
  lubridate::month(end_dummy) <- lubridate::month(sea_end)

  # Overwrite day
  lubridate::day(end_dummy) <- lubridate::day(sea_end)

  # Now deal with the case where the wrong year was assigned --
  # see comment above
  if (sum(dplyr::between(lubridate::as_date(dat$date),
                         start_dummy, end_dummy)) == 0) {
    lubridate::year(start_dummy) <- lubridate::year(start_dummy) - 1
    lubridate::year(end_dummy) <- lubridate::year(end_dummy) -1
  }

  # Assign relative date
  dat$reldate <- floor(as.numeric(difftime(dat$date, start_dummy,
                                           units="days")))

  #Remove any data not within the breeding limits
  dat <- dat %>%
    dplyr::filter(.data$date >= start_dummy & .data$date <= end_dummy)

  # Return the data and the actual season start and end
  return(list(dat = dat,
              actual_start = start_dummy,
              actual_end = end_dummy))

}
