#' Conversion of Julian day to date
#'
#' \code{julian_to_date} returns a date corresponding to Julian day and year
#' in input.
#'
#' @param jd Integer. Julian day to convert.
#' @param yr Integer. Year the Julian date refers to.
#' @return Returns a date in ymd format.
#' @examples
#' julian_to_date(1, 2000)
#' # Returns January 1st of 2000
#'
#' # Test what happens with leap years
#'
#' julian_to_date(100, 1991)
#' # 1991 was not a leap year; Julian day 100 is April 10th
#'
#' julian_to_date(100, 1992)
#' # 1992 was a leap year; Julian day 100 is April 9th
#' @export
julian_to_date <- function(jd, yr) {
  # Start with a dummy date
  dummy <- lubridate::ymd("1990-01-01")
  # Update the year
  lubridate::year(dummy) <- yr
  # Update the Julian day
  lubridate::yday(dummy) <- jd
  # Return the result
  return(dummy)
}

#' Process dates and subset data within nesting season
#'
#' \code{date_handler} finds the ymd starting and ending dates of the nesting
#' season based on the dates given in input (as Julian day or date), and
#' returns the subset of data that falls within the range. If inputting dates,
#' the year can be a dummy year and the function will update it to the correct
#' year for the current burst.
#' @param dat \code{data.frame} of movement data for a single burst. Needs to include
#' burst, date, long, lat
#' @param sea_start Integer (if Julian day) or date in which the nesting season
#' starts
#' @param sea_end Integer (if Julian day) or date in which the nesting season
#' ends
#' @return Returns subset of data comprised within the nesting season
#'
#' @export
date_handler <- function(dat, sea_start, sea_end) {

  # Determine the year
  if (sea_start > sea_end){
    # Then the year will change between start and end
    s_year <- min(year(dat$date))
    e_year <- s_year + 1
    # If this is the case, BUT the animal had no locations until > January 1st,
    # then this will return the wrong year -- handling this below
  } else {
    # The start and end year are the same
    s_year <- unique(year(dat$date))
    e_year <- s_year
  }

  # Set dummy dates
  start_dummy <- end_dummy <- ymd("1990-01-01")

  #Update the years of the dummy dates
  year(start_dummy) <- s_year
  year(end_dummy) <- e_year

  # Handle dat type of sea_start (either numeric or date)
  if (is.numeric(sea_start)) {

    # Overwrite month and day
    start_dummy <- julian_to_date(sea_start, s_year)

  } else {

    # Overwrite month
    month(start_dummy) <- month(sea_start)

    # Overwrite day
    day(start_dummy) <- day(sea_start)

  }

  # Handle dat type of sea_end (either numeric or date)
  if (is.numeric(sea_end)) {

    # Overwrite month and day
    end_dummy <- julian_to_date(sea_end, e_year)

  } else {

    # Overwrite month
    month(end_dummy) <- month(sea_end)

    # Overwrite day
    day(end_dummy) <- day(sea_end)

  }

  # Now deal with the case where the wrong year was assigned --
  # see comment above
  if(sum(between(dat$date, start_dummy, end_dummy))==0){
    year(start_dummy) <- year(start_dummy) - 1
    year(end_dummy) <- year(end_dummy) -1
  }

  # Assign relative date
  dat$reldate <- floor(as.numeric(difftime(dat$date, start_dummy,
                                           units="days")))

  #Remove any data not within the breeding limits
  dat <- dat %>%
    filter(date >= start_dummy & date <= end_dummy)

  # Return the data
  return(dat)

}
