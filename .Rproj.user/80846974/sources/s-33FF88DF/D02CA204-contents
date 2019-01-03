#' Conversion of Julian day to date.
#'
#' \code{julian_to_date} returns a date corresponding to Julian day and year in input.
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


# Finds the ymd starting and ending dates of the nesting season
# based on the (Julian or not) dates given in input,
# and returns the subset of data that falls within the range
date_handler <- function(dat, sea.start, sea.end) {

  # Determine the year
  if (sea.start > sea.end){
    # Then the year will change between start and end
    s.year <- min(year(dat$date))
    e.year <- s.year + 1
    # If this is the case, BUT the animal had no locations until > January 1st,
    # then this will return the wrong year -- handling this below
  } else {
    # The start and end year are the same
    s.year <- unique(year(dat$date))
    e.year <- s.year
  }

  # Set dummy dates
  start.dummy <- end.dummy <- ymd("1990-01-01")

  #Update the years of the dummy dates
  year(start.dummy) <- s.year
  year(end.dummy) <- e.year


  # Handle dat type of sea.start (either numeric or date)
  if (is.numeric(sea.start)) {

    # Overwrite month and day
    start.dummy <- julian_to_date(sea.start, s.year)

  } else {

    # Overwrite month
    month(start.dummy) <- month(sea.start)

    # Overwrite day
    day(start.dummy) <- day(sea.start)

  }

  # Handle dat type of sea.end (either numeric or date)
  if (is.numeric(sea.end)) {

    # Overwrite month and day
    end.dummy <- julian_to_date(sea.end, e.year)

  } else {

    # Overwrite month
    month(end.dummy) <- month(sea.end)

    # Overwrite day
    day(end.dummy) <- day(sea.end)

  }

  # Now deal with the case where the wrong year was assigned -- see comment above
  if(sum(between(dat$date, start.dummy, end.dummy))==0){
    year(start.dummy) <- year(start.dummy) - 1
    year(end.dummy) <- year(end.dummy) -1
  }

  # Assign relative date
  dat$reldate <- floor(as.numeric(difftime(dat$date, start.dummy, units="days")))

  #Remove any dat not within the breeding limits
  dat <- dat %>%
    filter(date >= start.dummy & date <= end.dummy)

  # Return the dat
  return(dat)

}
