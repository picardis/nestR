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

#' Dummy function for BJS to test git
#'
#' BJS wrote this function just to test git
#'
dummy <- function(){
  print(1 + 1)
}

#' Second dummy function for BJS to test git
#'
#' BJS wrote this function just to test git
#'
dummy2 <- function(){
  print(2 + 2)
}
