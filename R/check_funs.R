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
