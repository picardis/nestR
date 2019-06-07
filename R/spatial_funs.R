#' Calculate distance matrix between points in the data
#'
#' \code{dist_mat} calculates pairwise distances between all points in the
#' data.
#' @details Distances are calculated using the function
#' \code{\link[geosphere]{distGeo}}. Takes advantage of \code{data.table}
#' for a fast implementation. Adapted from a post on \href{
#' https://stackoverflow.com/questions/36817423/how-to-efficiently-calculate-
#' distance-between-pair-of-coordinates-using-data-tab}{StackOverflow}.
#' @param dat \code{data.frame} of movement data for a single burst. Needs to
#' include burst, date, long, lat
#' @return Returns \code{data.table} with distance matrix.
dist_mat <- function(dat){

  # Arrange distance matrix
  dmat <- data.frame(orig_id = rep(dat$loc_id, nrow(dat))) %>%
    left_join(dplyr::select(dat, loc_id, orig_lon = long, orig_lat = lat),
              by = c("orig_id" = "loc_id")) %>%
    mutate(dest_id = rep(dat$loc_id, each=nrow(dat))) %>%
    left_join(dplyr::select(dat, loc_id, dest_lon = long, dest_lat = lat),
              by = c("dest_id" = "loc_id"))

  # Convert to data.table
  data.table::setDT(dmat)

  # Use := operator (data.table) to calculate geographic distance
  dmat[ , dist := geosphere::distGeo(matrix(c(orig_lon, orig_lat), ncol = 2),
                                     matrix(c(dest_lon, dest_lat), ncol = 2))]

  #Return distance matrix
  return(dmat)
}

#' Get top candidate nests from possible competitors
#'
#' \code{get_candidates} uses a distance matrix returned by
#' \code{\link{dist_mat}} and a user-defined buffer to select candidate nest
#' sites.
#' @details Due to both movement and GPS error, recorded points around
#' recurrently visited locations are expected to be scattered around the true
#' revisited location. The buffer is meant to account for this scattering,
#' by grouping points that fall within the buffer distance.
#'
#' When grouping, several points peripheral to the true revisited location
#' may compete in grouping points around them. We term these 'competing
#' points'. If the buffers of two points do not overlap, those points are
#' not competing. Based on the assumption that the true revisited location
#' is the one that incorporates the most points within its buffer,
#' \code{get_candidates} compares the number of points that fall within the
#' buffers of competing points and selects the one that includes the most.
#'
#' A top candidate is selected for each cluster of competing points, i.e., one
#' representative for each cluster around a revisited location. If there are
#' multiple revisited locations with non-competing points, independent top
#' candidates are all returned.
#'
#' To speed up calculations, the user can define \code{min_pts} as the minimum
#' number of points that need to fall within the buffer for a point to be
#' considered as a potential candidate. This discards isolated points from
#' consideration as revisited locations.
#' @param dm Distance matrix returned by \code{\link{dist_mat}}
#' @param buffer Buffer distance (in meters) used to group points
#' @param min_pts Minimum number of points within the buffer for a point to be
#' retained. Defaults to 2
#' @return Returns \code{data.frame} relating original location identifiers
#' (\code{loc_id}) to the identifier of the corresponding candidate nest
#' (\code{group_id}).
get_candidates <- function(dm, buffer, min_pts = 2){

  # Pre-process distance matrix
  dm <- dm %>%
    filter(dist <= buffer) %>% # Keep just measurements less than the buffer
    mutate(group_id = as.integer(NA)) # Initialize the group_id field which
    # will be used to label points falling within the same buffer

  # Remove "isolated" points, as defined by parameter 'min_pts'
  iso <- dm %>%
    group_by(orig_id) %>%
    tally() %>%
    filter(n < min_pts) %>%
    pull(orig_id)
  dm <- dm %>%
    mutate(group_id =
             case_when(
               orig_id %in% iso ~ orig_id,
               TRUE ~ as.integer(NA)
             ))

  # Loop while any 'group_id' is NA
  # Select the single point with the most others inside its buffer
  # Assign all of the points inside that buffer the group_id for that point
  # Repeat for any unassigned
  while (any(is.na(dm$group_id))) {

    # Find the point with the most other points inside its buffer
    top <- dm %>%
      filter(is.na(group_id)) %>%
      group_by(orig_id) %>%
      tally() %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(orig_id)

    # Find all the other points inside that buffer
    others <- dm %>%
      filter(orig_id == top) %>%
      pull(dest_id)

    # Assign the group of 'top' to all origins in 'others'
    dm <- dm %>%
      mutate(group_id = case_when(
        !is.na(group_id) ~ group_id,
        orig_id %in% others ~ top,
        TRUE ~ as.integer(NA)
      ))
  } # End while()

  # Create data.frame with loc_id and the group_id it belongs in
  cands <- dm %>%
    dplyr::select(loc_id = orig_id, group_id) %>%
    unique() %>%
    arrange(loc_id)

  # Return the result
  return(cands)
}

#' Summarize number of points within candidate buffers
#'
#' \code{candidate_summary} counts the number of points grouped within each
#' candidate buffer and arranges them in descending order.
#' @param cands \code{data.frame} of associations between points and nest
#' candidates returned by \code{\link{get_candidates}}
#' @return Returns \code{tibble} counting number of points within each
#' candidate buffer.
candidate_summary <- function(cands){

    cands <- cands %>%
    group_by(group_id) %>%
    tally() %>%
    arrange(desc(n))

  # Return result
  return(cands)
}
