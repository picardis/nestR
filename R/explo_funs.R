#' Extract nests and non-nests from revisited locations
#'
#' \code{get_explodata} uses known nest locations to extract an equal number of
#' true nests and non-nests from a set of revisited locations for exploration
#' of parameter values.
#'
#' @details If no prior information is available to the user about which
#' parameter values to use to filter nests among revisited locations, some
#' exploration of the data is necessary. The objective of the exploration
#' phase is to identify the set of parameter values that best discriminates
#' between nests and non-nests in the species or population at hand.
#'
#' Our suggested procedure consists in running a first coarse screening of
#' revisited locations, using loose thresholds of behavioral parameters for
#' filtering. For example, using \code{min_consec = 1}, \code{min_top_att = 1},
#' and \code{min_days_att = 1} in \code{find_nests}. In most cases, previous
#' knowledge about the biology of the species should allow the user to
#' already provide values for \code{sea_start}, \code{sea_end}, and
#' \code{nest_cycle}. The output of this first screening is a set of revisited
#' locations at the \code{buffer} distance of choice, which should include
#' some nests as well as other repeatedly visited locations that are not nests.
#' Comparing the values of behavioral parameters at nests versus non-nests can
#' inform the choice of parameter values for later analysis.
#'
#' This procedure requires knowledge of true nest locations for at least a
#' subset of the data. Given some data on known nest locations and the
#' coarse-screening output of \code{find_nests}, the function
#' \code{get_explodata} identifies true nests and an equal number of non-nests
#' to compare them to.
#'
#' Comparing behavioral parameter values at nests versus non-nests will
#' allow the user to find the set of parameter values that best discriminates
#' between them. The resulting set of parameters can then be used to find nests
#' in new data or in a subset of data for which no prior information on nests
#' is available.
#'
#' The user can pass data on known nests as either coordinates or location IDs.
#' Ideally, prior and independent information on nest locations
#' is available for a subset of the data. In this case, we recommend
#' that coordinates are passed to the function argument \code{known_coords}.
#' When coordinates of true nests are not known a-priori but the user is able
#' to visually inspect revisited locations and identify those that are true
#' nests (for example because they fall within known colonies), providing
#' location IDs for true nests in \code{known_ids} is an alternative option.
#'
#' When passing \code{known_coords}, the user is required to also specify a
#' value for \code{buffer}. Because of GPS error, the coordinates of the point
#' representing the true nest in \code{candidate_nests} might not exactly match
#' those of the known nest location. If coordinates of true nests are provided
#' rather than location IDs, the function selects the true nest among the set
#' of candidates by choosing the candidate with the most visits among those
#' that fall within a \code{buffer} distance from the known nest location.
#' We recommend using for this argument the same value used for argument
#' \code{buffer} in \code{find_nests}.
#'
#' @param candidate_nests \code{data.frame} of candidate nests output by
#' \code{find_nests}
#' @param known_coords \code{data.frame} of coordinates for known nests. Needs
#' to include burst, long, lat.
#' @param known_ids \code{data.frame} of location IDs for known nests. Needs
#' to include burst and loc_id.
#' @param buffer Integer. Buffer distance (in meters) used to select true
#' nest location among candidates when \code{known_coords} is provided.
#' @param pick_overlapping Logical. If \code{TRUE} (default), the non-nest is
#' picked among those whose time range overlaps with the true nesting attempt.
#' @return A \code{data.frame} including an equal number of true nests and
#' non-nests and their revisitation parameters.
#'
#' @export
get_explodata <- function(candidate_nests,
                          known_coords,
                          known_ids,
                          buffer,
                          pick_overlapping = TRUE) {

  # Case 1: known_coords and buffer are provided
  if (!missing(known_coords) && !missing(buffer)) {

    # Keep only bursts for which we have a known nest
    candidate_nests <- candidate_nests %>%
      filter(burst %in% known_coords$burst)

    # Create empty lists to store results
    nests <- list()
    non_nests <- list()

    # Loop through each burst
    for (i in unique(candidate_nests$burst)) {

      # Subset burst and order by total visits
      sub <- candidate_nests %>%
        filter(burst == i) %>%
        arrange(desc(tot_vis))

      # Make into matrix for distance computation
      cands_matrix <- sub %>%
        select(long, lat) %>%
        as.matrix(ncol = 2)

      # Select known nest location
      known <- known_coords %>%
        filter(burst == i) %>%
        select(long, lat)

      # Compute distance of each candidate from real nest
      sub$dist_from_known <- distGeo(cands_matrix, known)

      # Subset candidates within buffer distance of known nest location
      true_nest <- sub %>%
        filter(dist_from_known <= buffer) %>%
        slice(1)

      # The real nest is the top (most visited) among those
      nests[[i]] <- true_nest

      # Subset the rest
      rest <- sub %>%
        filter(loc_id != nests[[i]]$loc_id)

      # If the true nest was found, select a non-nest too
      if (nrow(nests[[i]]) > 0) {

        # If pick_overlapping == TRUE, pick non-nest among those that
        # temporally overlap with the true one
        if (pick_overlapping == TRUE) {

          # Get start and end of true attempt
          true_start <- true_nest %>%
            pull(attempt_start)
          true_end <- true_nest %>%
            pull(attempt_end)

          # The non-nest is the top visited among those that temporally
          # overlap with the nest
          non_nest <- rest %>%
            filter((between(attempt_start, true_start, true_end) |
                      between(attempt_end, true_start, true_end)) &
                     tot_vis == max(rest$tot_vis)) %>%
            slice(1)

        } else { # Otherwise, if pick_overlapping == FALSE, the non-nest
          # is simply the top visited among the other points

          # The non-nest is the top visited among the rest
          non_nests[[i]] <- rest %>%
            filter(tot_vis == max(rest$tot_vis)) %>%
            slice(1)

        }

      }
      }

    } else if (!missing(known_ids)) {
    # Case 2: known_ids are provided

      for (i in unique(candidate_nests$burst)) {

        # Subset burst and order by total visits
        sub <- candidate_nests %>%
          filter(burst == i) %>%
          arrange(desc(tot_vis))

        # Select known nest location
        known <- known_ids %>%
          filter(burst == i) %>%
          select(loc_id)

        # Select real nest based on location ID
        true_nest <- sub %>%
          filter(loc_id == known)

        # Store in list
        nests[[i]] <- true_nest

        # Subset the rest
        rest <- sub %>%
          filter(loc_id != known)

        # If the true nest was found, select a non-nest too
        if (nrow(nests[[i]]) > 0) {

          # If pick_overlapping == TRUE, pick non-nest among those that
          # temporally overlap with the true one
          if (pick_overlapping == TRUE) {

            # Get start and end of true attempt
            true_start <- true_nest %>%
              pull(attempt_start)
            true_end <- true_nest %>%
              pull(attempt_end)

            # The non-nest is the top visited among those that temporally
            # overlap with the nest
            non_nest <- rest %>%
              filter((between(attempt_start, true_start, true_end) |
                        between(attempt_end, true_start, true_end)) &
                       tot_vis == max(rest$tot_vis)) %>%
              slice(1)

          } else { # Otherwise, if pick_overlapping == FALSE, the non-nest
            # is simply the top visited among the other points

            # The non-nest is the top visited among the rest
            non_nests[[i]] <- rest %>%
              filter(tot_vis == max(rest$tot_vis)) %>%
              slice(1)

          }

      }
      }
    }

  nests_df <- do.call("rbind", nests)
  non_nests_df <- do.call("rbind", non_nests)

  nests_df$nest <- "yes"
  non_nests_df$nest <- "no"

  explodata <- rbind(nests_df, non_nests_df)

  return(explodata)

}



