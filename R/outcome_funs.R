#' Set initial known-state values prior to JAGS MCMC
#'
#' This function is used in \code{estimate_outcomes}
#' Used to initialize the latent variable \code{z} in the JAGS MCMC.
#' Sets \code{z} = 1 for all time steps in between the first sighting
#' and the last sighting.
#'
#' @param ch Capture history, i.e., matrix of nest visits
#' @return A matrix of initial states for each nesting attempt
#'
initialize_z <- function(ch) {
  # Initialize state using the "capture history" (in CMR parlance)
  state <- ch

  # Loop through each nest
  for (i in 1:nrow(ch)) {
    # The earliest "sighting" will always be the first day of the attempt
    n1 <- 1

    # The last sighting is the last time the animal was observed at the nest
    n2 <- max(which(ch[i,] > 0))

    # Set all states between first and last to 1
    state[i, n1:n2] <- 1

    # Reset first to NA (because always see them on first day by definition)
    state[i, n1] <- NA
  }

  # Now set any states remaining as 0 to NA so that JAGS will estimate them
  state[state == 0] <- NA

  # Return
  return(state)
}

#' Estimate nesting outcomes
#'
#' \code{estimate_outcomes} fits a Bayesian hierarchical model to the histories
#' of nest revisitation to estimate the outcome of nesting attempts.
#'
#' @details Data can be passed from \code{\link{format_visits}} to the function
#' arguments \code{fixes} and \code{visits}.
#'
#' The function runs a JAGS MCMC with uninformative priors for the estimation of
#' nest survival. Parameters for the MCMC can be specified by the user by passing
#' them as a list to \code{mcmc_params}.
#'
#' The user can choose among four possible models: a null model with constant p
#' and phi; a model where p varies with time; a model where phi varies with time;
#' and a model where both p and phi vary with time.
#'
#' @param fixes A matrix of the number of GPS fixes on each day of the nesting
#' attempt as returned by \code{\link{format_visits}}.
#' @param visits A matrix of the number of nest visits on each day of the nesting
#' attempt as returned by \code{\link{format_visits}}.
#' @param model Type of model to be run. One of "null", "p_time", "phi_time",
#' or "phi_time_p_time".
#' @param mcmc_params List of MCMC parameters. \code{burn_in}, \code{n_chain},
#' \code{thin}, \code{n_adapt}, \code{n_iter}
#'
#' @return A list of \code{mcarray} objects.
#'
#' @export
estimate_outcomes <- function(fixes,
                              visits,
                              model = "null",
                              mcmc_params = list(burn_in = 1000,
                                                 n_chain = 2,
                                                 thin = 5,
                                                 n_adapt = 1000,
                                                 n_iter = 10000)){

  # Select the correct JAGS file for the model
  model_txt <- dplyr::case_when(
    model == "null" ~ "nest_outcome_null.txt",
    model == "phi_time" ~ "nest_outcome_phi_time.txt",
    model == "p_time" ~ "nest_outcome_p_time.txt",
    model == "phi_time_p_time" ~ "nest_outcome_phi_time_p_time.txt"
  )

  # Path to the JAGS file
  jags_file <- file.path(system.file(package = "nestR"), "jags", model_txt)

  # Starting values for survival status
  s1 <- initialize_z(ch = visits)

  # Define JAGS model
  jags <- rjags::jags.model(file = jags_file,
                     data = list("nests" = nrow(visits),
                               "days" = ncol(visits),
                               "gps_fixes" = fixes,
                               "y" = visits),
                     inits = list("z" = s1),
                     n.chain = mcmc_params$n_chain,
                     n.adapt = mcmc_params$n_adapt)

  #Run the burn-in
  stats::update(object = jags, n.iter = mcmc_params$burn_in)

  #Generate posterior samples
  post <- rjags::jags.samples(model = jags,
                       variable.names = c("phi.b0", "phi.b1", "phi", "p.b0", "p.b1", "p", "z"),
                       n.iter = mcmc_params$n_iter,
                       thin = mcmc_params$thin)

  # Add the names to the list 'post'
  post$names <- row.names(fixes)

  # Add the model type
  post$model <- model

  return(post)

}

#' Plot survival over time from MCMC run
#'
#' \code{plot_survival} makes a plot of mean nest survival over time
#' as estimated by \code{estimate_outcomes}.
#'
#' @param mcmc_obj List of \code{mcarrays} output by \code{estimate_outcomes}.
#' @param ci Numeric. Credible interval level.
#'
#' @return A plot of nest survival over time.
#'
#' @export
plot_survival <- function(mcmc_obj, ci = 0.95){

  # Get the detection element from the MCMC list
  phi <- mcmc_obj$phi

  # Calculate the quantiles for the bounds of the credible interval
  lwr <- 0 + (1 - ci)/2
  upr <- 1 - (1 - ci)/2

  # Data frame
  # Mean across all MCMC iterations and chains
  # Credible intervals across all MCMC iterations and chains
  phi_df <- data.frame(phi_mean = apply(phi, 1, mean),
                       phi_lwr = apply(phi, 1, quantile, lwr),
                       phi_upr = apply(phi, 1, quantile, upr))

  # Plot
  p <- ggplot2::ggplot(phi_df,
                  ggplot2::aes(1:length(phi_mean), phi_mean)) +
    ggplot2::ylim(0, 1) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = phi_lwr,
                                      ymax = phi_upr),
                         fill = "#43BF7199") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
          axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
          axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20))) +
    ggplot2::ylab("Daily Survival (phi)") +
    ggplot2::xlab("Days") +
    ggplot2::ggtitle("Survival")

  print(p)
  return(p)

}

#' Plot detection probability over time from MCMC run
#'
#' \code{plot_detection} makes a plot of mean nest detection probability over time
#' as estimated by \code{estimate_outcomes}.
#'
#' @param mcmc_obj List of \code{mcarrays} output by \code{estimate_outcomes}.
#' @param ci Numeric. Credible interval level.
#'
#' @return A plot of nest detection probability over time.
#'
#' @export
plot_detection <- function(mcmc_obj, ci = 0.95){

  # Get the detection element from the MCMC list
  p <- mcmc_obj$p

  # Calculate the quantiles for the bounds of the credible interval
  lwr <- 0 + (1 - ci)/2
  upr <- 1 - (1 - ci)/2

  # Data frame
  # Mean across all MCMC iterations and chains
  # Credible intervals across all MCMC iterations and chains
  p_df <- data.frame(p_mean = apply(p, 1, mean),
                       p_lwr = apply(p, 1, quantile, lwr),
                       p_upr = apply(p, 1, quantile, upr))

  # Plot
  ggplot2::ggplot(p_df,
                  ggplot2::aes(1:length(p_mean), p_mean)) +
    ggplot2::ylim(0, 1) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = p_lwr,
                                      ymax = p_upr),
                         fill = "#41448799") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
                   axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20))) +
    ggplot2::ylab("Daily Detection Probability (p)") +
    ggplot2::xlab("Days") +
    ggplot2::ggtitle("Detection Probability")

}

#' Plot nest survival over time from MCMC run
#'
#' \code{plot_nest_surv} makes a plot of nest survival over time for a chosen
#' nesting attempt as estimated by \code{estimate_outcomes}.
#'
#' @param mcmc_obj List of \code{mcarrays} output by \code{estimate_outcomes}.
#' @param who Integer. Which nesting attempt to plot.
#' @param ci Numeric. Credible interval level.
#'
#' @return A plot of individual nest survival over time.
#'
#' @export
plot_nest_surv <- function(mcmc_obj, who = 1, ci = 0.95){

  # Get the latent survival element from the MCMC list
  z <- mcmc_obj$z

  # Get the individual of interest
  z_ind <- z[who, , ,]

  # Calculate the quantiles for the bounds of the credible interval
  lwr <- 0 + (1 - ci)/2
  upr <- 1 - (1 - ci)/2

  # Data frame
  # Mean across all MCMC iterations and chains
  # Credible intervals across all MCMC iterations and chains
  zind_df <- data.frame(z_mean = apply(z_ind, 1, mean),
                       z_lwr <- apply(z_ind, 1, quantile, lwr),
                       z_upr <- apply(z_ind, 1, quantile, upr))

  # Plot
  ggplot2::ggplot(zind_df,
                  ggplot2::aes(1:length(z_mean), z_mean)) +
    ggplot2::ylim(0, 1) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = z_lwr,
                                      ymax = z_upr),
                         fill = viridis::viridis(n = 1,
                                                 alpha = 0.5,
                                                 begin = 0.8,
                                                 end = 0.8)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
                   axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20))) +
    ggplot2::ylab("Daily Survival (Z)") +
    ggplot2::xlab("Days") +
    ggplot2::ggtitle(paste0("Survival of Nest ", mcmc_obj$names[who]))

}


#' Summary of outcomes from MCMC run
#'
#' \code{summarize_outcomes} returns summary statistics of estimated nesting
#' attempt outcomes.
#'
#' @details The function takes as input a list of \code{mcarrays} output by
#' \code{estimate_outcomes} and returns a list including the following:
#'
#' \itemize{
#'
#' \item mean, lower and upper credible interval values (at the level
#' specified by the user) for both survival and detection probability,
#' where applicable (depending on which model formula was chosen);
#' \item for each attempt, mean, lower and upper credible interval values
#' of the probability of success and of the failure date. If the estimated
#' probability of success is 1, the failure date corresponds to the duration
#' of a complete nesting attempt.
#'
#' }
#'
#' @param mcmc_obj List of \code{mcarrays} output by \code{estimate_outcomes}.
#' @param ci Numeric. Credible interval level.
#'
#' @return A list with (a) the population-level survival and (b) detection
#' parameters and (c) the individual nest fates
#'
#' @export
summarize_outcomes <- function(mcmc_obj, ci = 0.95){

  # Initialize list for output
  out <- list()

  # Calculate the quantiles for the bounds of the credible interval
  lwr <- 0 + (1 - ci)/2
  upr <- 1 - (1 - ci)/2

  ### Population-level survival

  # If the model had time-varying phi, report the slope and intercept
  if (grepl("phi_time", mcmc_obj$model)){

    # Note that these parameters are on logit scale
    out$phi <- data.frame(b0_lwr = apply(mcmc_obj$phi.b0, 1, quantile, lwr),
                          b0_mean = apply(mcmc_obj$phi.b0, 1, mean),
                          b0_upr = apply(mcmc_obj$phi.b0, 1, quantile, upr),
                          b1_lwr = apply(mcmc_obj$phi.b1, 1, quantile, lwr),
                          b1_mean = apply(mcmc_obj$phi.b1, 1, mean),
                          b1_upr = apply(mcmc_obj$phi.b1, 1, quantile, upr))
  } else {

    # Note that these estimates are not on logit scale
    out$phi <- data.frame(lwr = quantile(mcmc_obj$phi, lwr),
                          mean = mean(mcmc_obj$phi),
                          upr = quantile(mcmc_obj$phi, upr))
    row.names(out$phi) <- NULL

  }

  ### Population-level detection

  if (grepl("p_time", mcmc_obj$model)){

    # Note that these parameters are on logit scale
    out$p <- data.frame(b0_lwr = apply(mcmc_obj$p.b0, 1, quantile, lwr),
                          b0_mean = apply(mcmc_obj$p.b0, 1, mean),
                          b0_upr = apply(mcmc_obj$p.b0, 1, quantile, upr),
                          b1_lwr = apply(mcmc_obj$p.b1, 1, quantile, lwr),
                          b1_mean = apply(mcmc_obj$p.b1, 1, mean),
                          b1_upr = apply(mcmc_obj$p.b1, 1, quantile, upr))
  } else {

    # Note that these estimates are not on logit scale
    out$p <- data.frame(lwr = quantile(mcmc_obj$p, lwr),
                        mean = mean(mcmc_obj$p),
                        upr = quantile(mcmc_obj$p, upr))
    row.names(out$p) <- NULL

  }

  ### Individual burst outcomes

  # Get the latent survival variable
  z <- mcmc_obj$z

  # Get the burst names
  bursts <- mcmc_obj$names

  # Create data.frame of results
  indiv <- data.frame(burst = bursts,
                      pr_succ_lwr = NA,
                      pr_succ_mean = NA,
                      pr_succ_upr = NA,
                      last_day_lwr = NA,
                      last_day_mean = NA,
                      last_day_upr = NA)

  # Probability burst was a successful nest (survived to last day)
  # Get the last day for each burst + iteration + chain
  last_day <- apply(z, c(1, 3, 4), getElement, ncol(z))
  # Get values for each burst
  indiv$pr_succ_lwr <- apply(last_day, 1, quantile, lwr)
  indiv$pr_succ_mean <- apply(last_day, 1, mean)
  indiv$pr_succ_upr <- apply(last_day, 1, quantile, upr)

  # Latest day that a nest survived to
  latest_day <- apply(z, c(1, 3, 4), sum)
  # Get values for each burst
  indiv$last_day_lwr <- apply(latest_day, 1, quantile, lwr)
  indiv$last_day_mean <- apply(latest_day, 1, mean)
  indiv$last_day_upr <- apply(latest_day, 1, quantile, upr)

  # Add to output list
  out$outcomes <- indiv

  return(out)
}
