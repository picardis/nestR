#' Sets initial known-state values prior to JAGS MCMC
#'
#' This function is used in \code{estimate_outcomes}
#' Used to intialize the latent variable \code{z} in the JAGS MCMC.
#' Sets \code{z} = 1 for all time steps in between the first sighting
#' and the last sighting.
#'
#' @export
initialize_z <- function(ch){
  # Initialize state using the "capture history" (in CMR parlance)
  state <- ch

  # Loop through each nest
  for (i in 1:nrow(ch)){
    # The earliest "sighting" will always be the first day of the attempt
    n1 <- 1

    # The last sighting is the last time the animal was observed at the nest
    n2 <- max(which(ch[i,]>0))

    # Set all states between first and last to 1
    state[i, n1:n2] <- 1

    # Reset first to NA (because always see them on first day by definition)
    state[i, n1] <- NA
  }

  # Now set any states remaining as 0 to NA so that JAGS will estimate them
  state[state==0] <- NA

  # Return
  return(state)
}

#' Estimates nesting outcomes
#'
#' \code{estimate_outcomes} does blah blah blah...
#' @details Data can be passed from \code{\link{format_visits}}... blah blah blah
#'
#' @param fixes A matrix of the number of GPS fixes on each day of the nesting attempt
#'   as returned by \code{\link{format_visits}}.
#' @param visits A matrix of the number of nest visits on each day of the nesting attempt
#'   as returned by \code{\link{format_visits}}.
#'
#' @export
estimate_outcomes <- function(fixes,
                              visits,
                              model = "null",
                              mcmc_params = list(burn_in = 1000,
                                                 n_chain = 2,
                                                 thin = 5,
                                                 n_adapt = 1000,
                                                 n_iter = 10000)
){
  # Select the correct JAGS file for the model
  model_txt <- case_when(
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
  jags <- jags.model(file = jags_file,
                     data = list("nests" = nrow(visits),
                               "days" = ncol(visits),
                               "gps_fixes" = fixes,
                               "y" = visits),
                     inits = list("z" = s1),
                     n.chain = mcmc_params$n_chain,
                     n.adapt = mcmc_params$n_adapt)

  #Run the burn-in
  update(object = jags, n.iter = mcmc_params$burn_in)

  #Generate posterior samples
  post <- jags.samples(model = jags,
                       variable.names = c("phi.b0", "phi.b1", "phi", "p.b0", "p.b1", "p", "z"),
                       n.iter = mcmc_params$n_iter,
                       thin = mcmc_params$thin)

  return(post)

}

#' Plot survival over time from MCMC run
#'
#' Blah
#'
#' @export
plot_survival <- function(mcmc_obj, ci = 0.95){

  # Get the detection element from the MCMC list
  phi <- mcmc_obj$phi

  # Calculate the quantiles for the bounds of the credible interval
  lwr <- 0 + (1 - ci)/2
  upr <- 1 - (1 - ci)/2

  # Mean across all MCMC iterations and chains
  phi_mean <- apply(phi, 1, mean)
  # Get lower credible interval across all MCMC iterations and chains
  phi_lwr <- apply(phi, 1, quantile, lwr)
  # Get upper credible interval across all MCMC iterations and chains
  phi_upr <- apply(phi, 1, quantile, upr)

  # Plot
  plot(1:length(phi_mean), phi_mean, ylim = c(0,1),
       type = "l", xlab = "Day", ylab = "Daily Survival")
  lines(1:length(phi_mean), phi_lwr, lty=2)
  lines(1:length(phi_mean), phi_upr, lty=2)
}

#' Plot detection over time from MCMC run
#'
#' Blah
#'
#' @export
plot_detection <- function(mcmc_obj, ci = 0.95){

  # Get the detection element from the MCMC list
  p <- mcmc_obj$p

  # Calculate the quantiles for the bounds of the credible interval
  lwr <- 0 + (1 - ci)/2
  upr <- 1 - (1 - ci)/2

  # Mean across all MCMC iterations and chains
  p_mean <- apply(p, 1, mean)
  # Get lower credible interval across all MCMC iterations and chains
  p_lwr <- apply(p, 1, quantile, lwr)
  # Get upper credible interval across all MCMC iterations and chains
  p_upr <- apply(p, 1, quantile, upr)

  # Plot
  plot(1:length(p_mean), p_mean, ylim = c(0,1),
       type = "l", xlab = "Day", ylab = "Detection Probability")
  lines(1:length(p_mean), p_lwr, lty=2)
  lines(1:length(p_mean), p_upr, lty=2)
}

#' Plot latent survival for an individual nest
#'
#' Blah
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

  # Mean across all MCMC iterations and chains
  z_mean <- apply(z_ind, 1, mean)
  # Get lower credible interval across all MCMC iterations and chains
  z_lwr <- apply(z_ind, 1, quantile, lwr)
  # Get upper credible interval across all MCMC iterations and chains
  z_upr <- apply(z_ind, 1, quantile, upr)

  # Plot
  plot(1:length(z_mean), z_mean, ylim = c(0,1),
       type = "l", xlab = "Day", ylab = "Probability Nest Survives")
  lines(1:length(z_mean), z_lwr, lty=2)
  lines(1:length(z_mean), z_upr, lty=2)
}

