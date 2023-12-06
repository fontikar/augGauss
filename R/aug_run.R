#' @export

aug_fit_model <- function(transformed_data, ...) {

  # Determine number of groups
  numGroups = length(transformed_data)

  # Set up lists for loop to populate
  mcmc_out <- vector("list", numGroups)
  names(mcmc_out) <- names(transformed_data)

  samples <- vector("list", numGroups)
  names(samples) <- names(transformed_data)

  # Run the model
  for (i in 1:numGroups) {

    # Fit models for each group
    mcmc_out[[i]] <- Run_Model(transformed_data[[i]], ...)
    samples[[i]] <- mcmc_out[[i]]$samples

    print(i)

  }

  out <- list(mcmc_out, samples)
  names(out) <- c("mcmc_out", "samples")
  return(out)
}

#' @export

Run_Model <- function(transformed_data, ...) {

  augG_params <- c("M", "SDPlus", "SDMinus", "height", "noise", "predR", "log_lik",
                   "M_group", "SDPlus_group", "SDMinus_group", "height_group")

  # This function runs the model in stan

  stanfit <- rstan::sampling(object = stanmodels$aug_gaus,
                             data = transformed_data,
                             pars = augG_params,
                             algorithm = "NUTS",
                             cores = parallel::detectCores(),
                             ...)

  # save diagnostics
  diag <- rstan::get_sampler_params(stanfit, inc_warmup = FALSE)

  # save samples
  samples <- rstan::extract(stanfit)

  # # save summary file
  summary <- rstan::summary(stanfit, probs = c(0.025, 0.50, 0.975))$summary
  # write.csv(summary, file = paste0(file_name_root, modelName,"-", groupName, "-summary.csv"), row.names = TRUE)
  #
  # waic and loo measures
  loglik <- loo::extract_log_lik(stanfit)
  waic <- loo::waic(loglik)
  loo <- loo::loo(loglik)
  # write_csv(as.data.frame(t(waic$estimates)), paste0(file_name_root, modelName, "-", groupName, "-waic.csv"))
  # write_csv(as.data.frame(t(loo$estimates)), paste0(file_name_root, modelName, "-", groupName, "-loo.csv"))
  #
  # output
  out <- list(stanfit, diag, samples, summary, waic, loo)
  names(out) <- c("stanfit", "diag", "samples", "summary", "waic", "loo")

  return(out)
}
