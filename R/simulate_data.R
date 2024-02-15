#' Simulate augmented Gaussian gradients
#'
#' @description
#' This function simulates augmented Gaussian gradients for groups of subjects.
#' @param nSubj an integer specifying the number of subjects per group.
#' @param nGroups an integer specifying the number of experimental groups.
#' @param dimVals a vector of numbers specifying the dimension values. Must range between -0.5 and +0.5. Defaults to seq(-.5, +.5, .05).
#' @param M a vector of length nGroups specifying the Mean parameter for each group.
#' @param H a vector of length nGroups specifying the Height parameter for each group.
#' @param WM a vector of length nGroups specifying the Width- parameter for each group.
#' @param WP a vector of length nGroups specifying the Width+ parameter for each group.
#' @param Noise a number specifying the amount of noise (SD of Gaussian) added to simulate the data.
#' @param save_data a logical variable indicating whether to save data as .csv.
#' @param path path to save .csv output.
#'
#' @return tibble of simulated data
#' @export
#' @author Jessica Lee
#'
#' @examples
#' simulate_data
simulate_data <- function(nSubj, nGroups,
                          dimVals = seq(-.5, +.5, .05),
                          M = c(0, 0, 0.1),
                          H = c(80, 75, 70),
                          WM = c(0.2, 0.2, 0.2),
                          WP = c(0.4, 0.4, 0.4),
                          Noise = 2,
                          save_data = FALSE,
                          path = "output/"){

  df <- list()

  for (g in 1:nGroups) {
  # browser()
    # generate individual parameters
    m <- stats::rnorm(nSubj, M[g], .1)
    h = stats::rnorm(nSubj, H[g], 1)
    wm = abs(stats::rnorm(nSubj, WM[g], .1))
    wp = abs(stats::rnorm(nSubj, WP[g], .1))
    noise = rep(Noise, nSubj)

    # generate individual gradients
    simData <- matrix(nrow = nSubj, ncol = length(dimVals))

    for (i in 1:nSubj) {
      gausLeft <- h[i] * exp(1)^-(((dimVals-m[i])^2) / (2 * wm[i]^2)) +
        stats::rnorm(length(dimVals), 0, noise)
      gausRight <- h[i] * exp(1)^-(((dimVals-m[i])^2) / (2 * wp[i]^2)) +
        stats::rnorm(length(dimVals), 0, noise)
      mIdx <- which.min(abs(dimVals-m[i]))
      if (mIdx == 1) {
        simData[i,] <- c(gausLeft[1], gausRight[(mIdx+1):length(dimVals)])
      } else {
        simData[i,] <- c(gausLeft[1:(mIdx-1)], gausRight[mIdx:length(dimVals)])
      }
    }
    subj <- rep(1:nSubj + (g-1)*nSubj, each = length(dimVals))
    x <- rep(dimVals, times = nSubj)
    y <- as.vector(t(simData))
    y[y > 100] <- 100
    y[y < 0] <- 0
    group <- rep(paste0("group", g), nSubj)
    df[[g]] <- data.frame(cbind(subj, group, x, y))
  }

  out <- do.call("rbind", df)
  out <- out |>
    dplyr::mutate(x = as.numeric(as.character(x)),
                  y = as.numeric(as.character(y)),
                  subj = as.numeric(as.character(subj))) |>
    dplyr::tibble()

  # Check if path exists, if not, create
  if(!file.exists(path))
    dir.create(path)

  # Save output data
  if(save_data == TRUE)
  readr::write_csv(out, paste0(path,Sys.Date(),"_simulated_data",".csv"))
  else return(out)
}


