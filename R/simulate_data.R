#' Simulate augmented Gaussian gradients
#'
#' @description
#' This function simulates augmented Gaussian gradients for groups of subjects
#' @param nSubj number of subjects
#' @param nGroups number of experimental groups
#' @param dimVals dimension values
#' @param M ...
#' @param H ...
#' @param WM ...
#' @param WP ...
#' @param Noise ...
#' @param save_data logical, save data as .csv
#' @param path path to save .csv output
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

    # generate individual parameters
    m <- rnorm(nSubj, M[g], .1)
    h = rnorm(nSubj, H[g], 1)
    wm = abs(rnorm(nSubj, WM[g], .1))
    wp = abs(rnorm(nSubj, WP[g], .1))
    noise = rep(Noise, nSubj)

    # generate individual gradients
    simData <- matrix(nrow = nSubj, ncol = length(dimVals))

    for (i in 1:nSubj) {
      gausLeft <- h[i] * exp(1)^-(((dimVals-m[i])^2) / (2 * wm[i]^2)) +
        rnorm(length(dimVals), 0, noise)
      gausRight <- h[i] * exp(1)^-(((dimVals-m[i])^2) / (2 * wp[i]^2)) +
        rnorm(length(dimVals), 0, noise)
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
  out <- out %>%
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


plot_simulated_data <- function(sim_data, dimVals = seq(-.5, +.5, .05)){
  layers <- list(
    ggplot2::geom_line(size = 1.5),
    ggplot2::geom_point(shape = rep(c(21, 24, 22), each=length(dimVals)), fill = "white", size = 2),
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", colour = "grey"),
    ggplot2::scale_x_continuous(limits = c(min(dimVals), max(dimVals)), breaks = dimVals, labels = c(min(dimVals), rep("", (length(dimVals) - 3)/2), "CS+", rep("", (length(dimVals) - 3)/2), max(dimVals))),
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)),
    ggplot2::scale_colour_manual(values = c("#301A4B", "#6DB1BF", "#D4775E")),
    ggplot2::labs(x = "stimulus", y = "mean response"),
    ggplot2::theme_classic(),
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(shape = c(21, 24, 22))))
  )

  gradients <- sim_data %>%
    dplyr::arrange(subj, group, x) %>%
    dplyr::mutate(group = forcats::fct_relevel(group, unique(group))) %>%
    dplyr::group_by(group, x) %>%
    dplyr::summarise(y = mean(y))

  fig <- ggplot2::ggplot(gradients, ggplot2::aes(x = x, y = y, group = group, colour = group,
                                                 shape = group)) + layers

  return(fig)
}


