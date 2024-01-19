#' Transform tabular data in Stan-friendly list data
#'
#' @param sim_data output from `simulate_data`
#'
#' @param dimVals dimension values, vector length of 5
#' @param groupNames optional vector of group names
#'
#' @export

transform_data <- function(sim_data, dimVals = seq(-.5, +.5, .05), groupNames = NULL) {

  # This function reads data in long format and prepares the data list for stan
  # Notes
  # - Responses should be labelled as "y", subject ID as "subj", group names
  # labelled as "group" (and should match groupNames)
  # - The dimVals argument does not have to match the labelling of the "x" column
  # - The CS+ should have a dimVal of 0
  # - Choose appropriate breaks and labels for the x-axis
  # See demo_data.csv for an example

  # When groupNames is supplied, check if # of groupNames matches levels in data
  if(! is.null(groupNames)) {
  if(! length(groupNames) == unique(sim_data$group) |> length())
    stop("Length of `groupNames` supplied does not match number of groups in data")
  }

  # Check if 0 is at min or max of dimVals
  if(dimVals[1] == 0 | dimVals[length(dimVals)] == 0){
    stop('`dimVals` cannot start or end with 0, please rescale your data. See vignette("rescaling data")')
  }

  # If no groupNames are supplied, identify time from data
  if(is.null(groupNames)){
    groupNames <- unique(sim_data$group)
    message("`groupNames` not supplied, taking unique values from `group` variable")
  }



  out <- vector("list", length(groupNames))

  for (i in 1:length(groupNames)) {
    subset_data <- sim_data |>
      dplyr::filter(.data$group == groupNames[i]) |>
      dplyr::arrange(.data$subj, .data$x)
    out[[i]] <- list(subj = subset_data[["subj"]],
                     responses = matrix(subset_data[["y"]], ncol = length(dimVals),
                                        byrow = TRUE),
                     nSubj = dplyr::n_distinct(subset_data[["subj"]]),
                     nStim = length(dimVals),
                     xs = dimVals)
  }
  names(out) <- groupNames
  return(out)
}



