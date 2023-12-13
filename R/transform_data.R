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

  # Determine groupNames
  if(is.null(groupNames))
    groupNames <- unique(sim_data$group)

  out <- vector("list", length(groupNames))

  for (i in 1:length(groupNames)) {
    subset_data <- sim_data |>
      dplyr::filter(group == groupNames[i]) |>
      dplyr::arrange(subj, x)
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



