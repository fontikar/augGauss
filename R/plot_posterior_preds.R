#' Plot posterior predictions for a group
#'
#' @param aug_model_output model output from `aug_fit_model`
#' @param group_name character string of one group
#' @param nSamp ...
#' @param include_preds logical include predictions or not
#' @param labels logical whether to display labels in plot
#' @param save_output logical whether to save output
#' @param path path to save output
#'
#' @export

Plot_Posterior_Preds_by_group <- function(aug_model_output,
                                          group_name,
                                          nSamp = 50,
                                          include_preds = FALSE,
                                          labels = FALSE,
                                          save_output = FALSE,
                                          path = "output/"){

  # Check if group name exists
  if(! group_name %in% names(aug_model_output$mcmc_out))
    rlang::abort(paste(group_name, "does not match levels in grouping variable, check spelling and try again!"))

  # Get group predictions
  group_output <- Get_Posterior_Preds_by_group(aug_model_output, group_name, nSamp)

  cs_loc <- (group_output$nStim-1)/2 # only if CS+ is in the middle of the dimension
  nStim <- group_output$nStim
  post_preds <- group_output$post_preds

  grad_layers <- list(
    ggplot2::geom_line(stat = "identity", linewidth = 1.25),
    ggplot2::labs(title = "", x = "dimension", y = "responding"),
    ggplot2::scale_colour_manual(values = c("black", "red")),
    ggplot2::geom_vline(xintercept = cs_loc, linetype = "dotted", colour = "black"),
    ggplot2::scale_x_continuous(limits = c(1, nStim), breaks = c(1, cs_loc, nStim),
                                labels = c(min(-.5), 0, max(+.5))),
    ggplot2::scale_y_continuous(limits = c(0, 140), breaks = c(0, 50, 100)),
    ggplot2::theme_classic(),
    ggplot2::theme(panel.background = ggplot2::element_rect(colour = "black", linewidth = 0.5, linetype = "solid", fill = NA),
                   legend.position = "none")
  )

  # Only gradients (default)
  grad_fig <-
    ggplot2::ggplot(post_preds, ggplot2::aes(y = .data$response, x = .data$dim)) +
    grad_layers

  # Want predictions
  if(include_preds){
    grad_fig <- grad_fig +
      ggplot2::geom_point(ggplot2::aes(y = .data$pred, x = .data$dim), colour = "#FF00001A", shape = 16)
  }

  # With labels
  if (labels) {
    grad_fig +
      ggplot2::geom_text(data = post_preds, mapping = ggplot2::aes(label = .data$label, x = cs_loc, y = 120), size = 2.5, colour = "black") +
      ggplot2::facet_wrap(~subj, nrow = 3)
  } else {
    p <- grad_fig + ggplot2::facet_wrap(~subj, nrow = 3) # No labels
  }


  if(save_output & include_preds){
    ggplot2::ggsave(file = paste0(path, group_output$group_name, "-postpreds"),
                    device = "png",
                    plot = p,
                    units = "cm")
  }
  if(save_output)
    ggplot2::ggsave(file = paste0(path, group_output$group_name, "-gradients"),
                    device = "png",
                    plot =   p,
                    units = "cm")

  p

}

#' Get posterior predictions for each group
#'
#' @param aug_model_output model output of `aug_fit_model`
#' @param group_nm name of the group
#' @param nSamp ...
#'
#' @keywords internal
Get_Posterior_Preds_by_group <- function(aug_model_output,
                                         group_nm,
                                         nSamp = 50){

  # Extract group output nicely
  group_output <- extract_group_output(aug_model_output, group_nm)

  # Create postpreds
  post_preds <- create_post_preds(group_output, nSamp)

  # Create and add labels
  label <- add_label(group_output)
  post_preds$label <- rep(label, each = nSamp * group_output$nStim)

  group_output$post_preds <- post_preds

  group_output$group_name <- group_nm

  group_output
}



#' Extract output for posterior predictions
#'
#' @param aug_model_output model output of `aug_fit_model`
#' @param group_nm ...
#' @keywords interal

extract_group_output <- function(aug_model_output, group_nm){


  # Get nSubj
  nSubj <- aug_model_output$transformed_data[[group_nm]]$nSubj

  # Get nStim
  nStim<- aug_model_output$transformed_data[[group_nm]]$nStim

  # Get subList
  subList <- unique(aug_model_output$transformed_data[[group_nm]]$subj)

  # Get predR
  predR <- aug_model_output$samples[[group_nm]]$predR

  # Get summary
  summary <- aug_model_output$mcmc_out[[group_nm]]$summary

  # Get responses
  responses <- aug_model_output$transformed_data[[group_nm]]$responses

  # Put it together as a list
  list(nSubj = nSubj,
       nStim = nStim,
       subList = subList,
       predR = predR,
       summary = summary,
       responses = responses)
}

#' Compile posterior predictions from model output
#'
#' @param group_output extracted group output from `extract_group_output`
#' @param nSamp ...
#' @keywords internal

create_post_preds <- function(group_output, nSamp = 50){

  nStim <- group_output$nStim
  nSubj <- group_output$nSubj
  subjList <- unique(group_output$subList)
  cs_loc <- (nStim-1)/2 # only if CS+ is in the middle of the dimension

  post_preds <- matrix(NA, nrow = nSubj*nStim*nSamp, ncol = 4)
  post_preds <- as.data.frame(post_preds)
  colnames(post_preds) <- c("subj", "dim", "samp", "pred")
  post_preds$subj <- rep(subjList, each = nSamp*nStim)
  post_preds$dim <- rep(rep(1:nStim, each = nSamp), times = nSubj)
  post_preds$samp <- rep(1:nSamp, times = nSubj*nStim)

  for (subj in 1:nSubj) {
    for (dim in 1:nStim) {
      start <- (subj-1) * nStim * nSamp+(dim-1) * nSamp + 1
      end <- (subj-1) * nStim * nSamp + (dim-1) * nSamp + nSamp
      post_preds$pred[start:end] <- sample(group_output[["predR"]][, subj, dim], size = nSamp)
    }
  }

  # add responses
  post_preds$response <- NA
  post_preds$response <- rep(group_output$responses, each = nSamp)

  post_preds
}

#' Add label for posterior prediction plots
#'
#' @param group_output  extracted group output from `extract_group_output`
#' @keywords internal

add_label <- function(group_output){
  nSubj <- group_output$nSubj
  summary <- group_output$summary

  # add mean posterior estimates
  label <- rep(NA, nSubj)
  for (i in 1:nSubj) {
    label[i] <- paste0("M: ", round(summary[,1][i], 2),
                       " W-: ", round(summary[,1][nSubj*2 + i], 2),
                       "\n",
                       " W+: ", round(summary[,1][nSubj + i], 2),
                       " H: ", round(summary[,1][nSubj*3 + i]))
  }

  label
}

