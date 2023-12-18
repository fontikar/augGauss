# This function plots the posterior predictives for each subject overlayed on
# the empirical gradients
# - Optional labels: display the mean of the posterior for each parameter

Get_Posterior_Preds <- function(aug_model_output, nSamp = 50) {

  # Get Post predictions
  get_post_preds  <- function(transformed_data,
                              samples){

    nStim <- transformed_data$nStim
    nSubj <- transformed_data$nSubj
    subjList <- unique(transformed_data$subj)
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
        post_preds$pred[start:end] <- sample(samples[["predR"]][, subj, dim], size = nSamp)
      }
    }

    responses <- transformed_data$responses

    # add responses
    post_preds$response <- NA
    post_preds$response <- rep(responses, each = nSamp)

    # add mean posterior estimates
    label <- rep(NA, nSubj)
    for (i in 1:nSubj) {
      label[i] <- paste0("M: ", round(summary$mean[i], 2),
                         " W-: ", round(summary$mean[nSubj*2 + i], 2),
                         "\n",
                         " W+: ", round(summary$mean[nSubj + i], 2),
                         " H: ", round(summary$mean[nSubj*3 + i]))
    }
    post_preds$label <- rep(label, each = nSamp * nStim)

    # figure layers
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

    # plot gradients alone
    grad_fig <-
      ggplot2::ggplot(post_preds, ggplot2::aes(y = response, x = dim)) +
      grad_layers +
      ggplot2::geom_text(data = post_preds, mapping = ggplot2::aes(label = label, x = cs_loc, y = 120), size = 2.5, colour = "black") +
      ggplot2::facet_wrap(~ subj)


    pred_fig <- grad_fig +
      ggplot2::geom_point(ggplot2::aes(y = pred, x = dim), colour = "#FF00001A", shape = 16)

     fig <- pred_fig +
       ggplot2::geom_text(data = post_preds, mapping = ggplot2::aes(label = label, x = cs_loc, y = 120), size = 2.5, colour = "black")



     patchwork::wrap_plots(grad_fig, fig)
  }

  post_preds <- purrr::map2(.x = aug_model_output$transformed_data,
                            .y = aug_model_output$samples,
                            ~get_post_preds(.x, .y))

}


