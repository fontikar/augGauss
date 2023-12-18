# This function calculates the Highest Density Intervals and
# p(Region of Practical Equivalence) for HDIparams
# Note:
# - ROPE parameters must be of length(HDIparams) and specified in the
# same order as HDIparams
# - hdiLim sets the limit for the HDI
# - propPost refers to the proportion of the posterior used to calculate p(direction) and p(ROPE)

Get_HDIs <- function(aug_model_output,
                     ropeLow = c(-.05, .1, .1, 70),
                     ropeHigh = c(+.05, .2, .2, 80),
                     hdiLim = .95,
                     propPost = 1,
                     save_output = FALSE, path = "output/") {

  # Isolate samples
  samples <- aug_model_output$samples

  # Determine names of HDIparams
  get_hdi_per_group <- function(samples){
    HDIparams <- names(samples)[grep("group", names(samples))]

    # Create dataframe to populate
    temp <- expand.grid(param = HDIparams,
                        hdi_lim = hdiLim,
                        hdi_low = NA,
                        hdi_high = NA,
                        p_dir = NA,
                        prop_rope = NA)

    temp <- temp |>
      dplyr::mutate(rope_low = ropeLow,
                    rope_high = ropeHigh) |>
      dplyr::select(param:hdi_high, rope_low, rope_high, prop_rope)

    for (i in 1:length(HDIparams)) {
      temp$hdi_low[i] <- bayestestR::hdi(as.vector(samples[[HDIparams[i]]]), ci = hdiLim)$CI_low
      temp$hdi_high[i] <- bayestestR::hdi(as.vector(samples[[HDIparams[i]]]), ci = hdiLim)$CI_high
      temp$p_dir[i] <- bayestestR::p_direction(as.vector(samples[[HDIparams[i]]]), ci = propPost)
      temp$prop_rope[i] <- bayestestR::rope(as.vector(samples[[HDIparams[i]]]), ci = propPost,
                                            range = c(ropeLow[i], ropeHigh[i]))$ROPE_Percentage
    }

    return(temp)
  }


  output <- purrr::map(samples,
             ~get_hdi_per_group(.x))

  groupNames <- names(output)

  if(save_output)
    purrr::map2(.x = output,
                .y = groupNames,
               ~write.csv(format(.x, scientific = FALSE),
                          paste0(path, .y, "-hdis.csv"),
                          row.names = FALSE)
    )

  return(output)

}

# This function calculates the Highest Density Intervals and
# p(Region of Practical Equivalence) for HDIparams for differences between
# samples_1 and samples_2
# Note:
# - ROPE parameters must be of length(HDIparams) and specified in the
# same order as HDIparams
# - hdiLim sets the limit for the HDI
# - propPost refers to the proportion of the posterior used to calculate p(direction) and p(ROPE)

Get_HDIs_diff <- function(aug_model_output, comparison = c("group1", "group2"),
                          ropeLowDiffs = c(-.05, -.05, -.05, -2.5), ropeHighDiffs = abs(c(-.05, -.05, -.05, -2.5)),
                          hdiLim = 0.95, propPost = 1,
                          save_output = FALSE, path = "output/") {

  # Isolate all samples
  samples <- aug_model_output$samples

  # Isolate specific group samples
  group_samples <- samples[names(samples) %in% comparison]

  # Set HDI params
  HDIparams <- c("M_group", "SDPlus_group", "SDMinus_group", "height_group")

  diff_m <- as.vector(group_samples[[1]]$M_group) - c(as.vector(group_samples[[2]]$M_group))
  diff_wplus <- as.vector(group_samples[[1]]$SDPlus_group) - c(as.vector(group_samples[[2]]$SDPlus_group))
  diff_wminus <- as.vector(group_samples[[1]]$SDMinus_group) - c(as.vector(group_samples[[2]]$SDMinus_group))
  diff_h <- as.vector(group_samples[[1]]$height_group) - c(as.vector(group_samples[[2]]$height_group))

  sample_diffs <- as.data.frame(cbind(diff_m, diff_wplus, diff_wminus, diff_h))

  colnames(sample_diffs) <- HDIparams

   temp <- data.frame(param = HDIparams,
                     hdi_lim = hdiLim,
                     hdi_low = rep(NA, length(HDIparams)),
                     hdi_high = rep(NA, length(HDIparams)),
                     p_dir = rep(NA, length(HDIparams)),
                     rope_low = ropeLowDiffs,
                     rope_high = ropeHighDiffs,
                     prop_rope = rep(NA, length(HDIparams)))

  for (i in 1:length(HDIparams)) {
    temp$hdi_low[i] <- bayestestR::hdi(as.vector(sample_diffs[[HDIparams[i]]]), ci = hdiLim)$CI_low
    temp$hdi_high[i] <- bayestestR::hdi(as.vector(sample_diffs[[HDIparams[i]]]), ci = hdiLim)$CI_high
    temp$p_dir[i] <- bayestestR::p_direction(as.vector(sample_diffs[[HDIparams[i]]]), ci = propPost)
    temp$prop_rope[i] <- bayestestR::rope(as.vector(sample_diffs[[HDIparams[i]]]), ci = propPost,
                              range = c(temp$rope_low[i], temp$rope_high[i]))$ROPE_Percentage
  }

  if(save_output)
  readr::write_csv(format(temp, scientific = FALSE),
                   paste0(path, comparison[1], "-", comparison[2], "-hdis.csv"))


  return(list(group_sample_diffs = sample_diffs, group_diff_hdi = temp))
}

