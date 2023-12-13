Get_HDIs <- function(aug_model_output,
                     ropeLow = c(-.05, .1, .1, 70),
                     ropeHigh = c(+.05, .2, .2, 80),
                     hdiLim = .95,
                     propPost = 1,
                     save_output = FALSE, path = "output/") {

  # This function calculates the Highest Density Intervals and
  # p(Region of Practical Equivalence) for HDIparams
  # Note:
  # - ROPE parameters must be of length(HDIparams) and specified in the
  # same order as HDIparams
  # - hdiLim sets the limit for the HDI
  # - propPost refers to the proportion of the posterior used to calculate p(direction) and p(ROPE)

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
