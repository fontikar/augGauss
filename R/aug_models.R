#' Select data configuration template for aug supported model
#'
#' @param model model name character string
#'
#' @return named list that matches Stan model parameters
#' @export
#'
#' @examples
#' aug_model("linear")

aug_model <- function(model=NULL){

  #TODO: Need a mechanism to check model requested in one that is supported by aug

  output <- switch(model,
                   linear = aug_lm(),
                   constant_single = aug_cgs(),
                   gaussian = aug_gauss())

  class(output) <- "aug_object"

  return(output)
}

#' Data configuration template for linear model
#' @keywords internal
#' @noRd

aug_lm <- function(){
  list(X = NULL,
       Y = NULL,
       N = NULL,
       model = "linear")
}

#' Data configuration template for constant growth single species model
#' @keywords internal
#' @noRd

aug_cgs <- function(){
  list(N_obs = NULL,
       N_ind = NULL,
       S_obs = NULL,
       census = NULL,
       census_interval = NULL,
       id_factor = NULL,
       S_0_obs = NULL,
       model = "constant_single")
}


#' Data configuration template for augmented Gaussian model
#' @keywords internal
#' @noRd

aug_gauss <- function(){
  list(nSubj = NULL,
       nStim = NULL,
       S_obs = NULL,
       census = NULL,
       census_interval = NULL,
       id_factor = NULL,
       S_0_obs = NULL,
       model = "aug_gauss")
}
