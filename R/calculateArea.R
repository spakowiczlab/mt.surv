#' Quantify taxonomy significance using multi-threshold results
#'
#' @description This function calculates the distance between alpha (default 0.05) and
#' p-values below the significance threshold as an indicator of area under
#' the curve.
#' @param taxlev a character object specifying which subject survival analysis
#' is applying to.
#' @param threshold.mat output from function survivalByQuantile. It includes information about
#' survival under various threshold
#' @param alpha a numeric value representing the statistically significant p-value
#' @return A data frame containing the calculated area values for the specified taxonomy level.
#' @examples
#' # Create mock threshold matrix for two taxonomy levels
#' threshold.mat <- list(
#'   Phylum = data.frame(
#'     pval             = c(0.01, 0.04, 0.10, 0.02, 0.50),
#'     hazard.direction = c("<1", "<1", ">1", ">1", "<1")
#'   ),
#'   Family = data.frame(
#'     pval             = c(0.03, 0.20, 0.001, 0.04, 0.08),
#'     hazard.direction = c(">1", "<1",  "<1",  ">1", ">1")
#'   )
#' )
#'
#' # Calculate area for Phylum level at default alpha = 0.05
#' result <- calculateArea(taxlev = "Phylum", threshold.mat = threshold.mat)
#'
#' # Calculate area for Family level with stricter alpha
#' result_strict <- calculateArea(
#'   taxlev    = "Family",
#'   threshold.mat = threshold.mat,
#'   alpha     = 0.01
#' )
#' @export
#'
calculateArea <- function(taxlev,threshold.mat, alpha = 0.05){
  output <- threshold.mat[[taxlev]]%>%
    dplyr::filter(pval < alpha) %>%
    dplyr::mutate(sig.dif = log(alpha) - log(pval),
           survival.effect = ifelse(hazard.direction == "<1", "positive", "negative")) %>%
    dplyr::group_by(survival.effect) %>%
    dplyr::summarise(sig.area = sum(sig.dif))%>%
    dplyr::mutate(species = taxlev)

  return(output)
}
