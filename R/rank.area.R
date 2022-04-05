#' Calarea
#' @description This function calculate the distence between 0.05 and p-value under 0.05 as an
#' indicator for area under the curve
#' @param taxlev a character object specifying which subject survival analysis
#' is applying to.
#' @param ptable.df output from function survivalByQuantile. It includes information about
#' survival under various threshold
#' @param alpha a numeric value representing the statistically significant p-value
#'
#' @return a numeric value representing area for each taxlev is stored in a dataframe
#' @export
#'
#' @examples
calarea <- function(taxlev,ptable.df, alpha = 0.05){
  input <- ptable.df[[taxlev]]
  output <- input %>%
    filter(pval < alpha) %>%
    mutate(sig.dif = log(alpha) - log(pval),
           survival.effect = ifelse(hazard.direction == "<1", "positive", "negative")) %>%
    group_by(survival.effect) %>%
    summarise(sig.area = sum(sig.dif))%>%
    mutate(species = taxlev)

  return(output)
}
