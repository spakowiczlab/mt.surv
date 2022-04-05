#' Title
#'
#' @param taxlev
#' @param ptable.df
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
rank.area <- function(taxlev,ptable.df, alpha = 0.05){
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
