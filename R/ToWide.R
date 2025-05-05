#' Change data frame into wide format and match survival data
#'
#' @param data.long input taxonomy matrix in long format
#' @param surv.dat survival information for the data.long
#' @param taxalevels character vector representing every taxonomy level to be changed into wide format
#'
#' @return a long format of the taxonomy data with matching survival information
#' @export
#'

ToWide <- function(data.long,surv.dat,
                    taxalevels = c("domain", "kingdom", "phylum", "class",
                                   "order", "family", "genus", "species")){
  exoRAtowide <- function(data.long,surv.dat, taxlev){
    tmp <- data.long %>%
      dplyr::select(ID,exo.ra)
    tmp$Taxa <- data.long[[taxlev]]
    tmp.wide <- tmp %>%
      dplyr::group_by(ID,Taxa)%>%
      dplyr::summarize(ra = sum(exo.ra, na.rm = T))%>%
      dplyr::ungroup()%>%
      tidyr::spread(key = "Taxa", value = "ra")%>%
      dplyr::filter(ID %in% surv.dat$ID)
    tmp.wide[is.na(tmp.wide)] <- 0
    return(tmp.wide)
  }
  w.ls <- lapply(taxalevels, function(x) exoRAtowide(data.long,surv.dat, x))
  names(w.ls) <- taxalevels

  #w.df <- purrr::reduce(w.ls, function(x,y) left_join(x,y))
  return(w.ls)
}
