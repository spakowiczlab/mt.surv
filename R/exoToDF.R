#' change data frame into wide format
#'
#' @param data input data in long format
#' @param surv survival information for the data
#' @param taxalevels character vector
#'
#' @return
#' @export
#'
#' @examples
exoToDF <- function(data,surv,
                    taxalevels = c("domain", "kingdom", "phylum", "class",
                                   "order", "family", "genus", "species")){
  exoRAtowide <- function(data,surv, taxlev){
    tmp <- data %>%
      select(ID,exo.ra)
    tmp$Taxa <- data[[taxlev]]
    tmp.wide <- tmp %>%
      group_by(ID,Taxa)%>%
      summarize(ra = sum(exo.ra, na.rm = T))%>%
      ungroup()%>%
      spread(key = "Taxa", value = "ra")%>%
      filter(ID %in% surv$ID)
    tmp.wide[is.na(tmp.wide)] <- 0
    return(tmp.wide)
  }
  w.ls <- lapply(taxalevels, function(x) exoRAtowide(data,surv, x))
  names(w.ls) <- taxalevels

  #w.df <- purrr::reduce(w.ls, function(x,y) left_join(x,y))
  return(w.ls)
}
