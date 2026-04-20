#' Change data frame into wide format and match survival data
#'
#' @param data.long input taxonomy matrix in long format
#' @param surv.dat survival information for the data.long
#' @param taxalevels character vector representing every taxonomy level to be changed into wide format
#' @return a long format of the taxonomy data with matching survival information
#' @examples
#' ## Reproducible example
#' set.seed(123)
#'
#' ## Simulated long-format exogenous relative abundance data
#' data.long <- data.frame(
#'   ID = rep(paste0("S", 1:10), each = 4),
#'   exo.ra = runif(40),
#'   domain  = rep("d__Bacteria", 40),
#'   kingdom = rep("k__Bacteria", 40),
#'   phylum  = rep(c("p__Firmicutes", "p__Proteobacteria"), each = 20),
#'   class   = rep(c("c__Bacilli", "c__Gammaproteobacteria"), each = 20),
#'   order   = rep("o__TestOrder", 40),
#'   family  = rep("f__TestFamily", 40),
#'   genus   = rep(c("g__A", "g__B"), times = 20),
#'   species = rep(c("s__A1", "s__B1"), times = 20)
#' )
#'
#' ## Simulated survival metadata
#' surv.dat <- data.frame(
#'   ID = paste0("S", 1:10),
#'   days = runif(10, 500, 3000),
#'   vitalstatus = sample(c(0, 1), 10, replace = TRUE)
#' )
#'
#' ## Convert to wide format across taxonomic levels
#' wide_list <- ToWide(
#'   data.long = data.long,
#'   surv.dat  = surv.dat
#' )
#'
#' ## Inspect output
#' names(wide_list)
#' wide_list$phylum
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
