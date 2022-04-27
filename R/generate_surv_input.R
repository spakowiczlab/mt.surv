#' Title
#'
#' @param cancer.subtype a character used to select data from input list, tresh.list, area
#' @param input.list a list containing survival and taxonomy information
#' @param tresh.list a list containing threshold information
#' @param area a list containing area information derived from threshold data. It's used
#' to select taxonomy level with top area
#' @param tax_num a numeric value specifyingthe amount of taxonomy level to be selected
#'
#' @return a list conatining survival data, taxonomy data, threshold data, and selected taxonomy.
#' The list should be used as an input for plotting survival analysis
#' @export
#'
#' @examples
generate_surv_input <- function(cancer.subtype, input.list, tresh.list, area, tax_num){

  data_from <- substr(names(input.list)[[1]],1,1)

  surv <- input.list[[paste(data_from,"_surv.",subgroup, sep = "")]]

  tax <- input.list[[paste(data_from,"_tax.",subgroup, sep = "")]]

  tax.list <- exoToDF(tax,surv)

  tax.mat <- purrr::reduce(tax.list, function(x,y) left_join(x,y))%>%
    arrange(ID)

  tresh <- tresh.list[[paste(data_from,"_tresh_",subgroup,".RData", sep = "")]]


  tax_out <- area %>%
    filter(grepl(data_from, type),
           grepl(subgroup, type),
           !grepl("unclassified", species))%>%
    select(species)%>%
    slice_head(n=tax_num)

  select_tax <- tax_out$species

  x <- list(tresh,tax.mat,surv, select_tax)
  names(x) <- c("tresh","tax.mat","surv", "select_tax")
  return(x)
}
