#' Make survival curve
#'
#' @param modified_input the output list from 'generate_surv_input' function
#' @param taxlev an output string object from 'generate_surv_input' specifying a taxonomy level to be plotted
#' @param title.input a string object to speficy the title of the plot
#'
#' @return a ggplot object
#' @export
#'
survival_plot <- function(modified_input, taxlev, title.input){


  surv <- modified_input[[3]]
  tax.mat <- modified_input[[2]]
  tresh <- modified_input[[1]]

  surv$Taxa <- tax.mat[[taxlev]]

  tre <- tresh[[taxlev]] %>%
    dplyr::arrange(pval) %>%
    dplyr::filter(!pval<pval[1]) %>%
    dplyr::select(percentile)

  cut <- stats::quantile(surv$Taxa, tre[1,])

  tmp <- gsub("[a-z]__(.*)","\\1",taxlev)

  surv <- surv %>%
    dplyr::mutate(group = ifelse(Taxa < cut, "< threshold", "> threshold"))

  temp <- survival::survfit(Surv(days, vitalstatus) ~ group, surv)

  p.temp <- ggsurvplot(temp,
                       data = surv,
                       pval = TRUE,
                       title = paste(tmp," (",title.input,")",sep = ""),
                       pval.coord = c(3500, 0.9))

  return(p.temp)
}
