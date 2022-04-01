#' Multi-threshold analysis
#'
#' @param input.var
#' @param input.mat
#' @param surv.dat
#' @param percentiles
#'
#' @return
#' @export
#'
#' @examples
survivalByQuantile <- function(input.var, input.mat, surv.dat, percentiles = seq(.01,.99,0.01)){

  surv.dat$input.var <- input.mat[[input.var]]
  ptable <- list()
  for(i in percentiles){
    cut <- quantile(surv.dat$input.var, i)
    surv.dat$tmp <- ifelse(surv.dat$input.var < cut, 0, 1)
    ### survival data and Tax data ID has to be the same and in same order (use arrange())
    survtemp <- coxph(Surv(days, vitalstatus) ~ tmp, surv.dat)
    dat_temp <- merge(data.frame(summary(survtemp)[["conf.int"]]),
                      data.frame(summary(survtemp)[["coefficients"]]))
    ptable[[as.character(i)]] <- dat_temp %>%
      mutate(hazard.ratio = exp..coef.,
             low.bound = lower..95,
             upper.bound = upper..95,
             pval = Pr...z..,
             percentile = i,
             cutoff.value = cut) %>%
      select(hazard.ratio, low.bound, upper.bound, percentile, cutoff.value, pval) %>%
      mutate(hazard.direction = ifelse(hazard.ratio >= 1, ">=1", "<1"))

  }
  ptable.df <- bind_rows(ptable)

  return(ptable.df)
}
