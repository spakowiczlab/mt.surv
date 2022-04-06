#' Compute cox model p-value under various threshold
#'
#' @details The threshold used in this function is to categorize data into two groups
#'
#' @param input.var a character object specifying which subject survival analysis
#' is applying to.
#' @param input.mata data frame containing information about an object to be tested.
#' The sample order must agree with surv.dat's sample order
#' @param surv.dat a data frame containing all survival information. The sample
#' order must agree with input.dat's sample order
#' @param percentiles a list of numeric values specifying all of the percentiles
#' to be tested on
#'
#' @return The function will generate a data frame containing hazard ratio,
#' lower bound, upper bound, percentile, cutoff threshold, and p-value. This data
#' frame can then be manipulated for various plots
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
