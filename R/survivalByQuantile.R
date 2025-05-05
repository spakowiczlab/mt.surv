#' Compute cox model p-value under various threshold
#'
#' @details The threshold used in this function is to categorize data into two groups
#'
#' @param input.var a character object specifying which subject survival analysis
#' is applying to.
#' @param input.tax data frame containing information about an object to be tested.
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

survivalByQuantile <- function(input.var, input.tax, surv.dat, percentiles = seq(.01, .99, 0.01)) {

  aa <- function(input.var, input.tax, surv.dat, percent) {
    surv.dat$input.var <- input.tax[[input.var]]
    cut <- quantile(surv.dat$input.var, percent)

    surv.dat$tmp <- dplyr::case_when(surv.dat$input.var < cut ~ 0,
                              surv.dat$input.var >= cut ~ 1)
    surv.dat$tmp <- factor(surv.dat$tmp)
    surv.dat$tmp <- forcats::fct_relevel(surv.dat$tmp, "0")

    surv_model <- survival::coxph(survival::Surv(days, vitalstatus) ~ tmp, surv.dat)

    tidy_out <- broom::tidy(surv_model, exponentiate = TRUE, conf.int = TRUE)

    dat_temp <- tidy_out %>%
      dplyr::filter(term == "tmp1") %>%  # keep the comparison group (vs. reference "0")
      dplyr::mutate(
        percentile = percent,
        cutoff.value = cut,
        hazard.direction = ifelse(estimate >= 1, ">=1", "<1")
      ) %>%
      dplyr::select(
        hazard.ratio = estimate,
        low.bound = conf.low,
        upper.bound = conf.high,
        percentile,
        cutoff.value,
        pval = p.value,
        hazard.direction
      )

    return(dat_temp)
  }

  # Run over all percentiles
  results <- purrr::map_dfr(percentiles, ~aa(input.var, input.tax, surv.dat, .x))
  return(results)
}
