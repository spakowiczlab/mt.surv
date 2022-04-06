#' Visulize Threshold vs. p-value
#'
#' @param ptable.df output from function survivalByQuantile. It includes information about
#' survival under various threshold
#' @param taxlev a character object specifying which subject survival analysis
#' is applying to.
#'
#' @return A ggplot
#' @export
#'
#' @examples
make_threshold <- function(ptable.df,taxlev){
  tmp <- ggplot(tresh[[taxlev]], aes(x=percentile, y=log(pval)))+
    geom_line(aes(color = hazard.direction, group=1))+
    geom_hline(yintercept = log(0.05))+
    ylab("log(pval)")+
    scale_color_manual(breaks = c(">=1", "<1"), values = c("red", "black")) +
    theme_bw()+
    ggtitle(taxlev)
    # theme(plot.title = element_text(size = 35),
    #       legend.text = element_text(size= 30),
    #       legend.title = element_text(size= 30),
    #       axis.text=element_text(size=25),
    #       axis.title=element_text(size=25))
  return(tmp)
}
