survival_plot <- function(modified_input, taxlev, title.input){

  surv <- modified_input[[3]]
  tax.mat <- modified_input[[2]]
  tresh <- modified_input[[1]]

  surv$Taxa <- tax.mat[[taxlev]]

  tre <- tresh[[taxlev]] %>%
    arrange(pval) %>%
    filter(!pval<pval[1]) %>%
    select(percentile)

  cut <- quantile(surv$Taxa, tre[1,])

  tmp <- gsub("[a-z]__(.*)","\\1",taxlev)

  surv <- surv %>%
    mutate(group = ifelse(Taxa < cut, "< threshold", "> threshold"))

  temp <- survfit(Surv(days, vitalstatus) ~ group, surv)

  p.temp <- ggsurvplot(temp,
                       data = surv,
                       pval = TRUE,
                       title = paste(tmp," (",title.input,")",sep = ""),
                       pval.coord = c(3500, 0.9))

  # p.temp$plot <- p.temp$plot +
  #   theme(plot.title = element_text(size=10))
  # theme(axis.text.y = element_blank(),
  #       axis.text.x = element_blank(),
  #       panel.background = element_rect(fill='transparent'),
  #       plot.background = element_rect(fill='transparent', color=NA),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank())+
  # ggtitle(paste(taxlev," (",modified_input,")",sep = ""))

  return(p.temp)
}
