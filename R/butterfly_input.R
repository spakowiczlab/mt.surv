#' Generate input
#'
#' @param area.input
#' @param subtype
#' @param num_micro
#'
#' @return
#' @export
#'
#' @examples
butterfly_input <- function(area.input, subtype, num_micro) {

  data_T <- area.input %>%
    filter(grepl("T", type),
           grepl(subtype, type))%>%
    select(-type)
  data_O <- area.input %>%
    filter(grepl("O", type),
           grepl(subtype, type))%>%
    mutate(area = -1*area)%>%
    select(-type)

  common.tax <- intersect(data_T$species,data_O$species)

  common.area.all <- data_T %>%
    filter(species %in% common.tax)%>%
    rename("T.area" = "area")%>%
    inner_join(data_O, by = "species")%>%
    rename("Taxa" = "species", "O.area" = "area")%>%
    mutate(sum.area = T.area + abs(O.area))%>%
    arrange(sum.area)%>%
    mutate(Taxa=factor(Taxa, levels=Taxa))%>%
    gather("type","area",-c("Taxa","sum.area"))


  common.area.head <- data_T %>%
    rename("T.area" = "area")%>%
    filter(species %in% common.tax)%>%
    inner_join(data_O, by = "species")%>%
    rename("Taxa" = "species", "O.area" = "area")%>%
    mutate(O.area = O.area*-1,
           sum.area = T.area + abs(O.area))%>%
    arrange(sum.area)%>%
    tail(n = num_micro)%>%
    mutate(Taxa=factor(Taxa, levels=Taxa))%>%
    gather("type","area",-c("Taxa","sum.area"))


  common.taxarea.head <- ggplot(common.area.head, aes(x = area, y = Taxa ,fill = type))+
    geom_col(position = "dodge")+
    # theme(axis.text.y=element_blank(),
    #       axis.ticks.y=element_blank())+
    ylab("Taxonomy")+
    xlab("area")

  common.taxarea.all <- ggplot(common.area.all, aes(x = area, y = Taxa ,fill = type))+
    geom_col(position = "stack")+
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    ylab("Taxonomy")+
    xlab("area")

  x <- list(common.taxarea.all,common.taxarea.head)
  names(x) <- c("common.taxarea.all","common.taxarea.head")
  return(x)
}
