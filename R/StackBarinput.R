#' Title
#'
#' @param TCGA.list a list object containing survival and taxonomy information for TCGA
#' @param ORIEN.list a list object containing survival and taxonomy information for ORIEN
#'
#' @return a dataframe that can be used to make stacked bar plot
#' @export
#'
#' @examples
StackBarinput <- function(TCGA.list, ORIEN.list){
  T_tax <- TCGA.list[[1]]%>%
    select(ID,grep("p__", colnames(.)),-grep("k__", colnames(.)))%>%
    inner_join(TCGA.list[[2]])%>%
    gather(key = phylum, value = exo.ra, -colnames(TCGA.list[[2]]))%>%
    filter(!phylum == "p__Chordata")%>%
    mutate(type = "TCGA",
           exo.ra = as.numeric(exo.ra),
           diagnosis = ifelse(diagnosis=="Dedifferentiated liposarcoma", "DDLPS",
                              ifelse(diagnosis == "Leiomyosarcoma, NOS", "LMS","other")))%>%
    group_by(ID,diagnosis)%>%
    mutate(ra = exo.ra/sum(exo.ra))%>%
    ungroup()%>%
    select(-vitalstatus,-days,-exo.ra)

  O_tax <- ORIEN.list[[1]]%>%
    select(ID,grep("p__", colnames(.)),-grep("k__", colnames(.)))%>%
    inner_join(ORIEN.list[[2]])%>%
    gather(key = phylum, value = exo.ra, -colnames(ORIEN.list[[2]]))%>%
    filter(!phylum == "p__Chordata")%>%
    mutate(type = "ORIEN",
           exo.ra = as.numeric(exo.ra),
           diagnosis = ifelse(grepl("Dedifferentiated liposarcoma",diagnosis),"DDLPS",
                              ifelse(grepl("Leiomyosarcoma, NOS",diagnosis),"LMS","other")))%>%
    group_by(ID,diagnosis)%>%
    mutate(ra = exo.ra/sum(exo.ra))%>%
    ungroup()%>%
    select(-AgeCollect,-AvatarKey,-AgeAtLastContact,-`Primary/Met`,-vitalstatus,-days,-exo.ra)

  #select top 7 representative phylum
  top_phylumT <- T_tax %>%
    group_by(phylum)%>%
    summarise(sum.ra = sum(ra))%>%
    slice_max(order_by = sum.ra, n=7)

  top_phylumO <- O_tax %>%
    group_by(phylum)%>%
    summarise(sum.ra = sum(ra))%>%
    slice_max(order_by = sum.ra, n=7)

  #create plot df
  plot <- rbind(T_tax, O_tax)%>%
    mutate(phylum = ifelse(type == "TCGA", phylum,
                           ifelse(phylum %in% top_phylumO$phylum, phylum, "other")),
           phylum = ifelse(type == "ORIEN", phylum,
                           ifelse(phylum %in% top_phylumT$phylum, phylum, "other")),
           tt = paste(type, diagnosis, sep = "\n"),
           phylum = gsub("p__(.*)","\\1", phylum))

  #create ID levels based on proteobacteria, and legend levels
  levels <- plot %>%
    filter(grepl("Proteo", phylum))%>%
    arrange(ra)

  plot$ID <- factor(plot$ID, c(as.character(levels$ID)))

  return(plot)
}
