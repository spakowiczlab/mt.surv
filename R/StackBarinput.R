#' Generate input objects for stacked bar plots
#'
#' @param TCGA.list a list object containing survival and taxonomy information for TCGA
#' @param ORIEN.list a list object containing survival and taxonomy information for ORIEN
#' @return a dataframe that can be used to make stacked bar plot
#' @examples
#' # Create mock TCGA taxonomic data
#' tcga_tax <- data.frame(
#'   ID = c("S1", "S2", "S3"),
#'   p__Proteobacteria = c(0.4, 0.3, 0.5),
#'   p__Firmicutes     = c(0.2, 0.4, 0.1),
#'   p__Bacteroidetes  = c(0.2, 0.1, 0.2),
#'   p__Actinobacteria = c(0.1, 0.1, 0.1),
#'   p__Chordata       = c(0.1, 0.1, 0.1)
#' )
#'
#' # Create mock TCGA clinical data
#' tcga_clin <- data.frame(
#'   ID          = c("S1", "S2", "S3"),
#'   diagnosis   = c("Dedifferentiated liposarcoma",
#'                   "Leiomyosarcoma, NOS",
#'                   "Other sarcoma"),
#'   vitalstatus = c(1, 0, 1),
#'   days        = c(365, 200, 500)
#' )
#'
#' # Create mock ORIEN taxonomic data
#' orien_tax <- data.frame(
#'   ID = c("P1", "P2", "P3"),
#'   p__Proteobacteria = c(0.5, 0.2, 0.3),
#'   p__Firmicutes     = c(0.1, 0.5, 0.2),
#'   p__Bacteroidetes  = c(0.2, 0.2, 0.3),
#'   p__Actinobacteria = c(0.1, 0.0, 0.1),
#'   p__Chordata       = c(0.1, 0.1, 0.1)
#' )
#'
#' # Create mock ORIEN clinical data
#' orien_clin <- data.frame(
#'   ID                = c("P1", "P2", "P3"),
#'   diagnosis         = c("Dedifferentiated liposarcoma",
#'                         "Leiomyosarcoma, NOS",
#'                         "Other sarcoma"),
#'   AgeCollect        = c(55, 62, 48),
#'   AvatarKey         = c("A1", "A2", "A3"),
#'   AgeAtLastContact  = c(57, 63, 50),
#'   PrimaryMet = c("Primary", "Met", "Primary"),
#'   vitalstatus       = c(1, 0, 1),
#'   days              = c(400, 150, 600)
#' )
#'
#' # Bundle into lists and run
#' TCGA.list  <- list(tcga_tax, tcga_clin)
#' ORIEN.list <- list(orien_tax, orien_clin)
#'
#' result <- StackBarinput(TCGA.list, ORIEN.list)
#' @export

StackBarinput <- function(TCGA.list, ORIEN.list){
  T_tax <- TCGA.list[[1]]%>%
    dplyr::select(ID,grep("p__", colnames(.)),-grep("k__", colnames(.)))%>%
    dplyr::inner_join(TCGA.list[[2]])%>%
    tidyr::gather(key = phylum, value = exo.ra, -colnames(TCGA.list[[2]]))%>%
    dplyr::filter(!phylum == "p__Chordata")%>%
    dplyr::mutate(type = "TCGA",
           exo.ra = as.numeric(exo.ra),
           diagnosis = ifelse(diagnosis=="Dedifferentiated liposarcoma", "DDLPS",
                              ifelse(diagnosis == "Leiomyosarcoma, NOS", "LMS","other")))%>%
    dplyr::group_by(ID,diagnosis)%>%
    dplyr::mutate(ra = exo.ra/sum(exo.ra))%>%
    dplyr::ungroup()%>%
    dplyr::select(-vitalstatus,-days,-exo.ra)

  O_tax <- ORIEN.list[[1]]%>%
    dplyr::select(ID,grep("p__", colnames(.)),-grep("k__", colnames(.)))%>%
    dplyr::inner_join(ORIEN.list[[2]])%>%
    tidyr::gather(key = phylum, value = exo.ra, -colnames(ORIEN.list[[2]]))%>%
    dplyr::filter(!phylum == "p__Chordata")%>%
    dplyr::mutate(type = "ORIEN",
           exo.ra = as.numeric(exo.ra),
           diagnosis = ifelse(grepl("Dedifferentiated liposarcoma",diagnosis),"DDLPS",
                              ifelse(grepl("Leiomyosarcoma, NOS",diagnosis),"LMS","other")))%>%
    dplyr::group_by(ID,diagnosis)%>%
    dplyr::mutate(ra = exo.ra/sum(exo.ra))%>%
    dplyr::ungroup()%>%
    dplyr::select(-AgeCollect,-AvatarKey,-AgeAtLastContact,-`PrimaryMet`,-vitalstatus,-days,-exo.ra)

  #select top 7 representative phylum
  top_phylumT <- T_tax %>%
    dplyr::group_by(phylum)%>%
    dplyr::summarise(sum.ra = sum(ra))%>%
    dplyr::slice_max(order_by = sum.ra, n=7)

  top_phylumO <- O_tax %>%
    dplyr::group_by(phylum)%>%
    dplyr::summarise(sum.ra = sum(ra))%>%
    dplyr::slice_max(order_by = sum.ra, n=7)

  #create plot df
  plot <- rbind(T_tax, O_tax)%>%
    dplyr::mutate(phylum = ifelse(type == "TCGA", phylum,
                           ifelse(phylum %in% top_phylumO$phylum, phylum, "other")),
           phylum = ifelse(type == "ORIEN", phylum,
                           ifelse(phylum %in% top_phylumT$phylum, phylum, "other")),
           tt = paste(type, diagnosis, sep = "\n"),
           phylum = gsub("p__(.*)","\\1", phylum))

  #create ID levels based on proteobacteria, and legend levels
  levels <- plot %>%
    dplyr::filter(grepl("Proteo", phylum))%>%
    dplyr::arrange(ra)

  plot$ID <- factor(plot$ID, c(as.character(levels$ID)))

  return(plot)
}
