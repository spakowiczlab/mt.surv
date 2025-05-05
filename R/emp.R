#' Generate empiricial test scripts for batch jobs
#'
#' @param type a character object. Either "TCGA" or "ORIEN"
#' @param path a character object specifying where the scripts and RDS will be save to. Don't add "/" at the end
#' @param taxo.num an integer to select how many taxonomy to be tested on
#' @param loop.num an integer to set number of cycles for bootstrapping
#'
#' @return a R script and a sl script which can then be submitted as a job
#' @export
#'
emp <- function(type,path,taxo.num, loop.num){
  fileOut<-file(file.path(path,"empirical.sl"))
  fileOutr <- file(file.path(path,"empirical.R"))
  writeLines(c("#!/bin/bash",
               paste0("#SBATCH --job-name=empirical_test"),
               "#SBATCH --time=24:00:00",
               "#SBATCH --nodes=1",
               "#SBATCH --mail-type=ALL",
               "#SBATCH --account=PAS1695",
               "",
               "",
               "module load R/3.6.3-gnu9.1",
               "",
               paste0("Rscript ", path, "/empirical.R")
  ), fileOut)

  writeLines(c("library(targets)",
               "library(mt.surv)",
               "library(tidyverse)",
               "tar_load(area)",
               "tar_load(input_TCGA)",
               "tar_load(input_ORIEN)",
               "",
               "one_tax <- function(input_list,taxa){",
               "",
               "  surv_df <- input_list[[2]]",
               "  tax_df <- input_list[[1]]%>%",
               '    rename("d"="domain","k"="kingdom","p"="phylum","c"="class",',
               '           "o"="order","f"="family","g"="genus","s"="species")%>%',
               '    rename("Tax" = substr(taxa,1,1))%>%',
               "    filter(Tax == taxa)%>%",
               "    group_by(ID,Tax)%>%",
               "    summarize(ra = sum(exo.ra, na.rm = T))%>%",
               "    ungroup()%>%",
               '    spread(key = "Tax", value = "ra")%>%',
               "    filter(ID %in% surv_df$ID)",
               "  return(tax_df)",
               "}",
               "",
               "",
               "find_area <- function(one_tax_in, taxa, input_list){",
               "",
               "  temp <- survivalByQuantile(taxa,one_tax_in,input_list[[2]])%>%",
               "    filter(pval < 0.05) %>%",
               "    mutate(sig.dif = log(0.05) - log(pval),",
               '           survival.effect = ifelse(hazard.direction == "<1", "positive", "negative")) %>%',
               "    group_by(survival.effect) %>%",
               "    summarise(sig.area = sum(sig.dif))%>%",
               "    mutate(species = taxa)",
               "  area.out <- as.data.frame(cbind(area = sum(temp$sig.area),Taxa = taxa))",
               "  return(area.out)",
               "}",
               "",
               "",
               "  emp <- function(input_list, taxa){",
               "  area.list <- list()",
               "  tmp <- one_tax(input_list, taxa)",
               "  area <- find_area(tmp, taxa, input_list)",
               "  area.list[['original']] <- area",
               "",
               paste0("  for (n in seq(1,",loop.num,",1)){"),
               "    tmp <- one_tax(input_list, taxa)",
               "    set.seed(n*100)",
               "    tmp[[2]] <-sample(tmp[[2]], replace = FALSE)",
               "    area <- find_area(tmp, taxa,input_list)",
               "    area.list[[as.character(n+1)]] <- area",
               "  }",
               '  out <- bind_rows(area.list, .id = "seeds")',
               "  return(out)",
               "}",
               "",
               "T.select_tax <- area %>%",
               '  filter(grepl("T_tresh.RData", type))%>%',
               "  select(species)%>%",
               '  filter(!grepl("unclassified", species))%>%',
               paste0("  dplyr::slice(1:",taxo.num, ")"),
               "",
               "O.select_tax <- area %>%",
               '  filter(grepl("O_tresh.RData", type))%>%',
               "  select(species)%>%",
               '  filter(!grepl("unclassified", species))%>%',
               paste0("  dplyr::slice(1:",taxo.num, ")"),
               "",
               # T.output <- lapply(T.select_tax$species, function(x) emp(input_TCGA,x))
               # names(T.output) <- T.select_tax

               paste0("output <- lapply(", substr(type,1,1), ".select_tax$species, function(x) emp(input_",type,",x))"),
               paste0("names(output) <- ",substr(type,1,1),".select_tax"),

               #saveRDS(T.output,"/fs/ess/PAS1695/projects/exosarc/data/T.empiri.RData")
               paste0('saveRDS(output, "',path,"/",substr(type,1,1),'.empiri.RData")')),
             fileOutr)
}
