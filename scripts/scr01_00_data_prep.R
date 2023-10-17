library(tidyverse)
library(caret)



minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


data_folder <- file.path("data")
results_folder <- file.path("results")


# Bendall (pre arcsinh transformed data)
dat <- readRDS(file.path(data_folder, "bendall_24_clust.rds"))
panel <- read_tsv(file.path(data_folder, "bendall_panel.txt"))

type_markers <- panel %>% dplyr::filter(marker_class == "type") %>%
        dplyr::pull(antigen)


dat <- dplyr::select(dat, all_of(c("cluster", type_markers))) %>%
        dplyr::rename("cell_population" = cluster) %>%
        as_tibble()

cell_pop_bendall <- as.character(unique(dat$cell_population))

# Mini (pre arcsinh transformed data)
dat_mini <- readRDS(file.path(data_folder, "mini_dat_all_meta.rds"))
panel_mini <- read_tsv(file.path(data_folder, "mini_panel.txt"))

type_markers_mini <- panel_mini %>% dplyr::filter(marker_class == "type") %>%
        dplyr::pull(antigen) %>%
        str_replace_all( "-", "_")

dat_mini <- dplyr::select(as_tibble(dat_mini), all_of(c("merging1", type_markers_mini))) %>%
        dplyr::rename("cell_population" = merging1)

cell_pop_mini <- as.character(unique(dat_mini$cell_population))

# Export cell populations
export_cell_pop <- function(input_data, cell_pop, type_marks){
        cell_pop_out <- dplyr::filter(input_data, cell_population == cell_pop) %>%
                dplyr::select(all_of(type_marks))
        return(cell_pop_out)
}

# nk_bendall <- dplyr::filter(dat, cell_population =="NK") %>%
#         dplyr::select(all_of(type_markers))
# write_tsv(nk_bendall, file.path(results_folder, "nk_bendall.tsv"))
# 
# nk_mini <- dplyr::filter(dat_mini, cell_population =="CD56+CD16+NK cells")%>%
#         dplyr::select(all_of(type_markers_mini))
# write_tsv(nk_mini, file.path(results_folder, "nk_mini.tsv"))
#

cd4_bendall <- export_cell_pop(dat, "Naive CD4+ T", type_markers )
write_tsv(cd4_bendall, file.path(results_folder, "cd4_bendall.tsv"))
cd4_mini <- export_cell_pop(dat_mini, "Niave T helper cells", type_markers_mini)
write_tsv(cd4_mini, file.path(results_folder, "cd4_mini.tsv"))

cd8_bendall <- export_cell_pop(dat, "Naive CD8+ T", type_markers )
write_tsv(cd8_bendall, file.path(results_folder, "cd8_bendall.tsv"))
cd8_mini <- export_cell_pop(dat_mini, "Niave cytotoxic T cells", type_markers_mini)
write_tsv(cd8_mini, file.path(results_folder, "cd8_mini.tsv"))

