# Load Data and Packages
source(here::here('01_code/packages.R'))
load(here::here("00_data/Distmat_filter_time_points.Rdata"))
rm(dist_mat_global_filter_time_points_18_19, 
   dist_mat_global_filter_time_points_20_21)

# Distance matrix
dist_mat_points_18_19_filter <- S_v_18_19_filter_points %>%
  group_by(v_1, v_2) %>%
  dplyr::summarise(dist = mean(points_L), 
                   .groups = "drop")

dist_mat_points_20_21_filter <- S_v_20_21_filter_points %>%
  group_by(v_1, v_2) %>%
  dplyr::summarise(dist = mean(points_L), 
                   .groups = "drop")


#---- Clustering ----
# cluster functions
cluster_fun <- function(dist_mat, method = "complete") {
  dist_mat %<>% 
    pivot_wider(names_from = v_2, 
                values_from = dist, 
                id_cols = v_1) %>%
    dplyr::select(-v_1) %>%
    as.dist()
  
  hclust(dist_mat, method = method)
}

# plot function
dendro_plot_fun <- function(object) {
  title <- deparse(substitute(object))
  object %>%
    ggdendro::ggdendrogram() +
    theme(axis.text.x = element_text(size = 7), 
          plot.title = element_text(hjust = 0.5, size = 15)) +
    ggtitle(title)
}

L_18_19_complete_filter <- cluster_fun(dist_mat = dist_mat_points_18_19_filter)
L_20_21_complete_filter <- cluster_fun(dist_mat = dist_mat_points_20_21_filter)

L_18_19_single_filter <- cluster_fun(dist_mat = dist_mat_points_18_19_filter, method = "single")
L_20_21_single_filter <- cluster_fun(dist_mat = dist_mat_points_20_21_filter, method = "single")

L_18_19_av_filter <- cluster_fun(dist_mat = dist_mat_points_18_19_filter, method = "average")
L_20_21_av_filter <- cluster_fun(dist_mat = dist_mat_points_20_21_filter, method = "average")

L_18_19_ward_filter <- cluster_fun(dist_mat = dist_mat_points_18_19_filter, method = "ward.D2")
L_20_21_ward_filter <- cluster_fun(dist_mat = dist_mat_points_20_21_filter, method = "ward.D2")

#---- Dendrogram ----
plot_all_fun <- function(name, save.dendro = TRUE) {
  var_name <- name %>% 
    paste0("plot_", .)
  dendro <- dendro_plot_fun(get(name)) +
    ggtitle(name)
  assign(var_name, dendro, envir = .GlobalEnv)
  
  if(save.dendro) {
    ggsave(filename = paste0(
      here::here("04_plots/Dendrogram/points_filtered"), "/", var_name, ".pdf"), 
      width = 1215, height = 745, units = "px", 
      dpi = 60)
  }
}

ls() %>% 
  as_tibble() %>%
  dplyr::filter(str_detect(value, "fun", negate = T) & 
                  str_detect(value, "plot", negate = T) &
                  (str_detect(value, "L"))) %>%
  dplyr::pull(value) %>%
  purrr::map(plot_all_fun)


