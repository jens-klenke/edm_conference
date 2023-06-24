# Load Data and Packages
source(here::here('01_code/packages.R'))
load(here::here("00_data/Distmat_time_points.Rdata"))
rm(dist_mat_global_time_points_18_19, 
   dist_mat_global_time_points_20_21)

# Distance matrix
dist_mat_points_18_19 <- S_v_18_19_points %>%
  group_by(v_1, v_2) %>%
  dplyr::summarise(dist = mean(points_L), 
                   .groups = "drop")

dist_mat_points_20_21 <- S_v_20_21_points %>%
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

L_18_19_complete <- cluster_fun(dist_mat = dist_mat_points_18_19)
L_20_21_complete <- cluster_fun(dist_mat = dist_mat_points_20_21)

L_18_19_single <- cluster_fun(dist_mat = dist_mat_points_18_19, method = "single")
L_20_21_single <- cluster_fun(dist_mat = dist_mat_points_20_21, method = "single")

L_18_19_av <- cluster_fun(dist_mat = dist_mat_points_18_19, method = "average")
L_20_21_av <- cluster_fun(dist_mat = dist_mat_points_20_21, method = "average")

L_18_19_ward <- cluster_fun(dist_mat = dist_mat_points_18_19, method = "ward.D2")
L_20_21_ward <- cluster_fun(dist_mat = dist_mat_points_20_21, method = "ward.D2")

#---- Dendrogram ----
plot_all_fun <- function(name, save.dendro = TRUE) {
  var_name <- name %>% 
    paste0("plot_", .)
  dendro <- dendro_plot_fun(get(name)) +
    ggtitle(name)
  assign(var_name, dendro, envir = .GlobalEnv)
  
  if(save.dendro) {
    ggsave(filename = paste0(
      here::here("04_plots/Dendrogram/points"), "/", var_name, ".pdf"), 
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


