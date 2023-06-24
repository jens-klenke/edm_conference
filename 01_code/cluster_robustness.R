# Load Data and Packages
source('01_code/packages.R')
load(here::here("00_data/Distmat_filter_time_points.Rdata"))
rm(list = setdiff(ls(), 
                  c("S_v_18_19_filter_points", 
                    "S_v_20_21_filter_points")))

# Add up to get a global measure
# all equal weights
dist_mat_global_filter_time_points_18_19_rc <-  S_v_18_19_filter_points %>%   
  pivot_longer(cols = c(points_L, L_1, L_2)) %>%
  group_by(v_1, v_2) %>%
  plyr::arrange(v_1, v_2, name) %>%
  dplyr::summarise(dist_L1 = mean(value[name != "L_2"]), 
                   dist_L2 = mean(value[name != "L_1"]), 
                   .groups = "drop")

dist_mat_global_filter_time_points_20_21_rc <-  S_v_20_21_filter_points %>%   
  pivot_longer(cols = c(points_L, L_1, L_2)) %>%
  group_by(v_1, v_2) %>%
  plyr::arrange(v_1, v_2, name) %>%
  dplyr::summarise(dist_L1 = mean(value[name != "L_2"]), 
                   dist_L2 = mean(value[name != "L_1"]), 
                   .groups = "drop")

save(S_v_18_19_filter_points, S_v_20_21_filter_points,
          dist_mat_global_filter_time_points_18_19_rc, dist_mat_global_filter_time_points_20_21_rc,
          file = here::here("00_data/Distmat_filter_time_points_rc.Rdata"))


#---- Clustering ----
#load(here::here("00_data/Distmat_filter_time_points_rc.Rdata"))

# cluster functions
L1_cluster_fun <- function(dist_mat, method = "complete") {
  dist_mat_L1 <- dist_mat %>% 
    pivot_wider(names_from = v_2, 
                values_from = dist_L1, 
                id_cols = v_1) %>%
    dplyr::select(-v_1) %>%
    as.dist()
  
  hclust(dist_mat_L1, method = method)
}

L2_cluster_fun <- function(dist_mat, method = "complete") {
  dist_mat_L2 <- dist_mat %>% 
    pivot_wider(names_from = v_2, 
                values_from = dist_L2, 
                id_cols = v_1) %>%
    dplyr::select(-v_1) %>%
    as.dist()
  
  hclust(dist_mat_L2, method = method)
}


# L1
L1_18_19_complete_filter_time_points_rc <- L1_cluster_fun(dist_mat = dist_mat_global_filter_time_points_18_19_rc)
L1_20_21_complete_filter_time_points_rc <- L1_cluster_fun(dist_mat = dist_mat_global_filter_time_points_20_21_rc)

L1_18_19_single_filter_time_points_rc <- L1_cluster_fun(dist_mat = dist_mat_global_filter_time_points_18_19_rc, method = "single")
L1_20_21_single_filter_time_points_rc <- L1_cluster_fun(dist_mat = dist_mat_global_filter_time_points_20_21_rc, method = "single")

L1_18_19_av_filter_time_points_rc <- L1_cluster_fun(dist_mat = dist_mat_global_filter_time_points_18_19_rc, method = "average")
L1_20_21_av_filter_points_rc <- L1_cluster_fun(dist_mat = dist_mat_global_filter_time_points_20_21_rc, method = "average")

L1_18_19_ward_filter_time_points_rc <- L1_cluster_fun(dist_mat = dist_mat_global_filter_time_points_18_19_rc, method = "ward.D2")
L1_20_21_ward_filter_time_points_rc <- L1_cluster_fun(dist_mat = dist_mat_global_filter_time_points_20_21_rc, method = "ward.D2")

# L2
L2_18_19_complete_filter_time_points_rc <- L2_cluster_fun(dist_mat = dist_mat_global_filter_time_points_18_19_rc)
L2_20_21_complete_filter_time_points_rc <- L2_cluster_fun(dist_mat = dist_mat_global_filter_time_points_20_21_rc)

L2_18_19_single_filter_time_points_rc <- L2_cluster_fun(dist_mat = dist_mat_global_filter_time_points_18_19_rc, method = "single")
L2_20_21_single_filter_time_points_rc <- L2_cluster_fun(dist_mat = dist_mat_global_filter_time_points_20_21_rc, method = "single")

L2_18_19_av_filter_rc <- L2_cluster_fun(dist_mat = dist_mat_global_filter_time_points_18_19_rc, method = "average")
L2_20_21_av_filter_rc <- L2_cluster_fun(dist_mat = dist_mat_global_filter_time_points_20_21_rc, method = "average")

L2_18_19_ward_filter_rc <- L2_cluster_fun(dist_mat = dist_mat_global_filter_time_points_18_19_rc, method = "ward.D2")
L2_20_21_ward_filter_rc <- L2_cluster_fun(dist_mat = dist_mat_global_filter_time_points_20_21_rc, method = "ward.D2")


rm(list = setdiff(ls(), ls()[startsWith(ls(), "L") & !endsWith(ls(), "fun")]))
save.image("00_data/Cluster_filter_time_points_rc.Rdata")


#---- Dendrogram ----

# plot function
dendro_plot_fun <- function(object) {
  title <- deparse(substitute(object))
  object %>%
    ggdendro::ggdendrogram() +
    theme(axis.text.x = element_text(size = 7), 
          plot.title = element_text(hjust = 0.5, size = 15)) +
    ggtitle(title)
}

plot_all_fun_points <- function(name, save.dendro = TRUE) {
  var_name <- name %>% 
    paste0("plot_", .)
  dendro <- dendro_plot_fun(get(name)) +
    ggtitle(name)
  assign(var_name, dendro, envir = .GlobalEnv)
  
  if(save.dendro) {
    ggsave(filename = paste0(
      here::here("04_plots/Dendrogram/time_points_filtered_rc"), "/", var_name, ".pdf"), 
      width = 1215, height = 745, units = "px", 
      dpi = 60)
  }
}

ls() %>% 
  as_tibble() %>%
  dplyr::filter(str_detect(value, "fun", negate = T) & 
                  str_detect(value, "plot", negate = T) &
                  (str_detect(value, "L1") | str_detect(value, "L2"))) %>%
  dplyr::pull(value) %>%
  purrr::map(plot_all_fun_points)

