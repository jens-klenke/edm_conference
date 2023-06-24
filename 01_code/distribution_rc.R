# Distribution of the distances between students

# load Distances and Packages
source('01_code/packages.R')
load("00_data/Distmat_filter_time_points_rc.Rdata")

# Remove duplicates
rm_duplicates <- function(dist_mat) {
  dist_mat %>% 
    dplyr::filter(v_1 != v_2) %>%
    rowwise() %>%
    dplyr::mutate(key = paste(sort(c(v_1, v_2)), collapse = "")) %>%
    distinct(key, .keep_all = T) %>%
    dplyr::select(-key) %>%
    ungroup()
}

dist_mat_global_filter_time_points_18_19_rc %<>% rm_duplicates()
dist_mat_global_filter_time_points_20_21_rc %<>% rm_duplicates()


#---- Boxplots ----

# Comparing the distributions between groups with Boxplots
dist_boxplot <- function(dist_18_19 = dist_mat_global_filter_time_points_18_19_rc, 
                         dist_20_21 = dist_mat_global_filter_time_points_20_21_rc, 
                         dist = "dist_L1") {
  # Join both tibbles and add grouping variable
  dist_18_19 %<>% 
    dplyr::mutate(group = "18_19", 
                  .before = v_1)
  dist_20_21 %<>% 
    dplyr::mutate(group = "20_21", 
                  .before = v_1)
  
  dist_mat <- dist_18_19 %>% 
    dplyr::bind_rows(dist_20_21)
  
  # Create Boxplots
  dist_mat %>% 
    ggplot(aes_string(x = "group", y = dist)) +
    geom_violin(colour = "white", 
                aes(fill = group),
                alpha = 0.25) +
    stat_boxplot(#aes(fill = group),
      #alpha = 0.8, 
      outlier.alpha = 0.3, 
      outlier.size = 1.8) +
    xlab("Group") +
    ylab("Distance") +
    theme_classic() +
    guides(fill = "none", 
           colour = "none") +
    scale_colour_manual(values = ggthemes::colorblind_pal()(8)[c(2, 6)]) +
    scale_fill_manual(values = ggthemes::colorblind_pal()(8)[c(2, 6)]) +
    scale_x_discrete(labels = c("18/19", "20/21"))
}

# Filtered Data with time and points
dist_boxplot()
ggsave(filename = "04_plots/Robustness_Check/Boxplot_time_points_filtered_rc.pdf", 
       height = 4, width = 7)

# Norm Dist
dist_mat_global_filter_time_points_18_19_rc %<>%
  dplyr::mutate(dist_L1_norm = (dist_L1 - mean(dist_L1))/sd(dist_L1))
dist_mat_global_filter_time_points_20_21_rc %<>%
  dplyr::mutate(dist_L1_norm = (dist_L1 - mean(dist_L1))/sd(dist_L1))

dist_mat_global_filter_time_points_18_19_rc %$% 
  min(dist_L1_norm)
# -3.463114

dist_boxplot(dist = "dist_L1_norm") 
ggsave(filename = "04_plots/Robustness_Check/Boxplot_time_points_filtered_norm.pdf", 
       height = 4, width = 7)


