# Distribution of the distances between students

# load Distances and Packages
source('01_code/packages.R')
load("00_data/Distmat_filter_time_points.Rdata")

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

dist_mat_global_filter_time_points_18_19 %<>% rm_duplicates()
dist_mat_global_filter_time_points_20_21 %<>% rm_duplicates()


#---- Boxplots ----
library(ggforce)
# Comparing the distributions between groups with Boxplots
dist_boxplot <- function(dist_18_19 = dist_mat_global_filter_time_points_18_19, 
                         dist_20_21 = dist_mat_global_filter_time_points_20_21, 
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
                alpha = 0.25, 
                width = 0.3) +
    stat_boxplot(#aes(fill = group),
                 #alpha = 0.8, 
                 width = 0.25,
                 outlier.alpha = 0.3, 
                 outlier.size = 1.8) +
    xlab("") +
    ylab("Distance \n") +
    theme_classic() +
    guides(fill = "none", 
           colour = "none") +
    scale_colour_manual(values = ggthemes::colorblind_pal()(8)[c(2, 6)]) +
    scale_fill_manual(values = ggthemes::colorblind_pal()(8)[c(2, 6)]) +
    scale_x_discrete(labels = c("\n Comparison Group \n 18/19", "\n Test Group \n 20/21")) + # +
    theme(#axis.title.x = element_text(size = 30),   # Increase x axis title size
#          axis.title.y = element_text(size = 30),   # Increase y axis title size
          axis.text.y = element_text(size = 30),
          axis.text.x = element_text(size = 40)
          )    # Increase  axis tick label size
    #geom_ellipse(aes(x0 = 2, y0 = -6, a = 0.01, b = 0.02, angle=0), 
     #            fill = "yellow", 
      #           alpha = 0.1)
}

dist_boxplot_1 <- dist_boxplot() + 
  ylim(c(0.05, 0.6))

# ggsave(dist_boxplot_1, filename = here::here("resources/graphics/boxplot_original.png"),
#        dpi = 1200,
#        height = 3, width = 4)

dist_boxplot_1_marked <- dist_boxplot_1 +
annotate("rect", xmin = 1.95, xmax = 2.05, ymin = 0.1, ymax = 0.05,
         alpha = .1,fill = "red", color = 'red')

# ggsave(dist_boxplot_1_marked, filename = here::here("resources/graphics/boxplot_original_marked.png"),
#        dpi = 1200,
#        height = 3, width = 4)

################################## 
'Norm Dist'
#################################
dist_mat_global_filter_time_points_18_19 %<>%
  dplyr::mutate(dist_L1_norm = (dist_L1 - mean(dist_L1))/sd(dist_L1))
dist_mat_global_filter_time_points_20_21 %<>%
  dplyr::mutate(dist_L1_norm = (dist_L1 - mean(dist_L1))/sd(dist_L1))

# Minimum value of Control Group 18_19
norm_degree <- dist_mat_global_filter_time_points_18_19 %$% 
  min(dist_L1_norm)
# -3.124476

##### trans 

limit_trans_low <- (0.05 - mean(dist_mat_global_filter_time_points_20_21$dist_L1))/sd(dist_mat_global_filter_time_points_20_21$dist_L1)

limit_trans_high <- (0.6 -mean(dist_mat_global_filter_time_points_20_21$dist_L1))/sd(dist_mat_global_filter_time_points_20_21$dist_L1)


# Plot of norm. Dist.
boxplot_norm_1 <- dist_boxplot(dist = "dist_L1_norm") +
  ylim(c(limit_trans_low, limit_trans_high)) +
  ylab("")

# ggsave(boxplot_norm_1, filename = "resources/graphics/boxplot_norm.png", 
#        dpi = 1200,
#        height = 3, width = 4)

# marked 
#boxplot_norm_1_marked <- 

trans <- (0.1 -mean(dist_mat_global_filter_time_points_20_21$dist_L1))/sd(dist_mat_global_filter_time_points_20_21$dist_L1)
trans_1 <- (0.05 -mean(dist_mat_global_filter_time_points_20_21$dist_L1))/sd(dist_mat_global_filter_time_points_20_21$dist_L1)

dist_boxplot_1_marked <- boxplot_norm_1 +
  annotate("rect", xmin = 1.98, xmax = 2.02, ymin = trans, ymax = trans_1,
           alpha = .1,fill = "red", color = 'red')

# ggsave(dist_boxplot_1_marked, filename = here::here("resources/graphics/boxplot_norm_marked.png"),
#        dpi = 1200,
#        height = 3, width = 4)

plot_height <- 10
# Poster 
# ggsave(dist_boxplot_1_marked,
#        filename = here::here('resources/graphics/boxplot_norm_marked_poster.png'),
#        dpi = 1200,
#        height = plot_height, 
#        width = (16/9)*plot_height)

ggsave(dist_boxplot_1_marked,
       filename = here::here('resources/graphics/boxplot_norm_marked_poster.svg'),
       dpi = 1200,
       height = plot_height, 
       width = (21/9)*plot_height)



