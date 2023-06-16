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
                alpha = 0.25) +
    stat_boxplot(#aes(fill = group),
                 #alpha = 0.8, 
                 outlier.alpha = 0.3, 
                 outlier.size = 1.8) +
    xlab("") +
    ylab("Distance") +
    theme_classic() +
    guides(fill = "none", 
           colour = "none") +
    scale_colour_manual(values = ggthemes::colorblind_pal()(8)[c(2, 6)]) +
    scale_fill_manual(values = ggthemes::colorblind_pal()(8)[c(2, 6)]) +
    scale_x_discrete(labels = c("Comparison Group \n 18/19", "Test Group \n 20/21"))# +
#    theme(axis.title.x = element_text(size = 30),   # Increase x axis title size
#          axis.title.y = element_text(size = 30),   # Increase y axis title size
#          axis.text.x = element_text(size = 20),    # Increase x axis tick label size
#          axis.text.y = element_text(size = 20)) +    # Increase y axis tick label size
    #geom_ellipse(aes(x0 = 2, y0 = -6, a = 0.01, b = 0.02, angle=0), 
     #            fill = "yellow", 
      #           alpha = 0.1)
}




# Filtered Data with time and points
dist_boxplot()
#ggsave(filename = "04_plots/Boxplots/Boxplot_time_points_filtered.pdf", 
#       height = 4, width = 4)



#---- Wilcoxon Test ----

wilcox.test(x = dist_mat_global_filter_time_points_18_19$dist_L1,
            y = dist_mat_global_filter_time_points_20_21$dist_L1, 
            paired = FALSE, 
            conf.int = TRUE)
# p-value < 2.2e-16

# Norm Dist
dist_mat_global_filter_time_points_18_19 %<>%
  dplyr::mutate(dist_L1_norm = (dist_L1 - mean(dist_L1))/sd(dist_L1))
dist_mat_global_filter_time_points_20_21 %<>%
  dplyr::mutate(dist_L1_norm = (dist_L1 - mean(dist_L1))/sd(dist_L1))

# Minimum value of Control Group 18_19
norm_degree <- dist_mat_global_filter_time_points_18_19 %$% 
  min(dist_L1_norm)
# -3.124476

# Plot of norm. Dist.
dist_boxplot(dist = "dist_L1_norm") + ylab("")
ggsave(filename = "04_plots/Boxplots/Boxplot_time_points_filtered_norm.pdf", 
       height = 4, width = 4)


################################################################################
'Jens: Run code till here to get boxplot with noramlized values '
#################################################################################


# Detect outliers < "nat. Minimum"
data_bp <- dist_boxplot(dist = "dist_L1_norm")$data %>%
  dplyr::filter(group == "20_21")

# Nat. Minimum = Minimum Control Group
data_bp %>%
  dplyr::filter(dist_L1_norm < norm_degree) %>%
  dplyr::arrange(dist_L1_norm)
# 16

# Detect outliers
L1_norm_18_19_stats <- boxplot(
  dist_mat_global_filter_time_points_18_19$dist_L1_norm, 
  plot = FALSE)$stats
Q1 <- L1_norm_18_19_stats[2, ]
Q3 <- L1_norm_18_19_stats[4, ]
IQR_norm_18_19 <- Q3 - Q1

# "Normal" Outliers
upper_bound <- Q3 + 1.5 * IQR_norm_18_19
# 2.57478
lower_bound <- Q1 - 1.5 * IQR_norm_18_19
# -2.598332

data_bp %>%
  dplyr::filter(dist_L1_norm < lower_bound) %>%
  dplyr::arrange(dist_L1_norm)

# Extreme Outliers
upper_bound_ex <- Q3 + 3 * IQR_norm_18_19
# 4.514698
lower_bound_ex <- Q1 - 3 * IQR_norm_18_19
# -4.53825

data_bp %>%
  dplyr::filter(dist_L1_norm < lower_bound_ex) %>%
  dplyr::arrange(dist_L1_norm)
