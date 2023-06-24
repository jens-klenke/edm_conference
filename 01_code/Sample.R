source('01_code/packages.R')
load("00_data/log_files.Rdata")
load("00_data/points.Rdata")
load("00_data/Distmat_filter_time_points.Rdata")
rm(S_v_18_19_filter_points, S_v_20_21_filter_points)

log_18_19 %<>% 
  dplyr::select(STUDI_ID, exercise, timestamp, elapsed_time_n) %>%
  dplyr::mutate(elapsed_time_n = time_length(elapsed_time_n, unit = "minute"), 
                exercise = as.factor(exercise))

log_20_21 %<>% 
  dplyr::select(STUDI_ID, exercise, timestamp, elapsed_time_n) %>%
  dplyr::mutate(elapsed_time_n = time_length(elapsed_time_n, unit = "minute"), 
                exercise = as.factor(exercise))


# Function 20_21
plot_cluster_stud_sample <- function(data, point_data, dist_mat) {
  limit <- range(data$timestamp)
  
  ex <- data %>% 
    distinct(exercise)
  
  ID <- dist_mat %$% 
    unique(v_1) %>%
    sample(size = 2)
  
  time_plot <- data %>%
    dplyr::filter(STUDI_ID %in% ID) %>%
    arrange(exercise) %>%
    full_join(ex, by = "exercise") %>% 
    ggplot(.) + 
    geom_point(aes(y = timestamp, x = exercise, 
                   fill = STUDI_ID, 
                   color = STUDI_ID, 
                   shape = STUDI_ID), 
               alpha = 0.9, 
               size = 2) + 
    ylim(limit) +
    ylab("time") +
    theme_classic() +
    theme(
      plot.margin = unit(c(0, 0, 0.5, 0.5), "cm"),
      panel.grid.major.x = element_line(),
      text = element_text(size = 10), 
      axis.text.x = element_text(angle = 90), 
      axis.title = element_text(size = 8),
      legend.title = element_text(size = 8)) +
    scale_shape_manual(values = c(16, 5, 0, 6), 
                      # labels = paste(LETTERS[clust], 
                      #                as.character(1:10), 
                      #                sep = "."), 
                       name = "Students", 
                       na.translate = F) +
    scale_colour_manual(values = ggthemes::colorblind_pal()(8), 
                      #  labels = paste(LETTERS[clust], 
                      #                 as.character(1:10), 
                      #                 sep = "."), 
                        name = "Students", 
                        na.translate = F) +
    scale_fill_manual(values = ggthemes::colorblind_pal()(8), 
                      # labels = paste(LETTERS[clust], 
                      #               as.character(1:10), 
                      #               sep = "."), 
                      name = "Students", 
                      na.translate = F) + 
    scale_x_discrete(labels = c(
      "1.4" = "1.3", 
      "6a.1" = "6a", 
      "6b.1" = "6b", 
      "6c.1" = "6c", 
      "7.1" = "7"
    ))
  
  point_plot <- point_data %>%
    dplyr::filter(STUDI_ID %in% ID) %>%
    right_join(ex, by = "exercise") %>%
    arrange(exercise) %>%
    ggplot() + 
    geom_col(aes(x = exercise, 
                 y = points, 
                 fill = STUDI_ID), 
             alpha = 0.9,
             #stat = "identity", 
             position = position_dodge2(preserve = "single"), 
             # width = 0.7, 
             colour = "white") +
    ylab("points") +
    theme_classic() +
    theme(
      plot.margin = unit(c(1, -0.5, 0.2, 0), "cm"),
      panel.grid.major.x = element_line(),
      text = element_text(size = 10), 
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 8),
      legend.title = element_text(size = 8)) +
    scale_fill_manual(
      values = ggthemes::colorblind_pal()(8),
     # labels = paste(LETTERS[clust], 
     #               as.character(1:10), 
     #               sep = "."), 
     name = "Students", 
     na.translate = F) + 
    scale_x_discrete(labels = c(
      "1.4" = "1.3", 
      "6a.1" = "6a", 
      "6b.1" = "6b", 
      "6c.1" = "6c", 
      "7.1" = "7"
    )) +
    scale_y_continuous(breaks = c(0, 100))
  
  
  ggpubr::ggarrange(point_plot, time_plot, 
                    heights = c(1, 2.5), 
                    ncol = 1, 
                    common.legend = TRUE, 
                    align = "v",
                    legend = "right", 
                    legend.grob = ggpubr::get_legend(time_plot))
}


plot_list_20_21 <- list()
for (i in 1:6) {
  plot_list_20_21[[i]] <- log_20_21 %>%
    plot_cluster_stud_sample(point_data = points_20_21, 
                      dist_mat = dist_mat_global_filter_time_points_20_21)
}

plot_list_18_19 <- list()
for (i in 1:6) {
  plot_list_18_19[[i]] <- log_18_19 %>%
    plot_cluster_stud_sample(point_data = points_18_19, 
                             dist_mat = dist_mat_global_filter_time_points_18_19)
}

cowplot::plot_grid(plotlist = plot_list_20_21, 
                   ncol = 2)
cowplot::plot_grid(plotlist = plot_list_18_19, 
                   ncol = 2)
