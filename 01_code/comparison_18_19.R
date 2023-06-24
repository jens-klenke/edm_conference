# Load Data and Packages
source('01_code/packages.R')
load("00_data/log_files.Rdata")
load("00_data/Cluster_filter_time_points.Rdata")
load("00_data/points.Rdata")
load("00_data/Distmat_filter_time_points.Rdata")
rm(list = setdiff(ls(), ls()[!startsWith(ls(), "L2") & !grepl("20_21", ls())]))

log_18_19 %<>% 
  dplyr::select(STUDI_ID, exercise, timestamp, elapsed_time_n) %>%
  dplyr::mutate(elapsed_time_n = time_length(elapsed_time_n, unit = "minute"), 
                exercise = as.factor(exercise))


#---- Plot linkage methods ----

# Plots with no labels
lab_plot <- function(hcobj) {
  ddata <- hcobj %>% 
    as.dendrogram() %>%
    #rank_branches() %>%
    set("branches_lwd", 0.7) %>%
    set("labels_colors", "white") %>%
    hang.dendrogram(hang = 0.1) %>% 
    set("labels_cex", 0.55)
  
  ddata %>%
    plot(cex.axis = 0.7) 
}


pdf(file = "04_plots/Dendrogram/dendro_anon_single_18_19.pdf", 
    width = 7, height = 1.5)
par(mar = c(1, 2, 1, 0))
L1_18_19_single_filter_time_points %>% 
  lab_plot()
dev.off()

pdf(file = "04_plots/Dendrogram/dendro_anon_comp_18_19.pdf", 
    width = 7, height = 1.5)
par(mar = c(1, 2, 1, 0))
L1_18_19_complete_filter_time_points %>%
  lab_plot()
dev.off()

pdf(file = "04_plots/Dendrogram/dendro_anon_av_18_19.pdf", 
    width = 7, height = 1.5)
par(mar = c(1, 2, 1, 0))
L1_18_19_av_filter_time_points %>%
  lab_plot()
dev.off()

#----Organize hclust objects in dendlist ----
set_custom <- function(hcl) {
  hcl %>%
    as.dendrogram() %>% 
    set("branches_lwd", 0.7) %>%
    set("labels_col", "white")
}
  
L1_18_19_dendlist <- dendextend::dendlist(L1_18_19_single_filter_time_points %>% 
                                            set_custom(),
                                          L1_18_19_complete_filter_time_points %>% 
                                            set_custom(),
                                          L1_18_19_av_filter_time_points %>% 
                                            set_custom())
names(L1_18_19_dendlist) <- c("single", "complete", "average")


# Correlation between different trees
coph_corr <- dendextend::cor.dendlist(L1_18_19_dendlist)
corrplot::corrplot(coph_corr, "circle", "lower")


#---- Cophenetic correlation between distance matrices and dendrogram ----
L1_18_19_dist <- dist_mat_global_filter_time_points_18_19 %>% 
  pivot_wider(names_from = v_2, 
              values_from = dist_L1, 
              id_cols = v_1) %>%
  dplyr::select(-v_1) %>%
  as.dist()

L1_18_19_single_coph <- cophenetic(L1_18_19_single_filter_time_points)
cor(L1_18_19_dist, L1_18_19_single_coph)
# 0.6658675

L1_18_19_complete_coph <- cophenetic(L1_18_19_complete_filter_time_points)
cor(L1_18_19_dist, L1_18_19_complete_coph) 
# 0.5050966

L1_18_19_av_coph <- cophenetic(L1_18_19_av_filter_time_points)
cor(L1_18_19_dist, L1_18_19_av_coph) 
# 0.7595485

#---- Compare Students ----

# Identify smallest h which results in 16 Clusters
smallest_h <- function(i) {
  L1_18_19_av_i <- L1_18_19_dendlist$average %>% 
  dendextend::cutree(h = i, 
                     use_labels_not_values = T, 
                     order_clusters_as_data = F)
  L1_18_19_av_i[duplicated(L1_18_19_av_i)] %>% 
    unique() %>% 
    length()
}

steps <- 0.1
while(smallest_h(i = steps) < 6) {
  print(steps)
  steps = steps + 0.001
}

0.236 + 0.001
# 0.237

L1_18_19_av_0.2 <- L1_18_19_dendlist$average %>% 
  dendextend::cutree(h = c(0.237), 
                     use_labels_not_values = T, 
                     order_clusters_as_data = F)

L1_18_19_av_0.2_index <- L1_18_19_av_0.2[duplicated(L1_18_19_av_0.2)] %>%
  unique()
clust_ID <- L1_18_19_av_0.2[L1_18_19_av_0.2 %in% L1_18_19_av_0.2_index]
leaf_xy <- tibble(
  y = L1_18_19_dendlist$average %>% 
    hang.dendrogram(hang = 0.1) %>% 
    dendextend::get_leaves_attr("height") -0.015, 
  x = 1:attr(L1_18_19_dendlist$average, "member")
)
clust_xy <- leaf_xy %>% dplyr::slice(which(duplicated(L1_18_19_av_0.2)))

pdf(file = "04_plots/Dendrogram/dendro_18_19_anon_av_02.pdf", 
    width = 8, height = 4)
par(mar = c(0, 4, 0, 0))
L1_18_19_dendlist$average %>%
  hang.dendrogram(hang = 0.1) %>%
  set("labels_cex", 0.55) %>%
  #dendextend::color_branches(h = 0.237#, 
                             #groupLabels = TRUE
  #) %>%
  plot(ylim = c(-0.15, 0.45), 
       axes = F, 
       ylab = "Dissimilarity")
#abline(h = 0.237, col = "red")
axis(side = 2, at = seq(0, 0.4, 0.1))
for (i in 1:6) {
  text(x = clust_xy$x[i], 
       y = clust_xy$y[i], 
       labels = LETTERS[i+6])
}
dev.off()

plot_cluster_stud <- function(data, point_data, clust, k) {
  limit <- range(data$timestamp)
 
  ex <- data %>% 
    distinct(exercise)
  
  ID <- clust_ID[clust_ID == clust] %>%
    labels()
  
  time_plot <- data %>%
    dplyr::filter(STUDI_ID %in% ID) %>%
    arrange(exercise) %>%
    left_join(ex, by = "exercise") %>% 
    ggplot(.) + 
    geom_point(aes(y = timestamp, x = exercise, 
                   fill = STUDI_ID, 
                   color = STUDI_ID, 
                   shape = STUDI_ID), 
               size = 2, 
               alpha = 0.9) + 
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
    scale_shape_manual(values = c(16, 5, 0, 6, 1, 2, 9), 
                       labels = paste(LETTERS[k], 
                                      as.character(1:2), 
                                      sep = "."), 
                       name = "Students", 
                       na.translate = F) +
    scale_colour_manual(values = ggthemes::colorblind_pal()(8)[c(2, 6, 4, 8, 1, 5, 7)], 
                        labels = paste(LETTERS[k], 
                                       as.character(1:2), 
                                       sep = "."), 
                        name = "Students", 
                        na.translate = F) +
    scale_fill_manual(values = ggthemes::colorblind_pal()(8)[c(2, 6, 4, 8, 1, 5, 7)], 
                      labels = paste(LETTERS[k], 
                                     as.character(1:2), 
                                     sep = "."), 
                      name = "Students", 
                      na.translate = F) + 
    scale_x_discrete(labels = c(
      "4.1" = "4", 
      "6a.1" = "6a",
      "6b.1" = "6b",
      "6c.1" = "6c"
    ))
  
  point_plot <- point_data %>%
    dplyr::filter(STUDI_ID %in% ID) %>%
    inner_join(ex, by = "exercise") %>%
    arrange(exercise) %>%
    ggplot() + 
    geom_col(aes(x = exercise, 
                 y = points, 
                 fill = STUDI_ID), 
             alpha = 0.9,
             position = position_dodge(preserve = "single"), 
             width = 0.8) +
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
      values = ggthemes::colorblind_pal()(8)[c(2, 6, 4, 8, 1, 5, 7)],
      labels = paste(LETTERS[k], 
                     as.character(1:2), 
                     sep = "."), 
      name = "Students") + 
    scale_x_discrete(labels = c(
      "4.1" = "4", 
      "6a.1" = "6a",
      "6b.1" = "6b",
      "6c.1" = "6c"
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

plot_list <- list()
k <- 1
for (i in L1_18_19_av_0.2_index) {
  plot_list[[k]] <- log_18_19 %>%
    plot_cluster_stud(point_data = points_18_19, 
                      clust = i, k = k+6)
  k <- k + 1
}

grid_plot_list <- function(plot_list, clust){
  cowplot::plot_grid(
    plotlist = plot_list[clust],
    labels = LETTERS[clust+6], 
    ncol = 2
  )
}

grid_plot_list(plot_list, 1:6)
ggsave(filename = "04_plots/Student_comparison/cowplot_comp_6_18_19.pdf", 
       height = 9, width = 7.17)
