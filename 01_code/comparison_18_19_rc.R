# Load Data and Packages
source('01_code/packages.R')
load("00_data/log_files.Rdata")
load("00_data/Cluster_filter_time_points_rc.Rdata")
load("00_data/points.Rdata")
load("00_data/Distmat_filter_time_points_rc.Rdata")
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
    #set("labels_colors", "white") %>%
    hang.dendrogram(hang = 0.1) %>% 
    set("labels_cex", 0.55)
  
  ddata %>%
    plot(cex.axis = 0.6) 
}


pdf(file = "04_plots/Robustness_Check/dendro_anon_single_18_19_rc.pdf", 
    width = 7, height = 1.5)
par(mar = c(1, 2, 1, 0))
L1_18_19_single_filter_time_points_rc %>% 
  lab_plot()
dev.off()

pdf(file = "04_plots/Robustness_Check/dendro_anon_comp_18_19_rc.pdf", 
    width = 7, height = 1.5)
par(mar = c(1, 2, 1, 0))
L1_18_19_complete_filter_time_points_rc %>%
  lab_plot()
dev.off()

pdf(file = "04_plots/Robustness_Check/dendro_anon_av_18_19_rc.pdf", 
    width = 7, height = 1.5)
par(mar = c(1, 2, 1, 0))
L1_18_19_av_filter_time_points_rc %>%
  lab_plot()
dev.off()



#---- Organize hclust objects in dendlist ----
set_custom <- function(hcl) {
  hcl %>%
    as.dendrogram() %>% 
    set("branches_lwd", 0.7) #%>%
  # set("labels_col", "white")
}

L1_18_19_dendlist_rc <- dendextend::dendlist(L1_18_19_single_filter_time_points_rc %>% 
                                            set_custom(),
                                          L1_18_19_complete_filter_time_points_rc %>% 
                                            set_custom(),
                                          L1_18_19_av_filter_time_points_rc %>% 
                                            set_custom())
names(L1_18_19_dendlist_rc) <- c("single", "complete", "average")

# Correlation between different trees
coph_corr <- dendextend::cor.dendlist(L1_18_19_dendlist_rc)
corrplot::corrplot(coph_corr, "circle", "lower")


#---- Cophenetic correlation between distance matrices and dendrogram ----
L1_18_19_dist <- dist_mat_global_filter_time_points_18_19_rc %>% 
  pivot_wider(names_from = v_2, 
              values_from = dist_L1, 
              id_cols = v_1) %>%
  dplyr::select(-v_1) %>%
  as.dist()

L1_18_19_single_coph <- cophenetic(L1_18_19_single_filter_time_points_rc)
cor(L1_18_19_dist, L1_18_19_single_coph)
# 0.5766705

L1_18_19_complete_coph <- cophenetic(L1_18_19_complete_filter_time_points_rc)
cor(L1_18_19_dist, L1_18_19_complete_coph) 
# 0.4266997

L1_18_19_av_coph <- cophenetic(L1_18_19_av_filter_time_points_rc)
cor(L1_18_19_dist, L1_18_19_av_coph) 
# 0.6083855


#---- Compare Students ----

# Identify smallest h which results in 16 Clusters
smallest_h <- function(i) {
  L1_18_19_av_i <- L1_18_19_dendlist_rc$average %>% 
    dendextend::cutree(h = i, 
                       use_labels_not_values = T, 
                       order_clusters_as_data = F)
  L1_18_19_av_i[duplicated(L1_18_19_av_i)] %>% 
    unique() %>% 
    length()
}

steps <- 0.2
while(smallest_h(i = steps) < 16) {
  print(steps)
  steps = steps + 0.001
}

0.229 + 0.001
# 0.23

# Cut Dendrogram at h = 0.23
pdf(file = "04_plots/Robustness_Check/dendro_anon_av_0.2_18_19.pdf", 
    width = 7, height = 1.5)
par(mar = c(1, 4, 1, 0))
L1_18_19_dendlist_rc$average %>%
  hang.dendrogram(hang = 0.1) %>%
  set("labels_cex", 0.55) %>%
  dendextend::color_branches(h = 0.23#, 
                             #groupLabels = TRUE
  ) %>%
  plot(ylim = c(-0.15, 0.45), 
       axes = F, 
       ylab = "proximity")
abline(h = 0.23, col = "red")
axis(side = 2, at = seq(0, 0.4, 0.1))
dev.off()

L1_18_19_av_0.2 <- L1_18_19_dendlist_rc$average %>% 
  dendextend::cutree(h = c(0.23), 
                     use_labels_not_values = T, 
                     order_clusters_as_data = F)

L1_18_19_av_0.2_index <- L1_18_19_av_0.2[duplicated(L1_18_19_av_0.2)] %>%
  unique()
clust_ID <- L1_18_19_av_0.2[L1_18_19_av_0.2 %in% L1_18_19_av_0.2_index]

plot_cluster_stud <- function(data, point_data, clust) {
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
                       #labels = paste(LETTERS[clust], 
                       #                as.character(1:10), 
                       #               sep = "."), 
                       name = "Students", 
                       na.translate = F) +
    scale_colour_manual(values = ggthemes::colorblind_pal()(8)[c(2, 6, 4, 8, 1, 5, 7)], 
                        #labels = paste(LETTERS[clust], 
                        #               as.character(1:10), 
                        #               sep = "."), 
                        name = "Students", 
                        na.translate = F) +
    scale_fill_manual(values = ggthemes::colorblind_pal()(8)[c(2, 6, 4, 8, 1, 5, 7)], 
                      #labels = paste(LETTERS[clust], 
                      #               as.character(1:10), 
                      #               sep = "."), 
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
      #labels = paste(LETTERS[clust], 
      #               as.character(1:10), 
      #               sep = "."), 
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
                      clust = i)
  k <- k + 1
}

grid_plot_list <- function(plot_list, clust){
  cowplot::plot_grid(
    plotlist = plot_list[clust],
    labels = LETTERS[clust], 
    ncol = 2
  )
}

grid_plot_list(plot_list, 1:4)
ggsave(filename = "04_plots/Robustness_Check/cowplot_comp_rc_1_18_19.pdf", 
       height = 7, width = 7.17)

grid_plot_list(plot_list, 5:8)
ggsave(filename = "04_plots/Robustness_Check/cowplot_comp_rc_2_18_19.pdf", 
       height = 7, width = 7.17)

grid_plot_list(plot_list, 9:12)
ggsave(filename = "04_plots/Robustness_Check/cowplot_comp_rc_3_18_19.pdf",
       height = 7, width = 7.17)

grid_plot_list(plot_list, 13:16)
ggsave(filename = "04_plots/Robustness_Check/cowplot_comp_rc_4_18_19.pdf",
       height = 7, width = 7.17)

grid_plot_list(plot_list, 17)
ggsave(filename = "04_plots/Robustness_Check/cowplot_comp_rc_5_18_19.pdf",
       height = 7, width = 7.17)


# Top 6 Cluster
L1_18_19_av_cut_0.2 <- L1_18_19_dendlist_rc$average %>% 
  cut(h = 0.23) %>% 
  extract2(2)

# Remove Leaves
cut_0.2_index <- L1_18_19_av_cut_0.2 %>% 
  purrr::map(~attr(., "member"))
cut_0.2_index %>% 
  unlist() %>%
  unique()
cut_0.2_index_2 <- which(cut_0.2_index >= 2)
cut_0.2_index_3 <- which(cut_0.2_index == 3)
# 69 70 71
cut_0.2_index_4 <- which(cut_0.2_index == 4)
# 33


# Height = Height of lowest node in Cluster
L1_18_19_av_cut_0.2_cl <- L1_18_19_av_cut_0.2 %>%
  extract(cut_0.2_index_2)
L1_18_19_av_cut_0.2_cl[[which(cut_0.2_index_2 == cut_0.2_index_3[1])]] <- L1_18_19_av_cut_0.2 %>%
  extract2(cut_0.2_index_3[1]) %>%
  extract2(2)
L1_18_19_av_cut_0.2_cl[[which(cut_0.2_index_2 == cut_0.2_index_3[2])]] <- L1_18_19_av_cut_0.2 %>%
  extract2(cut_0.2_index_3[2]) %>%
  extract2(2)
L1_18_19_av_cut_0.2_cl[[which(cut_0.2_index_2 == cut_0.2_index_3[3])]] <- L1_18_19_av_cut_0.2 %>%
  extract2(cut_0.2_index_3[3]) %>%
  extract2(2)
L1_18_19_av_cut_0.2_cl[[which(cut_0.2_index_2 == cut_0.2_index_4)]] <- L1_18_19_av_cut_0.2 %>%
  extract2(cut_0.2_index_4) %>%
  extract2(2) %>%
  extract2(2)

cl_function  <- function(x){tibble(
  height = attr(x, "height"), 
  member = list(partition_leaves(x)[[1]])
)}

L1_18_19_av_cut_0.2_cl %<>%
  purrr::map_dfr(cl_function) %>%
  mutate(clust = 1:17) %>%
  arrange(height)

clust_no <- as.numeric(L1_18_19_av_cut_0.2_cl$clust[1:6])
cowplot::plot_grid(plotlist = plot_list[clust_no],
                   labels = LETTERS[clust_no],
                   ncol = 2)
ggsave(filename = "04_plots/Robustness_Check/cowplot_18_19_top_6_rc.pdf", 
       height = 9, width = 7.17)





