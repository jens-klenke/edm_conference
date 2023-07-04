source(here::here('01_code/packages.R'))
# load data
#load(here::here('00_data/plot_list.Rdata'))
load(here::here('plot_list.Rdata'))

# save(log_20_21, L_1_20_21_av_0.2_index, clust_ID_20_21, points_20_21,
#      log_18_19, L1_18_19_av_0.2_index, clust_ID_18_19, points_18_19,
#      plot_list, plot_list_18_19, plot_list_20_21,
#      file = here::here('00_data/plot_list.Rdata'))

# plot function 
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
               alpha = 0.9, 
               size = 2) + 
    ylim(limit) +
    ylab("Time \n") +
    xlab("\n Task") +
    theme_classic() +
    theme(
      plot.margin = unit(c(0, 0, 0.5, 0.5), "cm"),
      panel.grid.major.x = element_line(),
      text = element_text(size = 20), #from 10
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = -0.02), # here change
      axis.title = element_text(size = 32),### from 8
      legend.title = element_text(size = 28)) + ### from 8
    scale_shape_manual(values = c(16, 5, 0, 6, 4), 
                       labels = paste(LETTERS[k], 
                                      as.character(1:2), 
                                      sep = "."), 
                       name = "Students", 
                       na.translate = F) +
    scale_colour_manual(values = ggthemes::colorblind_pal()(8)[c(2, 6, 4, 8, 5)], 
                        labels = paste(LETTERS[k], 
                                       as.character(1:2), 
                                       sep = "."), 
                        name = "Students", 
                        na.translate = F) +
    scale_fill_manual(values = ggthemes::colorblind_pal()(8)[c(2, 6, 4, 8, 5)], 
                      labels = paste(LETTERS[k], 
                                     as.character(1:2), 
                                     sep = "."), 
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
    inner_join(ex, by = "exercise") %>%
    arrange(exercise) %>%
    ggplot() + 
    geom_col(aes(x = exercise, 
                 y = points, 
                 fill = STUDI_ID), 
             alpha = 0.9,
             position = position_dodge(preserve = "single"), 
             width = 0.8) +
    ylab("Points") +
    theme_classic() +
    theme(
      plot.margin = unit(c(2, -0.5, 0.2, 0), "cm"),
      panel.grid.major.x = element_line(),
      text = element_text(size = 10), 
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 32),###from 8
      legend.title = element_text(size = 28)) + ### from 8
    scale_fill_manual(
      values = ggthemes::colorblind_pal()(8)[c(2, 6, 4, 8, 5)],
      labels = paste(LETTERS[clust], 
                     as.character(1:10), 
                     sep = "."), 
      name = "Students") + 
    scale_x_discrete(labels = c(
      "1.4" = "1.3", 
      "6a.1" = "6a", 
      "6b.1" = "6b", 
      "6c.1" = "6c", 
      "7.1" = "7"
    )) +
    scale_y_continuous(breaks = c(0, 100))
  
  
  ggpubr::ggarrange(point_plot, 
                    time_plot, 
                    heights = c(0.75, 2.5), 
                    ncol = 1, 
                    common.legend = TRUE, 
                    align = "v",
                    legend = "right", 
                    legend.grob = ggpubr::get_legend(time_plot))
}
clust_ID <- clust_ID_20_21

plot_list <- list()
k <- 1
for (i in L_1_20_21_av_0.2_index) {
  plot_list[[k]] <- log_20_21 %>%
    plot_cluster_stud(point_data = points_20_21, 
                      clust = i, 
                      k = k)
  k <- k + 1
}

plot_list_20_21 <- plot_list

grid_plot_list <- function(plot_list, index){
  cowplot::plot_grid(
    plotlist = plot_list[index],
    labels = LETTERS[index], 
    label_size = 48,
    vjust = 1, #default is 1.5
    ncol = 2 #changed from 2 to 3
  )
}

plot_height <- 10

# Plot AB
cowplot::plot_grid(
  plot_list_20_21[[1]], NULL, plot_list_20_21[[2]],
  labels = c(LETTERS[1], "", LETTERS[2]),
  rel_widths = c(1, 0.1, 1),
  label_size = 48,
  vjust = 1, #default is 1.5
  ncol = 3 #changed from 2 to 3
)

ggsave(filename = "resources/graphics/plot_ab.png", 
       dpi = 300,
       height = plot_height, width = (21/9)*plot_height)

# Plot CD
cowplot::plot_grid(
  plot_list_20_21[[3]], NULL, plot_list_20_21[[4]],
  labels = c(LETTERS[3], "", LETTERS[4]),
  rel_widths = c(1, 0.1, 1),
  label_size = 48,
  vjust = 1, #default is 1.5
  ncol = 3 #changed from 2 to 3
)

ggsave(filename = "resources/graphics/plot_cd.png", 
       dpi = 300,
       height = plot_height, width = (21/9)*plot_height)


# Plot EF
cowplot::plot_grid(
  plot_list_20_21[[5]], NULL, plot_list_20_21[[6]],
  labels = c(LETTERS[5], "", LETTERS[6]),
  rel_widths = c(1, 0.1, 1),
  label_size = 48,
  vjust = 1, #default is 1.5
  ncol = 3 #changed from 2 to 3
)

ggsave(filename = "resources/graphics/plot_ef.png", 
       dpi = 300,
       height = plot_height, width = (21/9)*plot_height)

############################################################################
## 2018/ 2019
############################################################################

clust_ID <- clust_ID_18_19

k <- 1
plot_list <- list()
for (i in L1_18_19_av_0.2_index) {
  plot_list[[k]] <- log_18_19 %>%
    plot_cluster_stud(point_data = points_18_19, 
                      clust = i, k = k+6)
  k <- k + 1
}
plot_list_18_19 <- plot_list

# gh 
cowplot::plot_grid(
  plot_list_18_19[[1]], NULL, plot_list_18_19[[2]],
  labels = c(LETTERS[1+6], "", LETTERS[2+6]),
  rel_widths = c(1, 0.1, 1),
  label_size = 48,
  vjust = 1, #default is 1.5
  ncol = 3 #changed from 2 to 3
)

ggsave(filename = "resources/graphics/plot_gh.png", 
       dpi = 300,
       height = plot_height, width = (21/9)*plot_height)

# ij
cowplot::plot_grid(
  plot_list_18_19[[3]], NULL, plot_list_18_19[[4]],
  labels = c(LETTERS[3+6], "", LETTERS[4+6]),
  rel_widths = c(1, 0.1, 1),
  label_size = 48,
  vjust = 1, #default is 1.5
  ncol = 3 #changed from 2 to 3
)

ggsave(filename = "resources/graphics/plot_ij.png", 
       dpi = 300,
       height = plot_height, width = (21/9)*plot_height)



# kl
cowplot::plot_grid(
  plot_list_18_19[[5]], NULL, plot_list_18_19[[6]],
  labels = c(LETTERS[5+6], "", LETTERS[6+6]),
  rel_widths = c(1, 0.1, 1),
  label_size = 48,
  vjust = 1, #default is 1.5
  ncol = 3 #changed from 2 to 3
)

ggsave(filename = "resources/graphics/plot_kl.png", 
       dpi = 300,
       height = plot_height, width = (21/9)*plot_height)


#########################################################
'Poster'
#########################################################

# change plot funktion for poster
plot_cluster_stud_poster <- function(data, point_data, clust, k) {
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
               alpha = 0.9, 
               size = 5) + 
    ylim(limit) +
    ylab("Time \n") +
    xlab("\n Task") +
    theme_classic() +
    theme(
      plot.margin = unit(c(0, 0, 0.5, 0.5), "cm"),
      panel.grid.major.x = element_line(),
      text = element_text(size = 20), #from 10
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = -0.02, size = 24), # here change
      axis.text.y = element_text(size = 24), 
      axis.title = element_text(size = 40),### from 8
      legend.title = element_text(size = 28)) + ### from 8
    scale_shape_manual(values = c(16, 5, 0, 6, 4), 
                       labels = paste(LETTERS[k], 
                                      as.character(1:2), 
                                      sep = "."), 
                       name = "Students", 
                       na.translate = F) +
    scale_colour_manual(values = ggthemes::colorblind_pal()(8)[c(2, 6, 4, 8, 5)], 
                        labels = paste(LETTERS[k], 
                                       as.character(1:2), 
                                       sep = "."), 
                        name = "Students", 
                        na.translate = F) +
    scale_fill_manual(values = ggthemes::colorblind_pal()(8)[c(2, 6, 4, 8, 5)], 
                      labels = paste(LETTERS[k], 
                                     as.character(1:2), 
                                     sep = "."), 
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
    inner_join(ex, by = "exercise") %>%
    arrange(exercise) %>%
    ggplot() + 
    geom_col(aes(x = exercise, 
                 y = points, 
                 fill = STUDI_ID), 
             alpha = 0.9,
             position = position_dodge(preserve = "single"), 
             width = 0.8) +
    ylab("Points") +
    theme_classic() +
    theme(
      plot.margin = unit(c(2, -0.5, 0.2, 0), "cm"),
      panel.grid.major.x = element_line(),
      text = element_text(size = 10), 
      axis.text.x = element_blank(), 
      axis.text.y = element_text(size = 24),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 40),###from 8
      legend.title = element_text(size = 40)) + ### from 8
    scale_fill_manual(
      values = ggthemes::colorblind_pal()(8)[c(2, 6, 4, 8, 5)],
      labels = paste(LETTERS[clust], 
                     as.character(1:10), 
                     sep = "."), 
      name = "Students") + 
    scale_x_discrete(labels = c(
      "1.4" = "1.3", 
      "6a.1" = "6a", 
      "6b.1" = "6b", 
      "6c.1" = "6c", 
      "7.1" = "7"
    )) +
    scale_y_continuous(breaks = c(0, 100))
  
  
  ggpubr::ggarrange(point_plot, 
                    time_plot, 
                    heights = c(0.75, 2.5), 
                    ncol = 1, 
                    common.legend = TRUE, 
                    align = "v",
                    legend = "right", 
                    legend.grob = ggpubr::get_legend(time_plot))
}




clust_ID <- clust_ID_20_21

plot_list_poster <- list()
k <- 1
for (i in L_1_20_21_av_0.2_index) {
  plot_list_poster[[k]] <- log_20_21 %>%
    plot_cluster_stud_poster(point_data = points_20_21,
                             clust = i,
                             k = k)
  k <- k + 1
}

plot_height <- 10
# Poster maybe later; B 
plot_list_poster[2]

# ggsave(filename = here::here('resources/graphics/plot_b_poster.png'), 
#        dpi = 300,
#        height = plot_height, width = (21/9)*plot_height)

# svg try
ggsave(filename = here::here('resources/graphics/plot_b_poster.svg'), 
       dpi = 300,
       height = plot_height, width = (21/9)*plot_height)
