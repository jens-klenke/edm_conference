# Load Data and Packages
source('01_code/packages.R')
load("00_data/log_files.Rdata")

#---- Filter log data ----
log_filter_fun <- function(log_data, count_ex, time) {
  log_data %<>% 
    dplyr::select(STUDI_ID, exercise, elapsed_time_n, elapsed_time_t) %>%
    dplyr::mutate(elapsed_time_n = time_length(elapsed_time_n, unit = "minute"), 
                  elapsed_time_t = time_length(elapsed_time_t, unit = "minute"), 
                  exercise = as.factor(exercise))
  
  # Find students with few answers
  count_index <- log_data %>%
    group_by(STUDI_ID) %>%
    distinct(exercise) %>%
    dplyr::summarise(count(STUDI_ID),
                     .groups = "drop") %>%
    dplyr::filter(freq < count_ex) %>%
    pull(STUDI_ID)
  
  # Find students with less than 10 minutes
  time_index <- log_data %>%
    group_by(STUDI_ID) %>%
    slice(which.max(elapsed_time_t)) %>%
    dplyr::filter(elapsed_time_t <= time) %>%
    dplyr::pull(STUDI_ID)
  
  log_data %>%
    dplyr::filter(!STUDI_ID %in% unique(c(count_index, time_index)))
}

# Remove students that did not answer ~30% of questions and required less than 10 minutes
log_18_19 %<>% log_filter_fun(count_ex = 6, time = 10)
log_20_21 %<>% log_filter_fun(count_ex = 5, time = 10)

#---- Distance matrix ----
dist_mat_filter_fun <- function(log_data) {
  log_cl <- log_data %>% 
    dplyr::select(-elapsed_time_t)

  ex <- log_cl %>%
    distinct(exercise)
  
  E_X_fun <- function(data) {
    # Step 1: Filter
    data %<>% 
      arrange(elapsed_time_n)
  
    # Step 2: Window and Count
    window <- data %>% 
      group_by(exercise) %>%
      dplyr::mutate(bin = cut(elapsed_time_n, 
                              seq(0, 75, 1), 
                              right = F)) %>%
      dplyr::count(bin)
    
    window %$% 
      levels(bin) %>%
      tibble(window = .) %>%
      tidyr::expand_grid(ex) %>%
      dplyr::left_join(window, 
                       by = c("window" = "bin", "exercise")) %>%
      dplyr::mutate(
        n = dplyr::coalesce(n, 0)
      ) %>%
      dplyr::arrange(exercise) %>%
      dplyr::select(exercise, window, n)
  }

  log_cl %>%
    split(f = .$STUDI_ID) %>%
    purrr::map(E_X_fun)
  
}

E_X_18_19_filter <- dist_mat_filter_fun(log_18_19)
E_X_20_21_filter <- dist_mat_filter_fun(log_20_21)

# Similarity measure between each event
S_v_fun <- function(data, log_data) {
  metric_fun <- function(data, list) {
    data_v_1 <- data %>%
      purrr::chuck(list[[1]])
    
    data %<>% 
      purrr::chuck(list[[2]]) %>%
      dplyr::left_join(data_v_1, by = c("exercise", "window")) %>%
      dplyr::mutate(
        L_1 = abs(n.x - n.y), 
        L_2 = (n.x - n.y)^2
      )
    
    data %>% 
      group_by(exercise) %>%
      dplyr::summarise(
        L_1 = sum(L_1), 
        L_2 = sqrt(sum(L_2)), 
        .groups = "drop"
        ) %>%
      dplyr::mutate(
        v_1 = list[[1]], 
        v_2 = list[[2]],
        .before = exercise
        )
  }

  ID <- log_data %>%
    distinct(STUDI_ID)

  purrr::cross2(.x = ID$STUDI_ID, 
                       .y = ID$STUDI_ID) %>%
    purrr::map_dfr(~metric_fun(data, .))
}

S_v_18_19_filter <- S_v_fun(E_X_18_19_filter, log_18_19)
S_v_20_21_filter <- S_v_fun(E_X_20_21_filter, log_20_21)


# Normalise Attributes
normalise_S_v <- function(S_v) {
  S_v %>% 
    group_by(exercise) %>%
    dplyr::mutate(L_1 = (L_1 - min(L_1))/(max(L_1) - min(L_1)), 
                  L_2 = (L_2 - min(L_2))/(max(L_2) - min(L_2))) %>%
    ungroup()
}

S_v_18_19_filter %<>% normalise_S_v()
S_v_20_21_filter %<>% normalise_S_v()

# Add up to get a global measure
dist_mat_18_19_filter <- S_v_18_19_filter %>% 
    group_by(v_1, v_2) %>%
    dplyr::summarise(dist_L1 = weighted.mean(L_1, 
                                             c(rep(2/35, 16), rep(1/35, 3))), 
                     dist_L2 = weighted.mean(L_2, 
                                             c(rep(2/35, 16), rep(1/35, 3))), 
                     .groups = "drop")


dist_mat_20_21_filter <-  S_v_20_21_filter %>% 
    group_by(v_1, v_2) %>%
    dplyr::summarise(dist_L1 = weighted.mean(L_1, 
                                             c(rep(2/30, 13), rep(1/30, 4))), 
                     dist_L2 = weighted.mean(L_2, 
                                             c(rep(2/30, 13), rep(1/30, 4))), 
                     .groups = "drop")


rm(list = setdiff(ls(), c("S_v_18_19_filter", "S_v_20_21_filter", "dist_mat_18_19_filter", "dist_mat_20_21_filter")))
save(S_v_18_19_filter, S_v_20_21_filter, dist_mat_18_19_filter, dist_mat_20_21_filter, 
     file = here::here("00_data/Distmat_filtered.Rdata"))

#---- Clustering ----
load(here::here("00_data/Distmat_filtered.Rdata"))

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

# plot function
dendro_plot_fun <- function(object) {
  title <- deparse(substitute(object))
  object %>%
    ggdendro::ggdendrogram() +
    theme(axis.text.x = element_text(size = 7), 
          plot.title = element_text(hjust = 0.5, size = 15)) +
    ggtitle(title)
}

# L1
L1_18_19_complete_filter <- L1_cluster_fun(dist_mat = dist_mat_18_19_filter)
L1_20_21_complete_filter <- L1_cluster_fun(dist_mat = dist_mat_20_21_filter)

L1_18_19_single_filter <- L1_cluster_fun(dist_mat = dist_mat_18_19_filter, method = "single")
L1_20_21_single_filter <- L1_cluster_fun(dist_mat = dist_mat_20_21_filter, method = "single")

L1_18_19_av_filter <- L1_cluster_fun(dist_mat = dist_mat_18_19_filter, method = "average")
L1_20_21_av_filter <- L1_cluster_fun(dist_mat = dist_mat_20_21_filter, method = "average")

L1_18_19_ward_filter <- L1_cluster_fun(dist_mat = dist_mat_18_19_filter, method = "ward.D2")
L1_20_21_ward_filter <- L1_cluster_fun(dist_mat = dist_mat_20_21_filter, method = "ward.D2")

# L2
L2_18_19_complete_filter <- L2_cluster_fun(dist_mat = dist_mat_18_19_filter)
L2_20_21_complete_filter <- L2_cluster_fun(dist_mat = dist_mat_20_21_filter)

L2_18_19_single_filter <- L2_cluster_fun(dist_mat = dist_mat_18_19_filter, method = "single")
L2_20_21_single_filter <- L2_cluster_fun(dist_mat = dist_mat_20_21_filter, method = "single")

L2_18_19_av_filter <- L2_cluster_fun(dist_mat = dist_mat_18_19_filter, method = "average")
L2_20_21_av_filter <- L2_cluster_fun(dist_mat = dist_mat_20_21_filter, method = "average")

L2_18_19_ward_filter <- L2_cluster_fun(dist_mat = dist_mat_18_19_filter, method = "ward.D2")
L2_20_21_ward_filter <- L2_cluster_fun(dist_mat = dist_mat_20_21_filter, method = "ward.D2")

#---- Dendrogram ----
plot_all_fun_filter <- function(name, save.dendro = TRUE) {
  var_name <- name %>% 
    paste0("plot_", .)
  dendro <- dendro_plot_fun(get(name)) +
    ggtitle(name)
  assign(var_name, dendro, envir = .GlobalEnv)
  
  if(save.dendro) {
    ggsave(filename = paste0(
      here::here("04_plots/Dendrogram/filtered"), "/", var_name, ".pdf"), 
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
  purrr::map(plot_all_fun_filter)


 

