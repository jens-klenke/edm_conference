# Load Data and Packages
source(here::here('01_code/packages.R'))

#---- Create points df ----

# Function
points_fun <- function(data) {
  # Filter R exercises
  data_06 <- data %>%
    dplyr::filter(str_detect(`Aufgabe:`, "6"))
  
  data %<>%
    dplyr::filter(!str_detect(`Aufgabe:`, "6") & !str_detect(`Aufgabe:`, "7"))
  
  data %<>% 
    dplyr::select(-c(`Gesamtpunktzahl:`, 
                     `#SolutionDatas:`,
                     starts_with("H"))) %>%
    pivot_longer(cols = starts_with("Stufe"), 
                 names_to = "stage", 
                 values_to = "points")
  
  data %<>%
    dplyr::mutate(
      exercise = paste(
        str_sub(`Aufgabe:`, -1, -1),
        str_sub(stage, -1, -1),
        sep = "."
      ), 
     points = dplyr::coalesce(points, 0)) %>%
    dplyr::rename("STUDI_ID" = `MatrikelNr.:`) %>%
    dplyr::select(STUDI_ID, points, exercise)

  data_06 %<>%
    dplyr::mutate(exercise = paste0(
      str_sub(`Aufgabe:`, -2), 
      ".1"
    ))  %>%
    dplyr::rename("STUDI_ID" = `MatrikelNr.:`, 
                  "points" = `Gesamtpunktzahl:`) %>%
    dplyr::select(STUDI_ID, points, exercise)
  

  bind_rows(data, data_06)
}

# 18_19
points_18_19 <- readxl::read_xlsx("00_data/PunktelisteNT1819.xlsx", 
                                  sheet = 1)
points_18_19 %<>% points_fun()


# 20_21 
# load points without ex 7
points_20_21 <- bind_rows(
  readxl::read_xls("00_data/Ergebnisse_Teilpunkte/statistische_uebersicht07.xls", 
                   sheet = 4),
  readxl::read_xls("00_data/Ergebnisse_Teilpunkte/statistische_uebersicht25.xls", 
                   sheet = 4), 
  readxl::read_xls("00_data/Ergebnisse_Teilpunkte/statistische_uebersicht28.xls", 
                   sheet = 4), 
  readxl::read_xls("00_data/Ergebnisse_Teilpunkte/statistische_uebersicht29.xls", 
                   sheet = 4)
)

points_20_21 %<>% 
  points_fun()

# load points of ex 7
points_20_21_ex7 <- bind_rows(
  readxl::read_xlsx("00_data/Ergebnisse_Teilpunkte/Teilpunkte/Aufgabe7_Ergebnisse07.xlsx"), 
  readxl::read_xlsx("00_data/Ergebnisse_Teilpunkte/Teilpunkte/Aufgabe7_Ergebnisse25.xlsx"), 
  readxl::read_xlsx("00_data/Ergebnisse_Teilpunkte/Teilpunkte/Aufgabe7_Ergebnisse28.xlsx"),
  readxl::read_xlsx("00_data/Ergebnisse_Teilpunkte/Teilpunkte/Aufgabe7_Ergebnisse29.xlsx")
)

points_20_21_ex7 %<>%
  dplyr::rename("STUDI_ID" = `Matrikelnummer`, 
                "points" = Aufgabe7) %>%
  dplyr::mutate(exercise = "7.1")

# merge points
points_20_21 %<>% bind_rows(points_20_21_ex7)
rm(points_20_21_ex7)

points_18_19 %<>%
  dplyr::mutate(STUDI_ID = as.character(STUDI_ID))

points_20_21  %<>%
  dplyr::mutate(STUDI_ID = as.character(STUDI_ID), 
                points = dplyr::recode(points, "-1" = 0)
                )


#---- Merge with log data ----
load(here::here("00_data/Distmat.Rdata"))

# Add points to Distance matrix
points_fun <- function(data, point_data) {
  data_points <- data %>%
    dplyr::left_join(point_data, 
                     by = c("v_1" = "STUDI_ID", "exercise")) %>%
    dplyr::rename("points_v_1" = points) %>%
    dplyr::mutate(points_v_1 = dplyr::coalesce(points_v_1, 0))
 
   data_points %<>%
    dplyr::left_join(point_data, 
                     by = c("v_2" = "STUDI_ID", "exercise")) %>%
    dplyr::rename("points_v_2" = points) %>%
     dplyr::mutate(points_v_2 = dplyr::coalesce(points_v_2, 0))
   
   data_points %>%
     dplyr::mutate(
       points_L = abs(points_v_1 - points_v_2) 
     ) %>%
     dplyr::select(-c(points_v_1, points_v_2))
}

S_v_18_19_points <- S_v_18_19 %>% points_fun(points_18_19)
S_v_20_21_points <- S_v_20_21 %>% points_fun(points_20_21)

# Normalise points (time already normalised)
normalise_S_v_points <- function(S_v) {
  S_v %>% 
    group_by(exercise) %>%
    dplyr::mutate(points_L = (points_L - min(points_L))/(max(points_L) - min(points_L))) %>%
    ungroup()
}

S_v_18_19_points %<>% normalise_S_v_points()  
S_v_20_21_points %<>% normalise_S_v_points()


# Add up to get a global measure
dist_mat_global_time_points_18_19 <-  S_v_18_19_points %>%   
    pivot_longer(cols = c(points_L, L_1, L_2)) %>%
    group_by(v_1, v_2) %>%
  plyr::arrange(v_1, v_2, name) %>%
  dplyr::summarise(dist_L1 = weighted.mean(value[name != "L_2"], 
                                             c(rep(4/89, 16), rep(2/89, 3), rep(1/89, 19))), 
                     dist_L2 = weighted.mean(value[name != "L_1"], 
                                             c(rep(4/89, 16), rep(2/89, 3), rep(1/89, 19))), 
                     .groups = "drop")

dist_mat_global_time_points_20_21 <-  S_v_20_21_points %>%   
  pivot_longer(cols = c(points_L, L_1, L_2)) %>%
  group_by(v_1, v_2) %>%
  plyr::arrange(v_1, v_2, name) %>%
  dplyr::summarise(dist_L1 = weighted.mean(value[name != "L_2"], 
                                           c(rep(4/77, 13), rep(2/77, 4), rep(1/77, 17))), 
                   dist_L2 = weighted.mean(value[name != "L_1"], 
                                           c(rep(4/77, 13), rep(2/77, 4), rep(1/77, 17))), 
                   .groups = "drop")

rm(list = setdiff(ls(), c("S_v_18_19_points", "S_v_20_21_points",
                          "dist_mat_global_time_points_18_19", "dist_mat_global_time_points_20_21")))
save(S_v_18_19_points, S_v_20_21_points,
     dist_mat_global_time_points_18_19, dist_mat_global_time_points_20_21,
     file = here::here("00_data/Distmat_time_points.Rdata"))

#---- Clustering ----
load(here::here("00_data/Distmat_time_points.Rdata"))

# cluster functions
L1_cluster_sum_fun <- function(dist_mat, method = "complete") {
  dist_mat_L1 <- dist_mat %>% 
    pivot_wider(names_from = v_2, 
                values_from = dist_L1, 
                id_cols = v_1) %>%
    dplyr::select(-v_1) %>%
    as.dist()
  
  hclust(dist_mat_L1, method = method)
}

L2_cluster_sum_fun <- function(dist_mat, method = "complete") {
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
L1_18_19_complete_points <- L1_cluster_sum_fun(dist_mat = dist_mat_global_time_points_18_19)
L1_20_21_complete_points <- L1_cluster_sum_fun(dist_mat = dist_mat_global_time_points_20_21)

L1_18_19_single_points <- L1_cluster_sum_fun(dist_mat = dist_mat_global_time_points_18_19, method = "single")
L1_20_21_single_points <- L1_cluster_sum_fun(dist_mat = dist_mat_global_time_points_20_21, method = "single")

L1_18_19_av_points <- L1_cluster_sum_fun(dist_mat = dist_mat_global_time_points_18_19, method = "average")
L1_20_21_av_points <- L1_cluster_sum_fun(dist_mat = dist_mat_global_time_points_20_21, method = "average")

L1_18_19_ward_points <- L1_cluster_sum_fun(dist_mat = dist_mat_global_time_points_18_19, method = "ward.D2")
L1_20_21_ward_points <- L1_cluster_sum_fun(dist_mat = dist_mat_global_time_points_20_21, method = "ward.D2")

# L2
L2_18_19_complete_points <- L2_cluster_sum_fun(dist_mat = dist_mat_global_time_points_18_19)
L2_20_21_complete_points <- L2_cluster_sum_fun(dist_mat = dist_mat_global_time_points_20_21)

L2_18_19_single_points <- L2_cluster_sum_fun(dist_mat = dist_mat_global_time_points_18_19, method = "single")
L2_20_21_single_points <- L2_cluster_sum_fun(dist_mat = dist_mat_global_time_points_20_21, method = "single")

L2_18_19_av_points <- L2_cluster_sum_fun(dist_mat = dist_mat_global_time_points_18_19, method = "average")
L2_20_21_av_points <- L2_cluster_sum_fun(dist_mat = dist_mat_global_time_points_20_21, method = "average")

L2_18_19_ward_points <- L2_cluster_sum_fun(dist_mat = dist_mat_global_time_points_18_19, method = "ward.D2")
L2_20_21_ward_points <- L2_cluster_sum_fun(dist_mat = dist_mat_global_time_points_20_21, method = "ward.D2")

#---- Dendrogram ----
plot_all_fun_points <- function(name, save.dendro = TRUE) {
  var_name <- name %>% 
    paste0("plot_", .)
  dendro <- dendro_plot_fun(get(name)) +
    ggtitle(name)
  assign(var_name, dendro, envir = .GlobalEnv)
  
  if(save.dendro) {
    ggsave(filename = paste0(
      here::here("04_plots/Dendrogram/time_points"), "/", var_name, ".pdf"), 
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




