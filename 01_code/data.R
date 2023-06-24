#-- loading packages ----
source(here::here('01_code/packages.R'))

##-- loading data ----
# log Data of the different exams
log_18_19 <- readr::read_csv2('00_data/eventLog_18-19.csv')
log_20_21 <- readxl::read_xlsx('00_data/eventLog_20-21_alle_Server.xlsx')


# overview data to extract the start of the exam for each student individually
overview_20_21_S_25 <- readr::read_delim('00_data/statistische_uebersicht_20-21_S_25.csv', 
                                         delim = ";", col_types = "c")
overview_20_21_S_07 <- readr::read_delim('00_data/statistische_uebersicht_20-21_S_07.csv', 
                                         delim = ";", col_types = "c")
overview_20_21_S_28 <- readxl::read_xls('00_data/statistische_uebersicht_20-21_S_28.xls', 
                                        col_types = "text")
overview_20_21_S_29 <- readxl::read_xls('00_data/statistische_uebersicht_20-21_S_29.xls', 
                                        col_types = "text")
overview_18_19 <- readr::read_delim('00_data/statistische_uebersicht_18-19.csv', 
                                    delim = ";", col_types = "c")


#-- cleaning data ----
log_18_19 %<>%
  # format date and time
  dplyr::mutate(timestamp = lubridate::dmy_hms(timestamp)) %>%
  # dealing with NA in stage (r exercise have no stage, but we assign 1)
  dplyr::mutate(stage = replace_na(stage, 1)) %>%
  # combining exercise and stages of the exercise to get unique identifier 
  dplyr::mutate(exercise = stringr::str_c(str_remove_all(task, pattern = paste(c('Aufgabe_', '_solution_export', '_', 'Text', '-'), collapse = "|"))
                                          , stage, sep = '.')) 

# getting start time from Overview
overview_18_19 %<>%
  # rename to match other database
  dplyr::rename(STUDI_ID = `MatrikelNr.:`) %>%
  # combining date and time to one variable
  tidyr::unite(start, c('Einreichungsdatum:', 'Einreichungszeitpunkt:')) %>%
  # format time properly
  dplyr::mutate(start = lubridate::dmy_hms(start))

log_18_19 %<>%
  # merge via left_join by STUDI_ID (Matrikelnummer)
  dplyr::left_join(overview_18_19, by = 'STUDI_ID') %>%
  # filtering admin accounts!! needs to be changed !! 
  dplyr::filter(!STUDI_ID %in%  c('statistikteam', 'benjamin.otto')) %>%
  group_by(STUDI_ID) %>%
  dplyr::mutate(start_t = min(timestamp),
                start_n = lubridate::dmy_hms("23.3.2019 10:46:50")
                ) %>%
  dplyr::ungroup()


log_20_21 %<>%
  # dealing with NA in stage (r exercise have no stage, but we assign 1)
  dplyr::mutate(stage = replace_na(stage, 1)) %>%
  # combining exercise and stages of the exercise to get unique identifier 
  dplyr::mutate(exercise = stringr::str_c(str_remove_all(task, pattern = paste(c('Aufgabe_', '_solution_export', '_', 'Text', '-'), collapse = "|"))
                                          , stage, sep = '.'), 
                STUDI_ID = as.character(STUDI_ID)) 

# combining the different Server Overviews of NT-20-21
overview_20_21_S_07 %<>%
  dplyr::mutate(`Einreichungszeitpunkt:` = as.character(`Einreichungszeitpunkt:`))

overview_20_21_S_25 %<>%
  dplyr::mutate(`Einreichungszeitpunkt:` = as.character(`Einreichungszeitpunkt:`))

overview_20_21 <- bind_rows(
  overview_20_21_S_07,
  overview_20_21_S_25, 
  overview_20_21_S_28, 
  overview_20_21_S_29
)

rm(overview_20_21_S_07, 
   overview_20_21_S_25, 
   overview_20_21_S_28, 
   overview_20_21_S_29)

# getting the start of the exam for each person
overview_20_21 %<>%
  # rename to match other database
  dplyr::rename(STUDI_ID = `MatrikelNr.:`) %>%
  # combining date and time to one variable
  tidyr::unite(start, c('Einreichungsdatum:', 'Einreichungszeitpunkt:')) %>%
  # format time properly
  dplyr::mutate(start = lubridate::dmy_hms(start))

# combining start time and log
log_20_21 %<>%
  left_join(overview_20_21, by = 'STUDI_ID') %>%
  # extract Date 
  dplyr::mutate(date = as.Date(start)) %>%
  # exclude with wrong starting time, because their restarted the exam 
  group_by(STUDI_ID) %>%
  dplyr::mutate(start_t = min(timestamp),
                start_n = lubridate::dmy_hms("09.04.2021 14:30:00")
  ) %>%
  dplyr::ungroup()

#-- Remove students with server problems ----
log_20_21 %<>% 
  dplyr::filter(!STUDI_ID %in% c("3098427",
                                 "3710303",
                                 "3098170", 
                                 "3066468",
                                 "3068542",
                                 "3111673", 
                                 "3055579",
                                 "3094682", 
                                 "3097028", 
                                 "3097022", 
                                 "3112458", 
                                 "3109618"))

#-- Remove start variable and calculate the duration for each exercise----
log_20_21 %<>% 
  dplyr::select(-start) %>%
  dplyr::mutate(elapsed_time_t = as.duration(interval(start_t, timestamp)), 
                elapsed_time_n = as.duration(interval(start_n, timestamp)))

log_18_19 %<>% 
  dplyr::select(-start) %>%
  dplyr::mutate(elapsed_time_t = as.duration(interval(start_t, timestamp)), 
                elapsed_time_n = as.duration(interval(start_n, timestamp))) 

save(log_18_19, log_20_21, 
     file = here::here("00_data/log_files.Rdata"))
