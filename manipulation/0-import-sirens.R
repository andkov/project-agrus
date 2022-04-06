rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. 
#This is not called by knitr, because it's above the first chunk.
cat("/014") # Clear the console

# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# if the line above DOES NOT generates the project root, re-map by selecting
# Session --> Set Working Directory --> To Project Directory location
# Project Directory should be the root by default, unless overwritten

# ---- load-sources ------------------------------------------------------------
source("./scripts/common-functions.R")# functions sourced throughout the project

# ---- load-packages -----------------------------------------------------------
# disable tho
# core packages - turn ON/OFF to help overview the scope of the script
library(magrittr)  # pipes
library(dplyr)     # data wrangling
# library(ggplot2)   # graphs
# library(janitor)   # tidy data
# library(tidyr)     # data wrangling
# library(forcats)   # factors
# library(stringr)   # strings
# library(lubridate) # dates
# library(readxl)    # data import
# library(explore)   # for `describe_all()` 
# library(scales)    # formatting
# library(labelled)  # labels - https://cran.r-project.org/web/packages/labelled/vignettes/intro_labelled.html
# library(rlang)     # tidy evaluations -  https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/   
# library(readr)

# ---- declare-globals ---------------------------------------------------------
# path_files should be the last element of the chunk
# ---- declare-functions -------------------------------------------------------
# store script-specific function here

# ---- load-data ---------------------------------------------------------------
library(googlesheets4)
googlesheets4::gs4_deauth() # to indicate there is no need for a access token
# By default we will work with public Sheets
# but see https://googlesheets4.tidyverse.org/ for how to set access to private
# URL https://docs.google.com/spreadsheets/d/1_YLy7BEP3fSUGlZeVxtybfFRIxVNswXUa0oEOVrOIJ8/edit#gid=200032068
sheet_name <- "1_YLy7BEP3fSUGlZeVxtybfFRIxVNswXUa0oEOVrOIJ8"
tab_name1 <- "source"

c("%m/%d/%Y", rep("%H:%M",17))
ds0 <- 
  read_sheet(
    sheet_name
    ,tab_name1
    ,skip = 1
    ,col_names = TRUE
    # ,col_types =c("%m/%d/%Y", rep("%H:%M",17))
   ) %>% 
  select(1,seq(2,16,2)) #%>% slice(1:7)
ds0 %>% glimpse()
hours <- c("00_03",	"03_06",	"06_09",	"09_12",	"12_15"	,"15_18",	"18_21",	"21_24")

# ---- inspect-data ------------------------------------------------------------


# ---- tweak-data --------------------------------------------------------------
ds1 <- 
  ds0 %>% 
  tidyr::fill(date) %>% 
  # filter(date %in% as.Date(c("2022-02-27","2022-02-28"))) %>% 
  # group_by(date) %>% 
  tidyr::pivot_longer(2:9,names_to = "hour", values_to = "time") %>% 
  arrange(date, hour, time) %>% 
  filter(!is.na(time)) %>% 
  mutate(
    time = hms::as_hms(time)#+hms::as_hms("07:00:00")
  )
ds1


ds2 <-
  ds1 %>% 
  mutate(
    signal = (!(row_number() %% 2 == 0L))
    ,date_time = lubridate::ymd_hms(paste0(date," ",time))
  ) %>% 
  select(date_time,signal) %>% 
  # group_by(date) %>% 
  mutate(
    event_n = cumsum(signal)
  ) %>% 
  mutate(
    signal = case_when(
      signal == TRUE ~ "signal_on"
      ,signal == FALSE ~ "signal_off"
      ,TRUE ~ NA_character_
    )
    # ,date_time = as.character(date_time)
    # ,date_time2 = strftime(date_time,format = "%Y-%m-%d %H:%M")
  ) %>% 
  tidyr::pivot_wider(names_from = "signal", values_from = "date_time") 
ds2

ds2 %>%
  arrange(desc(event_n)) %>% 
  readr::write_csv(file = "./data-public/raw/data-input.csv")

ds3 <- 
  readr::read_csv("./data-public/raw/data-input.csv")
ds3 %>% glimpse()


# ---- table-1 -----------------------------------------------------------------


# ---- graph-1 -----------------------------------------------------------------


# ---- graph-2 -----------------------------------------------------------------

# # ---- save-to-disk ------------------------------------------------------------
# # naming convention: step_id - step_name - cohort_id
# path_data_out <- "./data-unshared/derived/.../0-import-1.rds"
# # ---- publish -----------------------------------------------------------------
# # naming convention: step_id - data_transfer_object - cohort_id
# # one report (.Rmd) per script (.R), unless report series
# path_report_out <- "./manipulation/reports/0-import-1/0-import-1.Rmd"
# rmarkdown::render(
#   input = path_report_out ,
#   output_format=c(
#     "html_document"
#     # "word_document"
#     # "pdf_document"
#   ),
#   clean=TRUE
# )
