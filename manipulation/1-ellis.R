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
library(ggplot2)   # graphs
library(janitor)   # tidy data
library(tidyr)     # data wrangling
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
library(readxl)    # data import
library(explore)   # for `describe_all()` 
library(scales)    # formatting
library(labelled)  # labels - https://cran.r-project.org/web/packages/labelled/vignettes/intro_labelled.html
library(rlang)     # tidy evaluations -  https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/   
# 
# ---- declare-globals ---------------------------------------------------------

# path_files should be the last element of the chunk
# ---- declare-functions -------------------------------------------------------
# store script-specific function here
# OuhscMunge::readr_spec_aligned("data-public/raw/data-input.csv")
col_types <- readr::cols_only(
  `event_n`       = readr::col_integer(),
  `signal_on`     = readr::col_datetime(format = ""),
  `signal_off`    = readr::col_datetime(format = "")
)

# ---- load-data ---------------------------------------------------------------
ds_event <- readr::read_csv("data-public/raw/data-input.csv", col_types = col_types)
# ds0 <- readr::read_csv("./data-public/raw/data-input.csv",col_select = 2:3)

# ---- inspect-data ------------------------------------------------------------
ds_event %>% glimpse()

# ---- tweak-data --------------------------------------------------------------
ds1 <- 
  ds_event %>% 
  mutate(
    event_n = row_number()
    ,duration = as.duration(signal_off - signal_on)
  ) %>% 
  tidyr::pivot_longer(
    cols = c("signal_on","signal_off")
    ,names_to = "signal_goes"
    ,values_to = "date_time"
  ) %>% 
  mutate(
    signal_goes = signal_goes %>% str_remove("^signal_")
    ,date = strftime(date_time,"%Y-%m-%d") %>% date()
    ,time = strftime(date_time,"%H:%M:%S") %>% hms::as_hms()
  )
ds1

# ---- alt-data -----------------------------------------------------------------
ds0 <- 
  read_csv(
  "data-unshared/raw/air-siren-vinnytsia-2022-03-23-2.csv"
  ,col_types = cols(
    signal_goes = col_factor(levels = c("on", "off"))
    ,time  = col_time(format = "%H:%M")
    ,date  = col_date(format = "%m/%d/%Y")
  )
  ) %>% 
  tidyr::fill(date) %>% 
  mutate(
    date_time = lubridate::ymd_hms(paste0(date," ",time))
  ) %>% 
  # arrange(date,time) %>%
  select(-time,-date) %>%
  mutate(
    event_n = (!(row_number() %% 2 == 0L)) %>% cumsum()
  ) %>% 
  tidyr::pivot_wider(
    id_cols = "event_n"
    ,names_from="signal_goes"
    ,values_from = "date_time"
    ,names_prefix = "signal_"
  )
ds0 %>% glimpse()
ds0 %>% slice(15:22)

ds1 <- 
  ds0 %>% 
  mutate(
    event_n = row_number()
    ,duration = as.duration(signal_off - signal_on)
  ) %>% 
  tidyr::pivot_longer(
    cols = c("signal_on","signal_off")
    ,names_to = "signal_goes"
    ,values_to = "date_time"
  ) %>% 
  mutate(
    signal_goes = signal_goes %>% str_remove("^signal_")
    ,date = strftime(date_time,"%Y-%m-%d") %>% date()
    ,time = strftime(date_time,"%H:%M:%S") %>% hms::as_hms()
  )
ds1

# ---- graph-1 -----------------------------------------------------------------


# ---- graph-2 -----------------------------------------------------------------

# ---- save-to-disk ------------------------------------------------------------
# naming convention: step_id - step_name - cohort_id
path_data_out <- "./data-unshared/derived/.../0-import-1.rds"
# ---- publish -----------------------------------------------------------------
# naming convention: step_id - data_transfer_object - cohort_id
# one report (.Rmd) per script (.R), unless report series
path_report_out <- "./manipulation/reports/0-import-1/0-import-1.Rmd"
rmarkdown::render(
  input = path_report_out ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
