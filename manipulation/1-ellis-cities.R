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

# ---- load-data ---------------------------------------------------------------
library(googlesheets4)
googlesheets4::gs4_deauth() # to indicate there is no need for a access token
# By default we will work with public Sheets
# but see https://googlesheets4.tidyverse.org/ for how to set access to private
# URL https://docs.google.com/spreadsheets/d/1Ha5JmJRzV1e2ljN3lNLu-pZJwyehJPoKrw9CtMvBMks/edit?usp=sharing
sheet_name <- "1Ha5JmJRzV1e2ljN3lNLu-pZJwyehJPoKrw9CtMvBMks"
tab_name1 <- "За алфавітом"
tab_name2 <- "За населенням"
tab_name3 <- "За українцями"

population <- read_sheet(sheet_name,tab_name2,skip = 1)
ukrainians <- read_sheet(sheet_name,tab_name3,skip = 1)


founded <- 
  sheet_name %>% 
  read_sheet( 
    skip = 1
    ,sheet = tab_name1
    ,col_names = TRUE
  ) %>% 
  select(-1) %>% 
  mutate(
    across(names(.),as.character)
  )
names(founded) <- c("misto","oblast","pop_count","founded", "area_km2")

founded %>% slice(1:5) %>% select(1:4)

founded %>% readr::write_csv("./test.csv")

# library(formattable)
# 
# df <- data.frame(test = c("\u2265", "\u2264", "==", "equals", "!=", "\u2265=", "\u2264="))
# 
# formattable(df)

# ---- inspect-data ------------------------------------------------------------
founded %>% glimpse()
founded
founded %>% neat()
founded %>% knitr::kable(format = "latex")
# ---- tweak-data-founded --------------------------------------------------------------

ds0 <- readr::read_csv("data-public/raw/ukraine-cities.csv")

ds0 <- 
  founded %>%
  # test %>% 
  select(1:2) %>% slice(1:4) %>% 
  mutate(
    misto = str_remove(misto,"\\[.\\]")
    ,oblast = str_remove(oblast, " область")
    # ,misto2 = enc2utf8(misto)
    # ,misto2 = 
    ,misto = gsub("<U\\+([0-9A-F]{4})>", "&#x\\1;", misto)
    # ,misto = str_replace(misto, "<U\\+([0-9A-F]{4})>", "&#x\\1;")
    # ,misto = str_replace(misto, "\\<U\\+0410>", "XXXXXXX;")
    # ,oblast = gsub( "<U\\+([0-9A-F]{4})>", "&#x\\1;", x = oblast)
  #   ,count = pop_count %>% str_remove(" ") %>% as.integer()
  #   ,founded = founded %>% str_remove("\\[.\\]") %>% stringr::str_trim()
  #   ,centruy_founded = stringr
  #   ,pop_count = pop_count %>% str_remove(" ") %>% as.integer()
  )
ds0 %>% glimpse()
ds0 #%>% select(1:2) %>% slice(1:3)
ds0 %>% neat()

(a_string <- founded[1,1] %>% as.vector())
a_string %>% str_replace("\\<U\\+0410>","&#x\\1;")


d <- 
  ds0 %>% select(1:2) %>% slice(1:4)
d

ds0 %>% 
  group_by(oblast) %>% 
  count()


d <- 
  founded %>% 
  filter(founded = matches)



ds0 %>% readr::write_csv("./data-unshared/derived/ds0.csv")
ds1 <- readr::write_csv("./data-unshared/derived/ds0.csv")
# ---- table-1 -----------------------------------------------------------------


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
