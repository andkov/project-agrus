rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console

# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# if the line above DOES NOT generates the project root, re-map by selecting
# Session --> Set Working Directory --> To Project Directory location
# Project Directory should be the root by default unless overwritten

# ---- load-packages -----------------------------------------------------------
library(magrittr)  # pipes
library(dplyr)     # data wrangling
library(ggplot2)   # graphs
library(janitor)   # tidy data
library(tidyr)     # data wrangling
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates

# ---- load-sources ------------------------------------------------------------
source("./scripts/common-functions.R")
# ---- declare-globals ---------------------------------------------------------

# ---- declare-functions -------------------------------------------------------
# custom function for HTML tables

prints_folder <- paste0("./analysis/report-2/prints/")
if(!file.exists(prints_folder)){
  dir.create(file.path(prints_folder))
}


# ---- load-data ---------------------------------------------------------------
ds0 <- readr::read_csv("./data-public/raw/data-input.csv",col_select = 2:3)

# ---- inspect-data ------------------------------------------------------------
ds0 %>% glimpse()

# ---- tweak-data --------------------------------------------------------------
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
ds2 <- 
  ds1 %>% 
  group_by(date) %>% 
  summarize(
    
  )
# ---- table-1 -----------------------------------------------------------------


# ---- graph-1 -----------------------------------------------------------------
g1 <-
  ds1 %>% 
  ggplot(aes(x=time,y=date))+
  geom_point(shape=124)+
  scale_y_date(breaks = "1 day")
g1
# ---- graph-2 -----------------------------------------------------------------

# ---- save-to-disk ------------------------------------------------------------
path <- "./analysis/.../report-isolated.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
