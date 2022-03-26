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
config                         <- config::get()

# ---- declare-functions -------------------------------------------------------
# custom function for HTML tables

prints_folder <- paste0("./analysis/report-1/prints/")
if(!file.exists(prints_folder)){
  dir.create(file.path(prints_folder))
}

# store script-specific function here
# OuhscMunge::readr_spec_aligned("data-public/raw/data-input.csv")
col_types <- readr::cols_only(
  `event_n`       = readr::col_integer(),
  `signal_on`     = readr::col_datetime(format = ""),
  `signal_off`    = readr::col_datetime(format = "")
)

# ---- load-data ---------------------------------------------------------------
ds_event <- readr::read_csv("data-public/raw/data-input.csv", col_types = col_types)
rm(col_types)

ds_date_sunrise <- readr::read_rds(config$path_derived_sunrise_rds)

# ds |> dplyr::glimpse()

# ---- tweak-data --------------------------------------------------------------
ds_event <- 
  ds_event |> 
  dplyr::select(    # `dplyr::select()` drops columns not included.
    event_id           = `event_n`,
    start_dt           = `signal_on`,
    stop_dt            = `signal_off`,
  ) |> 
  dplyr::mutate(
    duration_minutes = as.integer(difftime(stop_dt, start_dt, units = "mins")),
    start_d          = as.Date(start_dt),
    stop_d           = as.Date(stop_dt),
    start_t          = hms::as_hms(start_dt),
    stop_t           = hms::as_hms(stop_dt),
  ) 

date_range <- range(ds_event$start_d, ds_event$stop_d) 

ds_date_sunrise <-
  ds_date_sunrise |> 
  dplyr::filter(dplyr::between(date, date_range[1] - 1L, date_range[2] + 1L))


ds_day <- 
  ds_event |> 
  dplyr::group_by(start_d) |> 
  dplyr::summarize(
    event_tally_within_day = dplyr::n()
    ,duration_total_day = sum(duration_minutes)
    ,duration_mean_day = mean(duration_minutes)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    date_index       = 1L +as.integer(difftime(start_d, min(start_d), units = "days")),
  )

ds_event_within_day <-
  ds_event |> 
  dplyr::select(
    event_id,
    start_dt,
    stop_dt,
  ) |> 
  dplyr::mutate(
    # crosses_midnight   = as.Date(start_d) < as.Date(stop_d),
    days   = 1L + as.integer(difftime(as.Date(stop_dt), as.Date(start_dt), units = "days")),
  ) |> 
  tidyr::uncount(days, .remove = FALSE, .id = "day_within_event") |> 
  dplyr::mutate(
    # day_within_event_rev = days - day_within_event + 1L,
    day_first = (day_within_event == 1L),
    day_last  = (day_within_event == days),
  ) |> 
  dplyr::mutate(
    start_dt  = dplyr::if_else(day_first, start_dt, as.Date(stop_dt) + lubridate::seconds(1)),
    stop_dt   = dplyr::if_else(day_last , stop_dt , as.Date(stop_dt) - lubridate::seconds(1))
  ) |> 
  dplyr::mutate(
    start_d     = as.Date(start_dt),
    start_t     = hms::as_hms(start_dt),
    stop_t      = hms::as_hms(stop_dt),
  )

ds_event_within_day


# ---- graph-1 -----------------------------------------------------------------
g1 <- 
  ds_event_within_day |>
  # dplyr::select(start_d, start_t, stop_t) |> 
  # dplyr::filter(date == as.Date("2022-03-10")) |> 
  # dplyr::slice(1:2) |>
  ggplot(aes(x = start_d)) +
  geom_ribbon(data = ds_date_sunrise, aes(x = date, ymax = sunrise, ymin = hms::as_hms("00:00:00"))) +
  geom_ribbon(data = ds_date_sunrise, aes(x = date, ymin = sunset , ymax = hms::as_hms("24:00:00"))) +
  geom_rect(
    aes(xmin = start_d - .5, xmax = start_d + .5, ymin = start_t, ymax = stop_t),
    color = "#bbbbbbbb", fill = "#88888888"
  ) +
  geom_hline(yintercept = hms::as_hms("23:59:59"), linetype = "44", color = "#bbbbbbbb") +
  geom_text(data = ds_day, aes(label = date_index), y = -Inf, hjust = .01, srt = 90, size = 3) +
  geom_text(data = ds_day, aes(label = event_tally_within_day), y = Inf, vjust = 1.01) +
  geom_text(label = "Доба\nспротиву",x =-Inf, y = -Inf, vjust = -.2, hjust = 0, size = 3)+
  geom_text(label = "Кількість\nтривог",x =-Inf, y = Inf,vjust = 1, hjust = 0, size = 3)+
  scale_y_time(breaks = ) +
  coord_cartesian(ylim = hms::parse_hms(c("00:00:00", "23:59:59"))) +
  theme_minimal() +
  labs(
    title = "Тривалисть та частота повітряних тривог у м.Вінниця"
    ,caption = "1)Понеділки позначені датами\ncreated by @andkov and @wibeasley"
    ,x = NULL
    ,y = "Година доби"
  )

g1
g1 %>% quick_save("1-cyclogram",width=9, height=5)


# ---- graph-2 -----------------------------------------------------------------
d2 <- 
  ds_day %>% 
  tidyr::pivot_longer(
    cols = c("event_tally_within_day","duration_total_day","duration_mean_day" )
    ,names_to = "measure"
    ,values_to = "value"
  ) %>% 
  mutate(
    measure = case_when(
      measure == "event_tally_within_day" ~ "count"
      ,measure == "duration_total_day" ~ "total_mins"
      ,measure == "duration_mean_day" ~ "mean_mins"
      
    )
  )
g2 <-
  d2 %>% 
  ggplot(aes(x=start_d, y = value))+
  geom_line()+
  geom_point()+
  # geom_text(aes(label = date_index), y = -Inf, vjust = -.01) +
  # geom_text( aes(label = round(value,0)), y = Inf,xjust = 2.11, angle = -90) +
  # scale_y_continuous(expand = expansion(mult = c(0,lk1.1)))+
  geom_smooth()+
  scale_x_date(date_breaks = "1 week", minor_breaks = "1 day",date_labels = "%b-%d")+
  facet_wrap(
    facets = "measure"
    , scales = "free"
    ,labeller = labeller(measure = c(
      "count" = "Кількість","mean_mins" = "Середня тривалість","total_mins" = "Сукупна тривалість"
    ))
  )+
  labs(
    title = "Статистика повітряних тривог у місті Вінниця"
    ,subtitle = "Зведення за кожну добу (0-24)"
    ,caption = "1)Понеділки позначені датами 2) Тривога зараховується до доби у яку почалась\n created by @andkov and @wibeasley"
    ,x = NULL
    ,y = "Значення"
  )
g2
g2 %>% quick_save("2-count-duration",width=12, height =4)
  
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
