rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. 
#This is not called by knitr, because it's above the first chunk.
# cat("/014") # Clear the console

# verify root location
# cat("Working directory: ", getwd()) # Must be set to Project Directory
# if the line above DOES NOT generates the project root, re-map by selecting
# Session --> Set Working Directory --> To Project Directory location
# Project Directory should be the root by default, unless overwritten

# ---- load-sources ------------------------------------------------------------
# source("./scripts/common-functions.R")# functions sourced throughout the project

# ---- load-packages -----------------------------------------------------------
# disable tho
# core packages - turn ON/OFF to help overview the scope of the script
# library(magrittr)  # pipes
# library(dplyr)     # data wrangling
library(ggplot2)   # graphs
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
# 
# ---- declare-globals ---------------------------------------------------------
config                         <- config::get()

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
# OuhscMunge::column_rename_headstart(ds)
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
  

ds_date_tally <-
  ds_event |> 
  # ds_date_sunrise |> 
  # # dplyr::left_join(ds_event, by = c("date" = "start_d")) |> 
  dplyr::group_by(start_d) |> 
  dplyr::summarize(
    event_tally_within_day = dplyr::n(),
  ) |> 
  dplyr::ungroup()

ds_date <-
  ds_date_sunrise |>
  dplyr::left_join(ds_date_tally, by = c("date" = "start_d")) |> 
  dplyr::mutate(
    date_index       = 0L + as.integer(difftime(date, min(date), units = "days")),
  ) |>
  dplyr::mutate(
    event_tally_within_day = dplyr::if_else(1L <= date_index, dplyr::coalesce(event_tally_within_day, 0L), NA_integer_)
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

# https://www.christies.com/en/lot/lot-5388667
# http://colrd.com/palette/19057/
palette_solid <- list(
  night        = "#0c2c84",
  astronomical = "#225ea8", #  "#A1B5B9",
  nautical     = "#1d91c0",
  civil        = "#7fcdbb",
  day          = "#ffffd9",
  signal       = "#660000"  # https://colorswatches.info/color/blood-red
)

palette_faint <- as.list(scales::alpha(palette_solid, alpha = .8))

# ds_event |>
ds_event_within_day |>
  # dplyr::select(start_d, start_t, stop_t) |> 
  # dplyr::filter(date == as.Date("2022-03-10")) |> 
  # dplyr::slice(1:2) |>
  ggplot(aes(x = start_d)) +
  geom_ribbon(data = ds_date, aes(x = date, ymin = hms::as_hms("00:00:00"), ymax = start_astronomical     ), fill = palette_solid$night       , color = NA) +
  geom_ribbon(data = ds_date, aes(x = date, ymin = start_astronomical     , ymax = start_nautical         ), fill = palette_solid$astronomical, color = NA) +
  geom_ribbon(data = ds_date, aes(x = date, ymin = start_nautical         , ymax = start_civil            ), fill = palette_solid$nautical    , color = NA) +
  geom_ribbon(data = ds_date, aes(x = date, ymin = start_civil            , ymax = sunrise                ), fill = palette_solid$civil       , color = NA) +
  geom_ribbon(data = ds_date, aes(x = date, ymin = sunrise                , ymax = sunset                 ), fill = palette_solid$day         , color = NA) +
  geom_ribbon(data = ds_date, aes(x = date, ymin = sunset                 , ymax = stop_civil             ), fill = palette_solid$civil       , color = NA) +
  geom_ribbon(data = ds_date, aes(x = date, ymin = stop_civil             , ymax = stop_nautical          ), fill = palette_solid$nautical    , color = NA) +
  geom_ribbon(data = ds_date, aes(x = date, ymin = stop_nautical          , ymax = stop_astronomical      ), fill = palette_solid$astronomical, color = NA) +
  geom_ribbon(data = ds_date, aes(x = date, ymin = stop_astronomical      , ymax = hms::as_hms("24:00:00")), fill = palette_solid$night      , color = NA) +
  geom_line(  data = ds_date, aes(x = date, y = zenith), color = "gold", linetype = "a3", size = 1) +
  
  geom_rect(
    aes(xmin = start_d - .5, xmax = start_d + .5, ymin = start_t, ymax = stop_t),
    # color = "#bbbbbbbb", fill = "#88888888",
    color = palette_solid$signal, fill = palette_faint$signal,
    size = .15
  ) +
  # geom_hline(yintercept = hms::as_hms("23:59:59"), linetype = "44", color = "#bbbbbbbb") +
  geom_text(data = ds_date, aes(x = date, label = date_index), y = -Inf, hjust = .01, srt = 90, size = 2, na.rm = T) +
  geom_text(data = ds_date, aes(x = date, label = event_tally_within_day, color = event_tally_within_day), y = Inf, vjust = 1.01, na.rm = T) +
  scale_x_date(date_labels = "%b\n%d") +
  scale_y_time(
    breaks = hms::as_hms(c("00:00:00", "08:00:00", "12:00:00", "16:00:00", "24:00:00")),
    labels = c("midnight", "8am", "noon", "4pm", "midnight")
  ) +
  # scale_color_brewer(type = "seq", palette = "YlOrRd") +
  # scale_color_continuous(type = "viridis") +
  scale_colour_viridis_b(direction = -1) +
  coord_cartesian(ylim = hms::parse_hms(c("00:00:00", "23:59:59"))) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    x = NULL,
    y = "Time of Day"
  )

ggsave(
  filename = "manipulation/sandbox/cyclogram.png",
  height   = 4,
  width    = 5,
  dpi      = 400,
  bg       = "white"
)
