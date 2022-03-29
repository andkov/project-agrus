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

ds_date_tally <-
  ds_event |> 
  # ds_date_sunrise |> 
  # # dplyr::left_join(ds_event, by = c("date" = "start_d")) |> 
  dplyr::group_by(start_d) |> 
  dplyr::summarize(
    event_tally_within_day = dplyr::n()
    ,duration_total_day = sum(duration_minutes)
    ,duration_mean_day = mean(duration_minutes)
  )|> 
  dplyr::ungroup()

ds_date <-
  ds_date_sunrise |>
  dplyr::left_join(ds_date_tally, by = c("date" = "start_d")) |> 
  dplyr::mutate(
    date_index       = 0L + as.integer(difftime(date, min(date), units = "days")),
    date_index_rev   = 0L + as.integer(difftime(max(date), date, units = "days")),
    date_of_month    = strftime(date, "%d") %>% as.integer(),
    month_name       = case_when(
      date == as.Date("2022-02-28") ~ "Лютий       ",
      date == as.Date("2022-03-02") ~ "Березень   ",
      TRUE ~ " "
    ),
    date_display     = sprintf("%2i/%2i", date_index, date_index_rev),
    # date_display     = sprintf("%2i\n%2i", date_index, date_index_rev),
    # date_display     = sprintf("%2s\n%2s\n%s", as.character(date_index), as.character(date_of_month),month_name),
    date_display     = sprintf("%2i", date_index),
    date_display     =
      dplyr::case_when(
        date_index      == 0  ~ NA_character_,
        date_index_rev  == 0  ~ NA_character_,
        TRUE                  ~ date_display
      ),
    
    date_display2     = sprintf("%2s\n%s",as.character(date_of_month),month_name),
    date_display2     =
      dplyr::case_when(
        date_index      == 0  ~ NA_character_,
        date_index_rev  == 0  ~ NA_character_,
        TRUE                  ~ date_display2
      )
    
    
  ) |>
  dplyr::mutate(
    event_tally_within_day = dplyr::if_else(1L <= date_index & 1L <= date_index_rev, dplyr::coalesce(event_tally_within_day, 0L), NA_integer_)
  )
ds_date %>% glimpse()
ds_date$date_display

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
# https://www.christies.com/en/lot/lot-5388667
# http://colrd.com/palette/19057/
# palette_solid <- list(
#   night        = "#0c2c84",
#   astronomical = "#225ea8", #  "#A1B5B9",
#   nautical     = "#1d91c0",
#   civil        = "#7fcdbb",
#   day          = "#ffffd9",
#   signal       = "#660000",  # https://colorswatches.info/color/blood-red
#   boundary     = "gray20",
#   zenith       = "gold"
# )
ukraine_blue <-   "#0057b7"
ukraine_yellow <- "#ffd700"



palette_solid <- list(
  night        = ukraine_blue,
  # astronomical = scales::alpha(ukraine_blue, alpha = .5),
  astronomical = colorspace::lighten(ukraine_blue,  amount = .12,space = "HLS"),
  nautical     =  colorspace::lighten(ukraine_blue, amount = .24,space = "HLS"),
  civil        =  colorspace::lighten(ukraine_blue, amount = .36,space = "HLS"),
  day          = ukraine_yellow,
  # signal       = "#660000",  # https://colorswatches.info/color/blood-red
  # signal       = "black",  # https://colorswatches.info/color/blood-red
  signal       = "#DB0D20",  # scarlet from russian flag
  boundary     = "gray20",
  zenith       = "white"
)

palette_faint <- as.list(scales::alpha(palette_solid, alpha = .8))

g1 <- 
  ds_date |>
  # mutate(
  #   zenith2 = hms::as_hms("12:2")
  # )
  # dplyr::select(start_d, start_t, stop_t) |> 
  # dplyr::filter(date == as.Date("2022-03-10")) |> 
  # dplyr::slice(1:2) |>
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = hms::as_hms("00:00:00"), ymax = start_astronomical     ), fill = palette_faint$night       , color = NA) +
  geom_ribbon(aes(ymin = start_astronomical     , ymax = start_nautical         ), fill = palette_faint$astronomical, color = NA) +
  geom_ribbon(aes(ymin = start_nautical         , ymax = start_civil            ), fill = palette_faint$nautical    , color = NA) +
  geom_ribbon(aes(ymin = start_civil            , ymax = sunrise                ), fill = palette_faint$civil       , color = NA) +
  geom_ribbon(aes(ymin = sunrise                , ymax = sunset                 ), fill = palette_faint$day         , color = NA) +
  geom_ribbon(aes(ymin = sunset                 , ymax = stop_civil             ), fill = palette_faint$civil       , color = NA) +
  geom_ribbon(aes(ymin = stop_civil             , ymax = stop_nautical          ), fill = palette_faint$nautical    , color = NA) +
  geom_ribbon(aes(ymin = stop_nautical          , ymax = stop_astronomical      ), fill = palette_faint$astronomical, color = NA) +
  geom_ribbon(aes(ymin = stop_astronomical      , ymax = hms::as_hms("24:00:00")), fill = palette_faint$night      , color = NA) +
  
  # geom_line(  aes(y = start_astronomical), color = palette_solid$boundary, linetype = "F3"  ) +
  # geom_line(  aes(y = start_nautical    ), color = palette_solid$boundary,                  size = 1  ) +
  # geom_line(  aes(y = start_civil       ), color = palette_solid$boundary,                  size = .25) +
  # geom_line(  aes(y = sunrise           ), color = palette_solid$boundary, linetype = "F8"  ) +
  # geom_line(  aes(y = sunset            ), color = palette_solid$boundary, linetype = "F8"  ) +  
  # geom_line(  aes(y = stop_civil        ), color = palette_solid$boundary,                  size = .25) +
  # geom_line(  aes(y = stop_nautical     ), color = palette_solid$boundary,                  size = 1  ) +
  # geom_line(  aes(y = stop_astronomical ), color = palette_solid$boundary, linetype = "F3"  ) +
  
  # geom_line(  aes(y = zenith), color = palette_solid$zenith, linetype = "a3", size = 1) +
  geom_line(  aes(y = zenith), color = palette_solid$zenith, linetype = "solid", size = .4) +
  
  geom_rect(
    data = ds_event_within_day,
    aes(xmin = start_d - .5, xmax = start_d + .5, ymin = start_t, ymax = stop_t, x = NULL),
    color = "black"
    # color = palette_solid$signal
    , fill = palette_faint$signal,
    size = .15
  ) +
  geom_text(aes(label = date_display) , y = -Inf, hjust = .5, vjust=- 4.9, srt = 0, size = 1.5, na.rm = T, color ="grey80") +
  geom_text(aes(label = date_display2), y = -Inf, hjust = .5, vjust=-.2,  srt = 0, size = 1.6, na.rm = T, color ="grey40") +
  
  geom_text(label = "№",x =-Inf, y = -Inf, vjust = -4.5, hjust = -.6, size = 1.6, color = "grey80",lineheight = .8)+
  # geom_text(label = "день",x =-Inf, y = -Inf, vjust = -6, hjust = -1.1, size = 1.5, color = "grey80",lineheight = .8)+
  # geom_text(label = "дата",x =-Inf, y = -Inf, vjust = -1.4, hjust = -1.1, size = 1.5, color = "grey80",lineheight = .8)+
  
  geom_text(label = "Кількість",x =-Inf, y = Inf,vjust = 1.6, hjust = 1 , size = 2, color = "grey80", lineheight=.8)+
  geom_text(aes(label = event_tally_within_day, color = event_tally_within_day), y = Inf, family = "mono", vjust = 1.3, na.rm = T, size=3.6) +
  
  geom_text(label = "зеніт",        x =as.Date("2022-03-17"), aes(y = hms::parse_hms("12:50:00")), color ="white", size =3.5)+
  geom_text(label = "сутінки",      x =as.Date("2022-03-19"), aes(y = hms::parse_hms("20:30:00")), color ="white", size =1.8)+
  geom_text(label = "астрономічнi", x =as.Date("2022-03-18"), aes(y = hms::parse_hms("19:50:00")), color ="white", size =1.8)+
  geom_text(label = "морськi",      x =as.Date("2022-03-18"), aes(y = hms::parse_hms("19:10:00")), color ="white", size =1.8)+
  geom_text(label = "цивільнi",     x =as.Date("2022-03-18"), aes(y = hms::parse_hms("18:35:00")), color ="white", size =1.8)+
  geom_text(label = "цивільний",    x =as.Date("2022-03-18"), aes(y = hms::parse_hms("06:02:00")), color ="white", size =1.8)+
  geom_text(label = "морський",     x =as.Date("2022-03-18"), aes(y = hms::parse_hms("05:27:00")), color ="white", size =1.8)+
  geom_text(label = "астрономічний",x =as.Date("2022-03-18"), aes(y = hms::parse_hms("04:50:00")), color ="white", size =1.8)+
  geom_text(label = "світанок",     x =as.Date("2022-03-19"), aes(y = hms::parse_hms("04:00:00")), color ="white", size =1.8)+
  #https://uk.wikipedia.org/wiki/%D0%A1%D0%B2%D1%96%D1%82%D0%B0%D0%BD%D0%BE%D0%BA
  scale_x_date(
    date_labels = "%b\n%d", date_breaks = "1 week", date_minor_breaks = "1 week"
    # ,limits = as.Date(c("2022-02-25","2022-03-25"))
    ,expand = expansion(mult=c(0,.05))
    ) +
  scale_y_time(
    breaks = hms::as_hms(c("00:00:00", "04:00:00", "08:00:00", "12:00:00", "16:00:00", "20:00:00", "24:00:00")),
    # minor_breaks = hms::as_hms(c("01:00:00", "02:00:00", "03:00:00", "05:00:00", "06:00:00", "07:00:00", "09:00:00")),
    labels = c("00", "04", "08", "12", "16", "20", "24")
    ,expand = expansion(mult=c(.06,.045))
    # ,sec.axis = sec_axis(name="Secondary")
  ) +
  # scale_color_brewer(type = "seq", palette = "YlOrRd") +
  # scale_color_continuous(type = "viridis") +
  scale_colour_viridis_b(direction = -1) +
  coord_cartesian(ylim = hms::parse_hms(c("00:00:00", "23:59:59"))) +
  theme_minimal() +
  labs(
    title = "Тривалисть та частота повітряних тривог у м.Вінниця"
    # ,subtitle = "м.Вінниця, перший місяць війни 2022 року"
    ,caption = "Design by Will Beasley and Andriy Koval, data by Victor Dyadkovych                                                                      "
    ,x = NULL
    ,y = "Година доби"
  )+
  theme(
    legend.position = "none"
    ,plot.title = element_text(size=10)
    ,plot.caption = element_text(size=5, color = "grey70")
    ,axis.title.y = element_text(size=7,color = "grey60")
    # ,axis.text.x = element_text(color = "grey10")
    ,axis.text.x = element_blank()
    ,axis.text.y = element_text(color = "grey60",family = "mono")
    # ,plot.margin = margin(20,20,20,20)
    # ,plot.background = element_rect(fill = "white", color = "black", size = 0) 
    # axis.l = element_text(margin=margin(t=20)
    ,panel.grid = element_blank()
    ,axis.ticks.y = element_line(size =.1)
  ) 
g1
ggsave(
  plot = g1,
  filename = "analysis/report-1/prints/cyclogram.png",
  height   = 4,
  width    = 5,
  dpi      = 400,
  bg       = "white"
)


# ---- graph-2 -----------------------------------------------------------------
d2 <- 
  ds_date_tally %>% 
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
