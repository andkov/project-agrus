# geom_text(aes(x=as.Date("2022-03-12")),label = "\U2193 Доба після другого вторгнення \U2193", y = -Inf,vjust = -5.5, hjust = -.05 , size = 2, color = "white", lineheight=.8)+
# geom_text(aes(label = date_display) , y = -Inf, hjust = .5, vjust=- 4.9, srt = 0, size = 1.5, na.rm = T, color ="white") +
# geom_text(aes(label = date_display2), y = -Inf, hjust = .5, vjust=-.2,  srt = 0, size = 1.6, na.rm = T, color ="grey40") +
# 
# # geom_text(label = "№",x =-Inf, y = -Inf, vjust = -4.5, hjust = -.6, size = 1.6, color = "grey80",lineheight = .8)+
# # geom_text(label = "день",x =-Inf, y = -Inf, vjust = -6, hjust = -1.1, size = 1.5, color = "grey80",lineheight = .8)+
# # geom_text(label = "дата",x =-Inf, y = -Inf, vjust = -1.4, hjust = -1.1, size = 1.5, color = "grey80",lineheight = .8)+
# 
# geom_text(aes(x=as.Date("2022-03-11")),label = "\U2191 Кількість повітряних тривог за добу \U2191", y = Inf,vjust = 3.8, hjust = -.05 , size = 2, color = "white", lineheight=.8)+
# geom_text(aes(label = event_tally_within_day, color = event_tally_within_day), y = Inf, family = "mono", vjust = 1.3, na.rm = T, size=3.6) +
# 
# geom_text(label = " зеніт",        x =as.Date("2022-02-25"), aes(y = hms::parse_hms("12:50:00")), color =palette_solid$zenith, size =3)+
# 
# # geom_text(label = "сутінки",      x =as.Date("2022-02-25"), aes(y = hms::parse_hms("20:00:00")), color ="white", size =1.8,srt=2,hjust=.3)+
# geom_text(label = "астрономічнi", x =as.Date("2022-02-26"), aes(y = hms::parse_hms("19:20:00")), color ="white", size =1.8,srt=2,hjust=.35)+
# geom_text(label = "морськi",      x =as.Date("2022-02-27"), aes(y = hms::parse_hms("18:40:00")), color ="white", size =1.8,srt=3,hjust=1)+
# geom_text(label = "цивільнi сутінки",     x =as.Date("2022-02-27"), aes(y = hms::parse_hms("18:05:00")), color ="white", size =1.8,srt=3,hjust=.2)+
# 
# # geom_text(label = "сутінки",      x =as.Date("2022-03-09"), aes(y = hms::parse_hms("20:05:00")), color ="white", size =1.8,srt=2)+
# # geom_text(label = "астрономічнi", x =as.Date("2022-03-10"), aes(y = hms::parse_hms("19:35:00")), color ="white", size =1.8,srt=3)+
# # geom_text(label = "морськi",      x =as.Date("2022-03-11"), aes(y = hms::parse_hms("19:05:00")), color ="white", size =1.8,srt=3)+
# # geom_text(label = "цивільнi",     x =as.Date("2022-03-11"), aes(y = hms::parse_hms("18:35:00")), color ="white", size =1.8,srt=3)+
# geom_text(label = "цивільний світанок",    x =as.Date("2022-02-28"), aes(y = hms::parse_hms("06:35:00")), color ="white", size =1.8,srt=-3,hjust=.5)+
# geom_text(label = "морський",     x =as.Date("2022-02-28"), aes(y = hms::parse_hms("06:00:00")), color ="white", size =1.8,srt=-3,hjust=1.15)+
# geom_text(label = "астрономічний",x =as.Date("2022-02-27"), aes(y = hms::parse_hms("05:25:00")), color ="white", size =1.8,srt=-3,hjust=.6)+
# # geom_text(label = "світанок",     x =as.Date("2022-02-26"), aes(y = hms::parse_hms("04:45:00")), color ="white", size =1.8,srt=-3,hjust=.65)+
# 
# 
# geom_text(label = "Пт",     x =as.Date("2022-02-25"), aes(y = hms::parse_hms("08:29:00")), color =palette_solid$signal,  size =1.6,)+
# geom_text(label = "Сб",     x =as.Date("2022-02-26"), aes(y = hms::parse_hms("08:29:00")), color =palette_solid$signal2, size =1.6)+
# geom_text(label = "Нд",     x =as.Date("2022-02-27"), aes(y = hms::parse_hms("08:29:00")), color =palette_solid$signal2, size =1.6)+
# geom_text(label = "Пн",     x =as.Date("2022-02-28"), aes(y = hms::parse_hms("08:29:00")), color =palette_solid$signal,  size =1.6,)+
# geom_text(label = "Вт",     x =as.Date("2022-03-01"), aes(y = hms::parse_hms("08:29:00")), color =palette_solid$signal,  size =1.6,)+
# geom_text(label = "Ср",     x =as.Date("2022-03-02"), aes(y = hms::parse_hms("08:29:00")), color =palette_solid$signal,  size =1.6,)+
# geom_text(label = "Чт",     x =as.Date("2022-03-03"), aes(y = hms::parse_hms("08:29:00")), color =palette_solid$signal,  size =1.6,)+
# #https://uk.wikipedia.org/wiki/%D0%A1%D0%B2%D1%96%D1%82%D0%B0%D0%BD%D0%BE%D0%BA
# scale_x_date(
#   date_labels = "%b\n%d", date_breaks = "1 week", date_minor_breaks = "1 week"
#   # ,limits = as.Date(c("2022-02-25","2022-03-25"))
#   ,expand = expansion(mult=c(0,.05))
#   ) +
# scale_y_time(
#   breaks = hms::as_hms(c("00:00:00", "04:00:00", "08:00:00", "12:00:00", "16:00:00", "20:00:00", "24:00:00")),
#   # minor_breaks = hms::as_hms(c("01:00:00", "02:00:00", "03:00:00", "05:00:00", "06:00:00", "07:00:00", "09:00:00")),
#   labels = c("00", "04", "08", "12", "16", "20", "24")
#   ,expand = expansion(mult=c(.06,.045))
#   # ,sec.axis = sec_axis(name="Secondary")
# ) +
# scale_fill_manual(values = c("TRUE"=palette_faint$signal2, "FALSE"=palette_faint$signal))+
# # scale_color_brewer(type = "seq", palette = "YlOrRd") +
# # scale_color_continuous(type = "viridis") +
# scale_colour_viridis_b(direction = -1) +
# coord_cartesian(ylim = hms::parse_hms(c("00:00:00", "23:59:59"))) +
# theme_minimal() +
# labs(
#   title = "Тривалисть та частота повітряних тривог у м.Вінниця"
#   # ,subtitle = "м.Вінниця, перший місяць війни 2022 року"
#   ,caption = "Design by Will Beasley and Andriy Koval, data by Victor Dyadkovych                                                                      "
#   ,x = NULL
#   ,y = "Година доби"
# )+






# 
# # ---- graph-2 -----------------------------------------------------------------
# d2 <-
#   ds_date_tally %>%
#   tidyr::pivot_longer(
#     cols = c("event_tally_within_day","duration_total_day","duration_mean_day" )
#     ,names_to = "measure"
#     ,values_to = "value"
#   ) %>%
#   mutate(
#     measure = case_when(
#       measure == "event_tally_within_day" ~ "count"
#       ,measure == "duration_total_day" ~ "total_mins"
#       ,measure == "duration_mean_day" ~ "mean_mins"
# 
#     )
#   )
# g2 <-
#   d2 %>%
#   ggplot(aes(x=start_d, y = value))+
#   geom_line(size=.4,linetype="dotted")+
#   geom_point(shape = 21, size = 3,fill=russian_red, color = "black", alpha = .4)+
#   # geom_text(aes(label = date_index), y = -Inf, vjust = -.01) +
#   # geom_text( aes(label = round(value,0)), y = Inf,xjust = 2.11, angle = -90) +
#   # scale_y_continuous(expand = expansion(mult = c(0,lk1.1)))+
#   geom_smooth(color=ukraine_blue, method = "loess")+
#   scale_x_date(date_breaks = "1 week", minor_breaks = "1 day",date_labels = "%b-%d")+
#   facet_wrap(
#     facets = "measure"
#     , scales = "free"
#     ,labeller = labeller(measure = c(
#       "count" = "Кількість","mean_mins" = "Середня тривалість","total_mins" = "Сукупна тривалість"
#     ))
#   )+
#   labs(
#     title = "Статистика повітряних тривог у місті Вінниця"
#     ,subtitle = "Зведення за кожну добу (0-24) -- Тривога зараховується до доби у яку почалась -- Понеділки позначені датами"
#     ,caption = "Design by Will Beasley and Andriy Koval, data by Victor Dyadkovych                                                              "
#     ,x = NULL
#     ,y = "Значення"
#   )+
#   theme(
#     plot.caption = element_text(size=10, color = "grey70")
#   )
# g2
# g2 %>% quick_save("2-count-duration",width=12, height =4)
# 
# 