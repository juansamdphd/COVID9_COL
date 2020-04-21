library(RSocrata)
library(tidyverse)

# Read Data from INS CO
# key name: covid19_col

covid19_col <- read.socrata(
  "https://www.datos.gov.co/resource/gt2j-8ykr.json",
  app_token = my_app_token,
  email     = my_email,
  password  = my_password)

covid19_col$fecha_de_notificaci_n <- ymd_hms(covid19_col$fecha_de_notificaci_n)
covid19_col$fecha_de_notificaci_n <- date(covid19_col$fecha_de_notificaci_n)

which(is.na(covid19_col$fecha_de_notificaci_n))

covid19_col$fecha_de_notificaci_n[3653] <- "2020-04-13"
covid19_col$fecha_de_notificaci_n[3654] <-  "2020-04-18"

covid19_col$fecha_de_muerte <- date(ymd_hms(covid19_col$fecha_de_muerte))

covid19_col$fecha_recuperado <- date(ymd_hms(covid19_col$fecha_recuperado))

covid19_col %>% 
  ggplot(aes(x=fecha_de_notificaci_n, colour = ciudad_de_ubicaci_n)) + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Bogotá D.C."), aes(y=cumsum(..count..)), geom = "step") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(y=cumsum(..count..)), geom = "step") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Medellín"), aes(y=cumsum(..count..)), geom = "step") + scale_x_date(date_labels = "%b %d", date_breaks = "1 day") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

cali_casos_notif <- covid19_col %>% 
  ggplot(aes(x=fecha_de_notificaci_n)) + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(y=cumsum(..count..)), binwidth = 1, geom = "step", colour = "red")  + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(y=cumsum(..count..)), binwidth = 1, geom = "point", colour = "red") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(y=cumsum(..count..), label = cumsum(..count..)), binwidth = 1, geom = "text", colour = "black", vjust = -0.25) + scale_x_date(name = "Fecha de Notificacion", date_labels = "%b %d", date_breaks = "1 day", minor_breaks = "1 day") + scale_y_continuous(name = "# de casos", breaks = seq(0,500,50)) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

covid19_col %>% filter(ciudad_de_ubicaci_n == "Cali") %>%
  count(atenci_n == "Fallecido")

cali_casos_muerte <- covid19_col %>% 
  ggplot(aes(x=fecha_de_muerte)) + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(y=cumsum(..count..)), binwidth = 1, geom = "step", colour = "red") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(y=cumsum(..count..), label = ..count..), binwidth = 1, geom = "text", colour = "black", vjust = -0.25) + scale_x_date(name = "Fecha de Notificacion", date_labels = "%b %d", date_breaks = "1 day", minor_breaks = "1 day") + scale_y_continuous(name = "# de casos", breaks = seq(0,500,50)) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

## Log10 graph
covid19_col %>% 
  ggplot(aes(x=fecha_de_notificaci_n)) + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(y=cumsum(..count..)), binwidth = 1, geom = "step", colour = "red") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(y=cumsum(..count..), label = cumsum(..count..)), binwidth = 1, geom = "text", colour = "black") + scale_x_date(name = "Fecha de Notificacion", date_labels = "%b %d", date_breaks = "1 day", minor_breaks = "1 day") + scale_y_log10(name = "# de casos") + annotation_logticks() + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")