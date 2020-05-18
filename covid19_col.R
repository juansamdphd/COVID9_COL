library(RSocrata)
library(tidyverse)
library(lubridate)
library(forecast)

# Read Data from INS CO
# key name: covid19_col

covid19_col <- read.socrata(
  "https://www.datos.gov.co/resource/gt2j-8ykr.json",
  app_token = my_app_token,
  email     = my_email,
  password  = my_password)

covid19_col$fecha_diagnostico <- date(ymd_hms(covid19_col$fecha_diagnostico))

covid19_col$fecha_de_notificaci_n <- date(ymd_hms(covid19_col$fecha_de_notificaci_n))

covid19_col$fecha_de_muerte <- date(ymd_hms(covid19_col$fecha_de_muerte))

covid19_col$fis <- date(ymd_hms(covid19_col$fis))

covid19_col$fecha_recuperado <- date(ymd_hms(covid19_col$fecha_recuperado))

cali <- covid19_col %>% filter(ciudad_de_ubicaci_n == "Cali") %>% arrange(fecha_de_notificaci_n) %>% mutate(count = row_number())

### Combine graphs (in progress)
covid19_col %>% 
  ggplot() + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x= fecha_diagnostico, y=cumsum(..count..)), binwidth = 1, geom = "step", colour = "red") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x=fecha_diagnostico, y=cumsum(..count..)), binwidth = 1, geom = "point", colour = "red") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x=fecha_diagnostico, y=cumsum(..count..), label = cumsum(..count..)), binwidth = 1, geom = "text", colour = "black", vjust = -0.25) + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x= fecha_de_muerte, y=cumsum(..count..)), binwidth = 1, geom = "step", colour = "black") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x=fecha_de_muerte, y=cumsum(..count..)), binwidth = 1, geom = "point", colour = "black") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x=fecha_de_muerte, y=cumsum(..count..), label = cumsum(..count..)), binwidth = 1, geom = "text", colour = "black", vjust = -0.25) + scale_x_date(date_labels = "%b %d", date_breaks = "1 day", minor_breaks = "1 day") + scale_y_continuous(name = "# de casos notificados (cumulativo)", breaks = seq(0,800,50)) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

## Cleaner code
covid19_col %>% filter(ciudad_de_ubicaci_n == "Cali") %>%
  ggplot() + stat_bin(aes(x= fecha_diagnostico, y=cumsum(..count..)), binwidth = 1, geom = "step", colour = "red") + stat_bin(aes(x= fecha_diagnostico, y=cumsum(..count..)), binwidth = 1, geom = "point", colour = "red") + stat_bin(aes(x=fecha_diagnostico, y=cumsum(..count..), label = cumsum(..count..)), binwidth = 1, geom = "text", colour = "black", vjust = -0.25) + stat_bin(aes(x= fecha_de_muerte, y=cumsum(..count..)), binwidth = 1, geom = "step", colour = "black") + stat_bin(aes(x=fecha_de_muerte, y=cumsum(..count..)), binwidth = 1, geom = "point", colour = "black") + stat_bin(aes(x=fecha_de_muerte, y=cumsum(..count..), label = cumsum(..count..)), binwidth = 1, geom = "text", colour = "black", vjust = -0.25) + scale_x_date(name = "Fecha de notificacion", date_labels = "%b %d", date_breaks = "1 day", minor_breaks = "1 day") + scale_y_continuous(name = "# de casos notificados (cumulativo)", breaks = seq(0,1500,100)) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Log10 graph
covid19_col %>% 
  ggplot() + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x= fecha_diagnostico, y=cumsum(..count..)), binwidth = 1, geom = "step", colour = "red") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x=fecha_diagnostico, y=cumsum(..count..)), binwidth = 1, geom = "point", colour = "red") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x=fecha_diagnostico, y=cumsum(..count..), label = cumsum(..count..)), binwidth = 1, geom = "text", colour = "black", vjust = -0.25) + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x= fecha_de_muerte, y=cumsum(..count..)), binwidth = 1, geom = "step", colour = "black") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x=fecha_de_muerte, y=cumsum(..count..)), binwidth = 1, geom = "point", colour = "black") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x=fecha_de_muerte, y=cumsum(..count..), label = cumsum(..count..)), binwidth = 1, geom = "text", colour = "black", vjust = -0.25) + scale_x_date(name = "Fecha de Diagnostico", date_labels = "%b %d", date_breaks = "1 day", minor_breaks = "1 day") + scale_y_log10(name = "# de casos (cumulativo)") + annotation_logticks() + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")
