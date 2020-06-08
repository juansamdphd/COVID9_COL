library(RSocrata)
library(tidyverse)
library(ggrepel)
library(lubridate)
library(forecast)

setwd("~/Documents/GitHub/COVID9_COL")

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

covid19_col %>% arrange(fecha_de_notificaci_n) %>% group_by(fecha_de_notificaci_n, ciudad_de_ubicaci_n) %>% summarize(Casos = n()) %>% View()

cali_notif <- covid19_col %>% filter(ciudad_de_ubicaci_n == "Cali") %>% arrange(fecha_de_notificaci_n) %>% group_by(fecha_de_notificaci_n) %>% summarise(count = n()) %>% mutate(cumu = cumsum(count))

cali_muerte <- covid19_col %>% filter(ciudad_de_ubicaci_n == "Cali") %>% arrange(fecha_de_muerte) %>% filter(is.na(fecha_de_muerte) == FALSE) %>% group_by(fecha_de_muerte) %>% summarise(count = n()) %>% mutate(cumu = cumsum(count))

cali_notif <- cali_notif %>% mutate(moving_avg_count_notif = round(ma(cali_notif$count, order = 5, centre = FALSE), digits = 0))

cali_notif <- cali_notif %>% mutate(moving_avg_notif = round(ma(cali_notif$cumu, order = 5, centre = FALSE), digits = 0))

cali_muerte <- cali_muerte %>% mutate(moving_avg_muerte = round(ma(cali_muerte$cumu, order = 5, centre = FALSE), digits = 0))

## Log10 scale with moving average
cali_notif %>% ggplot(aes(fecha_de_notificaci_n, moving_avg_notif)) + 
  geom_line(aes(colour = "red")) +
  geom_point(size = 0.5, colour = "red") +
  #geom_text(label = cali_notif$moving_avg_notif, position = position_dodge(0.9), check_overlap = FALSE, vjust = -0.5) +
  geom_point(data = cali_muerte, aes(fecha_de_muerte, moving_avg_muerte), size = 0.5) +
  geom_line(data = cali_muerte, aes(fecha_de_muerte, moving_avg_muerte, colour = "black")) +
  #geom_text(data = cali_muerte, aes(fecha_de_muerte, cumu, label = cumu), position = position_dodge(0.9), check_overlap = FALSE, vjust = -0.5) +
  scale_colour_manual(name = "", values = c("red" = "red", "black" = "black"), labels = c("Muertes", "Notificados")) +
  theme_bw()  + 
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") + 
  ggtitle("COVID-19 en Cali") +
  #scale_y_continuous(name = "Cuenta acumulativa", breaks = seq(0,2000,200)) +
  scale_y_log10(name = "# de casos (cumulativo)", limits = c(1,10000)) + annotation_logticks() +
  scale_x_date(name = "", date_labels = "%b %d", date_breaks = "5 day", minor_breaks = "1 day")

#### Absolute count with moving average ####
cali_notif %>% ggplot(aes(fecha_de_notificaci_n, moving_avg_notif)) + 
  geom_line(aes(colour = "red")) +
  geom_point(size = 0.5, colour = "red") +
  #geom_text(label = cali_notif$moving_avg_notif, position = position_dodge(0.9), check_overlap = FALSE, vjust = -0.5) +
  #geom_point(data = cali_muerte, aes(fecha_de_muerte, moving_avg_muerte), size = 0.5) +
  #geom_line(data = cali_muerte, aes(fecha_de_muerte, moving_avg_muerte, colour = "black")) +
  #geom_text(data = cali_muerte, aes(fecha_de_muerte, cumu, label = cumu), position = position_dodge(0.9), check_overlap = FALSE, vjust = -0.5) +
  #scale_colour_manual(name = "", values = c("red" = "red", "black" = "black"), labels = c("Muertes", "Notificados")) +
  theme_bw()  + 
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + 
  ggtitle("COVID-19 en Cali") +
  scale_y_continuous(name = "Cuenta acumulativa", breaks = seq(0,4000,500)) +
  #scale_y_log10(name = "# de casos (cumulativo)") + annotation_logticks() +
  scale_x_date(name = "", date_labels = "%b %d", date_breaks = "5 day", minor_breaks = "1 day")

#### Absolute daily ####
cali_notif %>% ggplot(aes(fecha_de_notificaci_n, count)) + 
  #geom_col(aes(colour = "red", fill = "red"), alpha = 0.2) +
  geom_line(aes(colour = "red")) +
  #geom_smooth(method = "loess", colour = "red", fill = "red", alpha = 0.2) +
  geom_point(size = 0.5, colour = "red") +
  #geom_text(label = cali_notif$moving_avg_notif, position = position_dodge(0.9), check_overlap = FALSE, vjust = -0.5) +
  #geom_point(data = cali_muerte, aes(fecha_de_muerte, moving_avg_muerte), size = 0.5) +
  #geom_line(data = cali_muerte, aes(fecha_de_muerte, moving_avg_muerte, colour = "black")) +
  #geom_text(data = cali_muerte, aes(fecha_de_muerte, cumu, label = cumu), position = position_dodge(0.9), check_overlap = FALSE, vjust = -0.5) +
  #scale_colour_manual(name = "", values = c("red" = "red", "black" = "black"), labels = c("Muertes", "Notificados")) +
  theme_bw()  + 
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + 
  ggtitle("COVID-19 en Cali") +
  scale_y_continuous(name = "Casos nuevos", breaks = seq(0,200,25)) +
  #scale_y_log10(name = "# de casos (cumulativo)") + annotation_logticks() +
  scale_x_date(name = "", date_labels = "%b %d", date_breaks = "5 day", minor_breaks = "1 day")

#### Absolute daily moving average ####
cali_notif %>% ggplot(aes(fecha_de_notificaci_n, moving_avg_count_notif)) + 
  #geom_col(aes(colour = "red", fill = "red"), alpha = 0.2) +
  geom_line(aes(colour = "red")) +
  #geom_smooth(method = "loess", colour = "red", fill = "red", alpha = 0.2) +
  geom_point(size = 0.5, colour = "red") +
  #geom_text(label = cali_notif$moving_avg_notif, position = position_dodge(0.9), check_overlap = FALSE, vjust = -0.5) +
  #geom_point(data = cali_muerte, aes(fecha_de_muerte, moving_avg_muerte), size = 0.5) +
  #geom_line(data = cali_muerte, aes(fecha_de_muerte, moving_avg_muerte, colour = "black")) +
  #geom_text(data = cali_muerte, aes(fecha_de_muerte, cumu, label = cumu), position = position_dodge(0.9), check_overlap = FALSE, vjust = -0.5) +
  #scale_colour_manual(name = "", values = c("red" = "red", "black" = "black"), labels = c("Muertes", "Notificados")) +
  theme_bw()  + 
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + 
  ggtitle("COVID-19 en Cali") +
  scale_y_continuous(name = "Casos nuevos\n (promedio movil)", breaks = seq(0,200,25)) +
  #scale_y_log10(name = "# de casos (cumulativo)") + annotation_logticks() +
  scale_x_date(name = "", date_labels = "%b %d", date_breaks = "5 day", minor_breaks = "1 day")

#### Absolute cumulative count NO moving average ####
cali_notif %>% ggplot(aes(fecha_de_notificaci_n, cumu)) + 
  geom_line(aes(colour = "red")) +
  geom_point(size = 0.5, colour = "red") +
  geom_text(data = cali_notif, aes( max(fecha_de_notificaci_n), max(cumu), label = max(cumu)), position = position_dodge(0.9), check_overlap = FALSE, vjust = -0.5, size = 5) +
  geom_point(data = cali_muerte, aes(fecha_de_muerte, cumu), size = 0.5) +
  geom_line(data = cali_muerte, aes(fecha_de_muerte, cumu, colour = "black")) +
  geom_text(data = cali_muerte, aes(max(fecha_de_muerte), max(cumu), label = max(cumu)), position = position_dodge(0.9), check_overlap = FALSE, vjust = -0.5, size = 5) +
  scale_colour_manual(name = "", values = c("red" = "red", "black" = "black"), labels = c("Muertes", "Notificados")) +
  theme_bw()  + 
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") + 
  ggtitle("COVID-19 en Cali") +
  scale_y_continuous(name = "Cuenta acumulativa", breaks = seq(0,4000,500)) +
  #scale_y_log10(name = "# de casos (cumulativo)") + annotation_logticks() +
  scale_x_date(name = "", date_labels = "%b %d", date_breaks = "5 day", minor_breaks = "1 day")

#### Log10 Absolute count NO moving average ####
cali_notif %>% ggplot(aes(fecha_de_notificaci_n, cumu)) + 
  geom_line(aes(colour = "red")) +
  geom_point(size = 0.5, colour = "red") +
  geom_text(data = cali_notif, aes( max(fecha_de_notificaci_n), max(cumu), label = max(cumu)), position = position_dodge(0.9), check_overlap = FALSE, vjust = -0.5, size = 5) +
  geom_point(data = cali_muerte, aes(fecha_de_muerte, cumu), size = 0.5) +
  geom_line(data = cali_muerte, aes(fecha_de_muerte, cumu, colour = "black")) +
  geom_text(data = cali_muerte, aes(max(fecha_de_muerte), max(cumu), label = max(cumu)), position = position_dodge(0.9), check_overlap = FALSE, vjust = -0.5, size = 5) +
  scale_colour_manual(name = "", values = c("red" = "red", "black" = "black"), labels = c("Muertes", "Notificados")) +
  theme_bw()  + 
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") + 
  ggtitle("COVID-19 en Cali") +
  #scale_y_continuous(name = "Cuenta acumulativa", breaks = seq(0,2000,200)) +
  scale_y_log10(name = "# de casos (cumulativo)", limits = c(1,10000)) + annotation_logticks() +
  scale_x_date(name = "", date_labels = "%b %d", date_breaks = "5 day", minor_breaks = "1 day")

#### Case status ####
cali_estado <- covid19_col %>% filter(ciudad_de_ubicaci_n == "Cali") %>%
  group_by(estado) %>% filter(estado !="N/A") %>% summarise(count = n()) %>% mutate(prp = round(count/sum(count)*100, digits = 2)) %>% mutate(ypos = cumsum(prp)-0.5*prp) 

# cali_estado$estado <- factor(c("Asintomático", "Leve", "Grave", "Moderado", "Fallecido"))

cali_estado %>% ggplot(aes(x = "", y = prp, fill = estado)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + 
  geom_text_repel(data = cali_estado, aes(y = ypos, label = prp), size = 5, colour = "white") + 
  theme_void() +
  scale_fill_brewer(palette = "Reds")

#### Using stat_bin just for Cali (adaptable to include other cities) ####
covid19_col %>% 
  ggplot() + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x= fecha_diagnostico, y=cumsum(..count..)), binwidth = 1, geom = "step", colour = "red") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x=fecha_diagnostico, y=cumsum(..count..)), binwidth = 1, geom = "point", colour = "red") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x=fecha_diagnostico, y=cumsum(..count..), label = cumsum(..count..)), binwidth = 1, geom = "text", colour = "black", vjust = -0.25) + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x= fecha_de_muerte, y=cumsum(..count..)), binwidth = 1, geom = "step", colour = "black") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x=fecha_de_muerte, y=cumsum(..count..)), binwidth = 1, geom = "point", colour = "black") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x=fecha_de_muerte, y=cumsum(..count..), label = cumsum(..count..)), binwidth = 1, geom = "text", colour = "black", vjust = -0.25) + scale_x_date(name = "", date_labels = "%b %d", date_breaks = "1 day", minor_breaks = "1 day") + scale_y_continuous(name = "# de casos notificados (cumulativo)", breaks = seq(0,800,50)) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

#### Using stat_bin just for Cali (cleaner) ####
covid19_col %>% filter(ciudad_de_ubicaci_n == "Cali") %>%
  ggplot() + stat_bin(aes(x= fecha_diagnostico, y=cumsum(..count..)), binwidth = 1, geom = "step", colour = "red") + stat_bin(aes(x= fecha_diagnostico, y=cumsum(..count..)), binwidth = 1, geom = "point", colour = "red") + stat_bin(aes(x=fecha_diagnostico, y=cumsum(..count..), label = cumsum(..count..)), binwidth = 1, geom = "text", colour = "black", vjust = -0.25) + stat_bin(aes(x= fecha_de_muerte, y=cumsum(..count..)), binwidth = 1, geom = "step", colour = "black") + stat_bin(aes(x=fecha_de_muerte, y=cumsum(..count..)), binwidth = 1, geom = "point", colour = "black") + stat_bin(aes(x=fecha_de_muerte, y=cumsum(..count..), label = cumsum(..count..)), binwidth = 1, geom = "text", colour = "black", vjust = -0.25) + scale_x_date(name = "Fecha de notificacion", date_labels = "%b %d", date_breaks = "1 day", minor_breaks = "1 day") + scale_y_continuous(name = "# de casos notificados (cumulativo)", breaks = seq(0,1500,100)) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

covid19_col %>% 
  ggplot() + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x= fecha_diagnostico, y=cumsum(..count..)), binwidth = 1, geom = "step", colour = "red") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x=fecha_diagnostico, y=cumsum(..count..)), binwidth = 1, geom = "point", colour = "red") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x=fecha_diagnostico, y=cumsum(..count..), label = cumsum(..count..)), binwidth = 1, geom = "text", colour = "black", vjust = -0.25) + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x= fecha_de_muerte, y=cumsum(..count..)), binwidth = 1, geom = "step", colour = "black") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x=fecha_de_muerte, y=cumsum(..count..)), binwidth = 1, geom = "point", colour = "black") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(x=fecha_de_muerte, y=cumsum(..count..), label = cumsum(..count..)), binwidth = 1, geom = "text", colour = "black", vjust = -0.25) + scale_x_date(name = element_blank(), date_labels = "%b %d", date_breaks = "5 day", minor_breaks = "1 day") + scale_y_log10(name = "# de casos (cumulativo)") + annotation_logticks() + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
