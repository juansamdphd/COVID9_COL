library(RSocrata)
library(tidyverse)

# Read Data from INS CO
# key name: covid19_col

### API key datos gov.co: datos_col
### key ID: c3moeeui79vxz3mcp4vqidh72
### secret key: 77ousfqh4a0ukrnm5ltxifr70z3c9f514z3jau1vv7ds1bdg4

covid19_col <- read.socrata(
  "https://www.datos.gov.co/resource/gt2j-8ykr.json",
  app_token = "gBnYk2R4qxZFqwev2oYi3aYuT",
  email     = "jucamilo.sanchez@gmail.com",
  password  = "Sarias23!"
)

covid19_col$fecha_de_notificaci_n <- ymd_hms(covid19_col$fecha_de_notificaci_n)
covid19_col$fecha_de_notificaci_n <- date(covid19_col$fecha_de_notificaci_n)

which(is.na(covid19_col$fecha_de_notificaci_n))

covid19_col$fecha_de_notificaci_n[3653] <- "2020-04-13"
covid19_col$fecha_de_notificaci_n[3654] <-  "2020-04-18"

covid19_col %>% 
  ggplot(aes(x=fecha_de_notificaci_n, colour = ciudad_de_ubicaci_n)) + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Bogotá D.C."), aes(y=cumsum(..count..)), geom = "step") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(y=cumsum(..count..)), geom = "step") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Medellín"), aes(y=cumsum(..count..)), geom = "step") + scale_x_date(date_labels = "%b %d", date_breaks = "1 day") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

covid19_col %>% 
  ggplot(aes(x=fecha_de_notificaci_n)) + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(y=cumsum(..count..)), binwidth = 1, geom = "step", colour = "red") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(y=cumsum(..count..), label = cumsum(..count..)), binwidth = 1, geom = "text", colour = "black") + scale_x_date(name = "Fecha de Notificacion", date_labels = "%b %d", date_breaks = "1 day", minor_breaks = "1 day") + scale_y_continuous(name = "# de casos") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")
