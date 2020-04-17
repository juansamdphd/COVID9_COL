library(RSocrata)
library(lubridate)
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

covid19_col %>% filter(ciudad_de_ubicaci_n =="CALI") %>% ggplot(aes(x = fecha_de_diagn_stico)) + stat_count(aes(y=cumsum(..count..)))

covid19_col %>% filter(Ciudad == "BOGOTA" | Ciudad == "CALI" | Ciudad == "MEDELLIN") %>% ggplot(aes(x = Fecha_dx, fill = Ciudad)) + stat_count()

cali <- covid19_col %>% filter(Ciudad =="CALI")
mde <- covid19_col %>% filter(Ciudad=="MEDELLIN")
bog <- covid19_col %>% filter(Ciudad=="BOGOTA")

ggplot(data = bog, aes(x = Fecha_dx, fill = Ciudad)) + stat_count(aes(y=cumsum(..count..))) + stat_count(data = cali, aes(x = Fecha_dx, y=cumsum(..count..))) + stat_count(data = mde, aes(x = Fecha_dx, y=cumsum(..count..))) + scale_x_date(date_labels = "%b %d", date_breaks = "1 day") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap(~Ciudad)
