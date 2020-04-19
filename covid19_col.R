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

covid19_col$fecha_de_diagn_stico <- date(covid19_col$fecha_de_diagn_stico)

covid19_col %>% 
  ggplot(aes(x=fecha_de_diagn_stico, colour = ciudad_de_ubicaci_n)) + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Bogota"), aes(y=cumsum(..count..)), geom = "step") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Cali"), aes(y=cumsum(..count..)), geom = "step") + stat_bin(data=subset(covid19_col, ciudad_de_ubicaci_n == "Medellin"), aes(y=cumsum(..count..)), geom = "step") + scale_x_date(date_labels = "%b %d", date_breaks = "1 day") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
