library(googlesheets4)
library(tidyverse)

# Read Data from INS CO
sheets_deauth()

covid_col <- sheets_read("1l76JEKrN9_2wdREXhL74qVgoppXJBqjP6G8oWxLipTA")

# Changing names to something R compatible
colnames(covid_col) <- c("ID", "Fecha_dx", "Ciudad", "Dept_Dist", "Status", "Edad", "Sexo", "Tipo", "Origen")

# Converting Excel date format to regular format, Excel Origin is "1899-12-30" in Windows, and "1904-01-01". Not sure if Mac's format translates to UNIX based systems (?) -Not needed anymore
# covid_col$Fecha_dx <- as.Date(covid_col$Fecha_dx, origin = "1899-12-30")

covid_col %>% filter(Ciudad =="CALI") %>% ggplot(aes(x = Fecha_dx)) + stat_count(aes(y=cumsum(..count..)))

covid_col %>% filter(Ciudad == "BOGOTA" | Ciudad == "CALI" | Ciudad == "MEDELLIN") %>% ggplot(aes(x = Fecha_dx, fill = Ciudad)) + stat_count()

cali <- covid_col %>% filter(Ciudad =="CALI")
mde <- covid_col %>% filter(Ciudad=="MEDELLIN")
bog <- covid_col %>% filter(Ciudad=="BOGOTA")

ggplot(data = bog, aes(x = Fecha_dx, fill = Ciudad)) + stat_count(aes(y=cumsum(..count..))) + stat_count(data = cali, aes(x = Fecha_dx, y=cumsum(..count..))) + stat_count(data = mde, aes(x = Fecha_dx, y=cumsum(..count..))) + scale_x_date(date_labels = "%b %d", date_breaks = "1 day") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap(~Ciudad)
