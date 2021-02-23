#load packages
library(tidyverse)
library(scales)
#Get the data
readxl::read_xlsx("indicators_12_02_2020.xlsx", sheet = "M2 Sem", skip = 3,col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  janitor::clean_names() -> m2

map(m2, class)

map(m2, ~sum(is.na(.)))

m2 %>% 
  filter(!is.na(variacion_percent_mensual)) %>% 
  slice(-(1:2)) %>% 
  ggplot(aes(fecha, variacion_percent_mensual)) +
  geom_area()

readxl::read_xlsx("indicators_12_02_2020.xlsx", sheet = "Inflacion Mensual", skip = 7) -> inflacion_m2

inflacion_m2 %>% 
  slice(13:nrow(inflacion_m2)) %>% 
  select(2, 4, 22) %>%  
  ggplot() +
  geom_area(aes(fecha, ...22)) +
  geom_line(aes(fecha, BCV...4)) +
  scale_x_datetime(labels = label_date_short(), date_breaks = "month") +
  coord_cartesian(xlim = as.POSIXct(c("2018-01-01 00", "2021-01-01 23")))
  
