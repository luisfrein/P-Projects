#Load packages
library(tidyverse)
library(rvest)
library(countrycode)
library(extrafont)

#datosmacro scraping url: https://datosmacro.expansion.com/tipo-interes
int_rate <- read_html("https://datosmacro.expansion.com/tipo-interes")

#Create a character vector with the data we´re interested in
int_rate %>% 
  html_nodes("tr") %>% 
  html_text() -> int_rate_tbl_raw

#Convert the vector to a tibble
int_rate_tbl_raw <- as_tibble(int_rate_tbl_raw)

#Use regex to separate the string into useful columns
int_rate_tbl_raw %>% 
  filter(str_detect(value, "2021")) %>% 
  mutate(value = str_remove(value, "\\[.\\]"), #Removes '[+]'. This allows me to extract 'país' correctly.
         date = lubridate::dmy(str_extract(value, "\\d+/\\d+/\\d{0,4}")), #Extracts date.
         country = str_trim(str_extract(value, "\\D+")), #Extracts country.
         value = str_remove(value, "\\D+\\d+/\\d+/\\d{0,4}"), #Removes country and date.
         value = str_sub(value, 1, 5), #Gets interest rate.
         value = str_remove(value, "%(?=.*)"), #Removes %, and 0 or more characters.
         value = as.numeric(str_replace(value, ",", ".")),  #Replaces ',' with '.'.
         value = round(value, digits = 1),
         country = countryname(country) #Translates country names from Spanish to English.
         ) -> int_rate_tbl

#Visualization
#Vector of colors
c(
  point = "#417A9F",
  bkg = "#F5F5F5",
  text = "#333333",
  family = "Gotham",
  segment = "#999999",
  caption = "#5C5C5C"
) -> colores

int_rate_tbl %>% 
  ggplot(aes(value, 
             fct_reorder(country, value), 
             label = value)) +
  geom_segment(aes(x = -1,
                   xend = value,
                   y = fct_reorder(country, value),
                   yend = country),
               color = colores[["segment"]],
               size = 1.25) +
  geom_point(size = 11,
             color = colores[["point"]]) +
  geom_text(color = colores[["bkg"]],
            family = colores[["family"]],
            size = 3) +
  scale_x_continuous( position = "top") +
  labs(title = "Central Banks Interest Rates",
       subtitle = str_wrap("Changing Interest rates is a monetary policy tool, generally carried out by central banks. When a bank raises interest rates, it increases the cost of borrowing money. When a bank lowers interest rates, it decreases the cost of borrowing money. Here is the central bank interest rate of 15 countries:", width = 60),
       caption = "\nMade by @luisfreii | Source: datosmacro",
       x = "\nCentral Bank Interest Rate (%)",
       y = NULL) + 
  theme(panel.background = element_rect(colores[["bkg"]]),
        plot.background = element_rect(colores[["bkg"]]),
        axis.text = element_text(color = colores[["text"]]),
        panel.grid = element_blank(),
        axis.title.x = element_text(hjust = 0.05, 
                                    family = colores[["family"]], 
                                    color = colores[["text"]]),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(family = colores[["family"]], 
                                   size = 11),
        plot.title = element_text(hjust = 0.05, 
                                  family = "Roboto", 
                                  color = colores[["text"]],
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(family = colores[["family"]], 
                                     color = colores[["text"]]),
        plot.caption = element_text(hjust = 0.05, 
                                  family = colores[["family"]], 
                                  color = colores[["caption"]]))

#Code to save the plot
ggsave("interest.svg",
       width = 15,
       height = 20,
       units = "cm",
       dpi = 320)
     
