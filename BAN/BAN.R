#Load packages
library(tidyverse)
library(extrafont)
library(gganimate)
library(scales)
library(ggimage)
library(lubridate)

#Get the data. 
banano <- read_csv("ban-usd-max.csv") %>% 
  janitor::clean_names() 

#Add doge image
banano$image <- 'BAN.png'

#Plot
banano %>% 
  mutate(snapped_at = as_date(snapped_at)) %>% 
  ggplot(aes(snapped_at, price)) +
  geom_line(size = 1,
            color = "#EADA2C") +
  labs(y = 'Price (USD)', x = NULL,
       title = 'The Rise of Banano',
       subtitle = 'Daily Banano price from November 3rd, 2018 to May 12th, 2021.',
       caption = '<br>Visualization: @luisfreii | Source: CoinGecko') + 
  theme(plot.title = element_text(family = 'IBM Plex Sans',
                                  size = 28,
                                  color = "#EADA2C",
                                  face = 'bold'),
        plot.subtitle = element_text(family = 'Fira Sans',
                                     color = "#EADA2C"),
        plot.caption = ggtext::element_markdown(family = 'Fira Sans',
                                                size = 9,
                                                color = "#EADA2C"),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = '#6D6779'),
        plot.background = element_rect('#46424D'),
        panel.background = element_rect('#46424D'),
        axis.title = element_text(color = "#EADA2C",
                                  family = "Fira Sans"),
        axis.text = element_text(color = "#EADA2C",
                                 family = "Fira Sans"),
        plot.margin = margin(25, 15, 10, 15),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = '#B6B2BD')) -> p1

#Add animation. Data appears gradually.
a1 <- p1  + geom_image(aes(image = image), size = 0.05) + 
  transition_reveal(snapped_at) 

anim <- animate(a1, 
                nframes = 40, end_pause = 10,
                fps = 10, 
                height = 12, width = 15, units = "cm", res = 150)

#Code to save the animation
#anim_save("BAN.gif", animation = last_animation())
