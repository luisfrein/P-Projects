#Load packages
library(tidyverse)
library(extrafont)
library(scales)
library(lubridate)

#Get the data
btc <- read_csv('btc-usd-max.csv') %>% 
  janitor::clean_names()

eth <- read_csv('eth-usd-max.csv') %>% 
  janitor::clean_names()

ada <- read_csv('ada-usd-max.csv') %>% 
  janitor::clean_names()

#Change class of 'snapped_at' column from character to date
btc %>% 
  mutate(snapped_at = as_date(snapped_at)) -> btc

eth %>% 
  mutate(snapped_at = as_date(snapped_at)) -> eth

ada %>% 
  mutate(snapped_at = as_date(snapped_at)) -> ada

#BTC chart

btc %>% 
  slice(977:3030) %>% 
  ggplot(aes(snapped_at, 1, fill = price)) +
  geom_tile() +
  scale_fill_gradient(low = "#B62F02", 
                      high = "#FFB60A",
                      limits = c(0, 70000),
                      breaks = seq(12000, 60000, 12000),
                      labels = c("12,000", "24,000", "36,000", "48,000", "60,000")) +
  scale_x_date(labels = label_date_short(),
               date_breaks = "4 month") +
  coord_cartesian(expand = FALSE) +
  labs(fill = "Price (USD)",
       y = NULL, 
       x = NULL,
       title = "Bitcoin's Price (USD)",
       subtitle = "Daily Bitcoin price. January 2016 to August 2021.",
       caption = "Made by **@luisfreii** | Source: **CoinGecko**") +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = .5, 
                               barwidth = unit(20, 'lines'), 
                               barheight = unit(.5, 'lines'))) +
  theme(legend.position = "top",
        plot.margin = margin(25, 15, 10, 15),
        panel.background = element_rect(color = "#F5EFFA"),
        plot.background = element_rect(color = "#F5EFFA"),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16,
                                  color = "#525252",
                                  face = "bold",
                                  family = "Fira Sans",
                                  hjust = .5),
        plot.subtitle = element_text(size = 12,
                                     color = "#525252",
                                     margin = margin(t = .2, b = .5, unit = "cm"),
                                     family = "Fira Sans",
                                     hjust = .5),
        plot.caption = ggtext::element_markdown(size = 7,
                                                color = "#525252",
                                                family = "Fira Sans"),
        legend.text = element_text(size = 9,
                                   family = "Fira Sans",
                                   color = "#525252"),
        legend.title = element_text(size = 10,
                                    family = "Fira Sans",
                                    color = "#525252"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 

#Code to save the plot
ggsave("btc.png",
       width = 20,
       height = 10,
       units = "cm",
       dpi = 320,
       type = "cairo-png")

eth %>% 
  slice(147:2200) %>% 
  ggplot(aes(snapped_at, 1, fill = price)) +
  geom_tile() +
  scale_fill_gradient(low = "#4E216E", 
                      high = "#5CEFFF",
                      limits = c(0, 4200),
                      breaks = seq(600, 4000, 600),
                      labels = c("600", "1,200", "1,800", "2,400", "3,000", "3,600")) +
  scale_x_date(labels = label_date_short(),
               date_breaks = "4 month") +
  coord_cartesian(expand = FALSE) +
  labs(fill = "Price (USD)",
       y = NULL, 
       x = NULL,
       title = "Ethereum's Price (USD)",
       subtitle = "Daily Ethereum price. January 2016 to August 2021.",
       caption = "Made by **@luisfreii** | Source: **CoinGecko**") +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = .5, 
                               barwidth = unit(20, 'lines'), 
                               barheight = unit(.5, 'lines'))) +
  theme(legend.position = "top",
        plot.margin = margin(25, 15, 10, 15),
        panel.background = element_rect(color = "#F5EFFA"),
        plot.background = element_rect(color = "#F5EFFA"),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16,
                                  color = "#525252",
                                  face = "bold",
                                  family = "Fira Sans",
                                  hjust = .5),
        plot.subtitle = element_text(size = 12,
                                     color = "#525252",
                                     margin = margin(t = .2, b = .5, unit = "cm"),
                                     family = "Fira Sans",
                                     hjust = .5),
        plot.caption = ggtext::element_markdown(size = 7,
                                                color = "#525252",
                                                family = "Fira Sans"),
        legend.text = element_text(size = 9,
                                   family = "Fira Sans",
                                   color = "#525252"),
        legend.title = element_text(size = 10,
                                    family = "Fira Sans",
                                    color = "#525252"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#Code to save the plot
ggsave("eth.png",
       width = 20,
       height = 10,
       units = "cm",
       dpi = 320,
       type = "cairo-png")

ada %>% 
  slice(147:2200) %>% 
  ggplot(aes(snapped_at, 1, fill = price)) +
  geom_tile() +
  scale_fill_gradient(low = "#9CC6D3", 
                      high = "#1E5EB5",
                      limits = c(0, 2.3),
                      breaks = seq(0.4, 2.28, 0.4),
                      labels = c("0.4", "0.8", "1.2", "1.6", "2")) +
  scale_x_date(labels = label_date_short(),
               date_breaks = "4 month") +
  coord_cartesian(expand = FALSE) +
  labs(fill = "Price (USD)",
       y = NULL, 
       x = NULL,
       title = "Cardano's Price (USD)",
       subtitle = "Daily Cardano price. October 2017 to August 2021.",
       caption = "Made by **@luisfreii** | Source: **CoinGecko**") +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = .5, 
                               barwidth = unit(20, 'lines'), 
                               barheight = unit(.5, 'lines'))) +
  theme(legend.position = "top",
        plot.margin = margin(25, 15, 10, 15),
        panel.background = element_rect(color = "#F5EFFA"),
        plot.background = element_rect(color = "#F5EFFA"),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16,
                                  color = "#525252",
                                  face = "bold",
                                  family = "Fira Sans",
                                  hjust = .5),
        plot.subtitle = element_text(size = 12,
                                     color = "#525252",
                                     margin = margin(t = .2, b = .5, unit = "cm"),
                                     family = "Fira Sans",
                                     hjust = .5),
        plot.caption = ggtext::element_markdown(size = 7,
                                                color = "#525252",
                                                family = "Fira Sans"),
        legend.text = element_text(size = 9,
                                   family = "Fira Sans",
                                   color = "#525252"),
        legend.title = element_text(size = 10,
                                    family = "Fira Sans",
                                    color = "#525252"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#Code to save the plot
ggsave("ada.png",
       width = 20,
       height = 10,
       units = "cm",
       dpi = 320,
       type = "cairo-png")
