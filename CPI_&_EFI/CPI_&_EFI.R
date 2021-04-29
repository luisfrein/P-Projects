#Load packages
library(tidyverse)
library(extrafont)
library(ggtext)
library(ggrepel)

#Read the data. Economic freedom index link: https://www.heritage.org/index/explore?view=by-region-country-year&u=637552502077689344
corruption_perception <- readxl::read_xlsx("CPI2020_GlobalTablesTS_210125.xlsx", skip = 2) %>% 
  select(1:2, 4) %>% 
  janitor::clean_names()

economic_freedom <- read_csv('data.csv') %>% 
  select(1, 3) %>% 
  janitor::clean_names() %>% 
  mutate(overall_score = as.numeric(overall_score)) 

#Join dfs
corruption_and_freedom <- left_join(corruption_perception, economic_freedom, by = c('country' = 'name')) %>% 
  filter(!is.na(overall_score))

#Plot
set.seed(1512)
corruption_and_freedom %>% 
  ggplot(aes(cpi_score_2020, overall_score, label = country, color = cpi_score_2020)) +
  geom_jitter(size = 4, 
              alpha = .75) +
  geom_text_repel(max.overlaps = 5,
                  family = "Fira Sans") +
  scale_color_gradient(low = '#282F3E',
                       high = "#22D37D") +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  labs(y = "Economic Freedom Index (2020)",
       x = "Corruption Perception Index (2020)", 
       title = "Corruption Perception Index\nAnd Economic Freedom Index",
       subtitle = "Countries go from <span style='color:#282F3E;'>**highly corrupt**</span> to <span style='color:#22D37D;'>**very clean**</span>.",
       caption = '<br>Graph: **@luisfreii**<br>Sources: **Transparency International** & **The Heritage Foundation**') +
  theme(plot.margin = margin(25, 15, 10, 15),
        panel.background = element_rect("#F5F5F5"),
        plot.background = element_rect("#F5F5F5"),
        panel.grid.major.y  = element_line(color = "#EBEBEB"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20,
                                  color = "#525252",
                                  face = "bold",
                                  family = "Fira Sans"),
        plot.subtitle = element_markdown(size = 12,
                                         color = "#525252",
                                         margin = margin(t = .2, b = .5, unit = "cm"),
                                         family = "Fira Sans"),
        plot.caption = element_markdown(size = 8,
                                        color = "#525252",
                                        family = "Fira Sans",
                                        hjust = 0),
        axis.title = element_text(color = "#525252",
                                  family = "Fira Sans"),
        axis.text = element_text(color = "#525252",
                                 family = "Fira Sans"),
        axis.ticks.y = element_blank(),
        legend.position = 'none',
        plot.title.position = "plot",
        plot.caption.position = "plot")

#Code to save the plot
# ggsave('CPI_&_EFI.svg',
#        width = 25,
#        height = 15,
#        dpi = 320,
#        units = 'cm')

# ggsave('CPI_&_EFI.png',
#        width = 25,
#        height = 15,
#        dpi = 320,
#        units = 'cm')
