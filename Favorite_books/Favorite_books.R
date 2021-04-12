#Load packages
library(tidyverse)
library(ggfittext)
library(extrafont)

#Get the data
read.csv("A's Top 50 Books.csv") %>% 
  janitor::clean_names() -> books

#Set text encoding
Encoding(books$title) <- "UTF-8"
Encoding(books$author) <- "UTF-8"

#Vector with plot colors, and font family
c(
  "miserables" = "#90070F",
  "crime" = "#0E2A3F",
  "expectations" = "#406843",
  "tangere" = "#660B10",
  "villette" = "#7B617E",
  "crooked" = "#181818",
  "light_cant" = "#006B7C",
  "passion" = "#384533",
  "crows" = "#586875",
  "jane" = "#FF6833",
  "text" = "#F5DBCC",
  "bkg" = "#D9BFC4"
) -> colores


#Longest books
books %>% 
  mutate(author = str_replace(author, pattern = " ", replacement = "\n"),
         color = case_when(title == "Les Misérables" ~ colores[["miserables"]],
                           title == "Crime and Punishment" ~ colores[["crime"]],
                           title == "Great Expectations" ~ colores[["expectations"]],
                           title == "Noli me Tangere" ~ colores[["tangere"]],
                           title == "Villette" ~ colores[["villette"]],
                           title == "Crooked Kingdom" ~ colores[["crooked"]],
                           title == "All the Light We Cannot See" ~ colores[["light_cant"]],
                           title == "The Passion of Dolssa" ~ colores[["passion"]],
                           title == "Six of Crows" ~ colores[["crows"]],
                           title == "Jane Eyre" ~ colores[["jane"]])) %>% 
  arrange(-number_of_pages) %>% 
  slice_head(n = 10) -> books_df

#Tibble with horizontal lines
h_lines <- tibble(
  x =  c(.55, 1.45, 1.55, 2.45, 2.55, 3.45, 3.55, 4.45, 4.55, 5.45, 5.55, 6.45, 6.55, 7.45, 7.55, 8.45, 8.55, 9.45, 9.55, 10.45),
  y = 1500,
  z = rep(1:10, each = 2)) 

#Tibble with vertical lines
v_lines <- tibble(
  x = c(5.86, 5.86, 6.13, 6.13, 8.87, 8.87, 9.13, 9.13),
  y = c(575, 1400, 575, 1400, 760, 1400, 760, 1400),
  z = rep(1:4, each = 2)
)

#Plot!
  ggplot() +
  geom_col(books_df,
           mapping = aes(fct_reorder(title, -number_of_pages), number_of_pages + 1500, 
                         fill = color)) +
  geom_text(books_df,
            mapping = aes(fct_reorder(title, -number_of_pages), number_of_pages + 1555, 
                          label = number_of_pages),
            fontface = "bold",
            color = "#9E4842") +
  geom_text(books_df,
            mapping = aes(fct_reorder(title, -number_of_pages), 170, 
                          label = author),
            size = 3.5,
            color = colores[["text"]],
            fontface = "bold") +
  geom_line(h_lines, mapping = aes(x = x, y = y, group = z),
            color = colores[["text"]]) +
  geom_line(h_lines, mapping = aes(x = x, y = 280, group = z),
            color = colores[["text"]]) +
  geom_line(v_lines, mapping = aes(x = x, y = y, group = z),
            size = 1,
            color = c("#EDD4D4", "#EDD4D4", "#DBA9A9", "#DBA9A9", "#EDD4D4", "#EDD4D4", "#DBA9A9", "#DBA9A9")) +
  annotate("text", 
           x = 1, y = 1400, 
           label = "Les\nMisérables", 
           hjust = .5,
           family = "Caslon Antique",
           size = 6,
           color = colores[["text"]],
           vjust = 1) +
  annotate("text", 
           x = 2, y = 1400, 
           label = "Crime and\nPunishment", 
           hjust = 0,
           angle = 270,
           family = "Trajan Pro",
           size = 5,
           color = colores[["text"]]) +
  annotate("text", 
           x = 3, y = 1400, 
           label = "Great\nExpectations", 
           hjust = .5,
           family = "Berkeley Oldstyle",
           size = 3.5,
           color = colores[["text"]],
           vjust = 1) +
  annotate("text", 
           x = 4, y = 1400, 
           label = "Noli Me Tangere", 
           hjust = 0,
           family = "StempelGaramond LT Roman",
           size = 6,
           angle = 270,
           color = colores[["text"]]) +
  annotate("text", 
           x = 5, y = 1400, 
           label = "Vilette", 
           hjust = .5,
           family = "StempelGaramond LT Roman",
           size = 6.5,
           color = colores[["text"]],
           vjust = 1) +
  annotate("text", 
           x = 6, y = 1400, 
           label = "CROOKED KINGDOM", 
           hjust = 0,
           angle = 270,
           family = "WanoQuin-Thin Free PU Thin",
           size = 6,
           color = colores[["text"]]) +
  annotate("text", 
           x = 7, y = 1400, 
           label = "All The Light\nWe Cannot See", 
           hjust = 0,
           angle = 270,
           family = "GuessSansW00-Heavy",
           size = 5,
           color = colores[["text"]]) +
  annotate("text", 
           x = 8, y = 1400, 
           label = "the\nPASSION\nof\nDOLSSA", 
           hjust = .5,
           family = "Astaire",
           size = 5,
           color = colores[["text"]],
           vjust = 1) +
  annotate("text", 
           x = 9, y = 1400, 
           label = "SIX OF CROWS", 
           hjust = 0,
           angle = 270,
           family = "WanoQuin-Thin Free PU Thin",
           size = 6,
           color = colores[["text"]]) +
  annotate("text", 
           x = 10, y = 1400, 
           label = "JANE EYRE", 
           hjust = 0,
           family = "StempelGaramond LT Roman",
           size = 6,
           angle = 270,
           color = colores[["text"]]) +
  annotate("text", 
           x = 4.9, y = 2700, 
           label = "Anne’s Bookshelf", 
           hjust = 0,
           family = "IBM Plex Sans",
           size = 18,
           color = "#9E4842") +
  annotate("text", 
           x = 4.9, y = 2500, 
           label = "These are the longest books on Anne’s favorites list (as of 2021).", 
           hjust = 0,
           family = "IBM Plex Sans",
           size = 4,
           color = "#9E4842") +
  geom_polygon(mapping = aes(x = c(0.25, 0.25, 10.75, 10.75),
                             y = c(0, -150, -150, 0)),
               fill = "#CC8B86") +
  geom_polygon(mapping = aes(x = c(0.4, 0.4, 10.4, 10.4),
                             y = c(-150, -250, -250, -150)),
               fill = "#7D4F50") +
  geom_polygon(mapping = aes(x = c(2, 2, 3, 3),
                             y = c(-250, -600, -600, -250)),
                 fill = "#7D4F50") +
  geom_polygon(mapping = aes(x = c(9, 9, 8, 8),
                             y = c(-250, -600, -600, -250)),
                 fill = "#7D4F50")  +
  geom_polygon(mapping = aes(x = c(2, 2, 3, 3),
                             y = c(-250, -400, -400, -250)),
               fill = "#583738")  +
  geom_polygon(mapping = aes(x = c(9, 9, 8, 8),
                             y = c(-250, -400, -400, -250)),
               fill = "#583738")  +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_fill_identity() +
  scale_y_continuous(breaks = seq(1500, 3000, by = 500), labels = c(0, 500, 1000, 1500)) +
  labs(y = "Number of\npages", x = NULL) +
  theme(axis.title.y = element_text(angle = 0, 
                                    hjust = 0,
                                    color = "#9E4842",
                                    family = "IBM Plex Sans"),
        plot.background = element_rect(colores[["bkg"]]),
        panel.background = element_rect(colores[["bkg"]]),
        panel.grid = element_blank(),
        axis.text.y = element_text(color = "#9E4842",
                                   family = "IBM Plex Sans"),
        axis.ticks.y = element_line(color = "#9E4842"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
  
#Code to save the plot
  # ggsave("A's_books.png",
  #        width = 30,
  #        height = 20,
  #        dpi = 320,
  #        units = "cm")
  # 
  # ggsave("A's_books.svg",
  #        width = 30,
  #        height = 20,
  #        dpi = 320,
  #        units = "cm")
