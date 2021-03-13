#Load packages
library(tidyverse)
library(ggfittext)

#library(googlesheets4)

#Get the data
read.csv("Anne's Top 50 Books.csv") %>% 
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
  "jane" = "#FF6833"
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

  ggplot(books_df) +
  geom_col(aes(fct_reorder(title, -number_of_pages), number_of_pages + 1500, 
               fill = color)) +
  geom_text(aes(fct_reorder(title, -number_of_pages), number_of_pages + 1555, 
                label = number_of_pages),
            fontface = "bold") +
  geom_text(aes(fct_reorder(title, -number_of_pages), 120, 
                label = author),
            size = 3.5) +
  geom_segment(aes(x = 0, xend = 10.5,
                   y = 1500, yend = 1500),
                   color = "white") +
  scale_fill_identity() +
  scale_y_continuous(breaks = seq(1500, 3000, by = 500), labels = c(0, 500, 1000, 1500)) +
  labs(y = "Number of\npages", x = NULL) +
  theme(axis.title.y = element_text(angle = 0, hjust = 0))
