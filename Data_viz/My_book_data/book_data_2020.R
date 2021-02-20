#Load packages
library(tidyverse)
library(extrafont)
library(scales)
library(ggfittext)
library(patchwork)
library(ggtext)

#Read data
read.csv("Books_read - Hoja 1.csv") %>% 
  janitor::clean_names() -> books

#Explore!
books %>% 
  mutate(rating_0_to_10 = str_replace(rating_0_to_10, ",", "."),
         rating_0_to_10 = as.numeric(rating_0_to_10),
         month_date = lubridate::dmy(case_when(month_finished_read == "March" ~ "01/03/2020",
                                                      month_finished_read == "April" ~ "01/04/2020",
                                                      month_finished_read == "May" ~ "01/05/2020",
                                                      month_finished_read == "June" ~ "01/06/2020",
                                                      month_finished_read == "July" ~ "01/07/2020",
                                                      month_finished_read == "August" ~ "01/08/2020",
                                                      month_finished_read == "September" ~ "01/09/2020",
                                                      month_finished_read == "October" ~ "01/10/2020",
                                                      month_finished_read == "November" ~ "01/11/2020",
                                                      month_finished_read == "December" ~ "01/12/2020"))) %>% 
  filter(year_read == 2020) -> books_2020 

remove(books)

background <- "#121317"
accent <- "#F45B69"
lines <- "#49525A"
text <- "#DCE9F9"

books_2020 %>% 
  select(book, author, genre) %>% 
  map_df(n_distinct) -> auxiliar_text

auxiliar_text$total_words <- sum(books_2020$length_in_words)

#Create theme
book_theme <- function() {
  theme(plot.background = element_rect(fill = background,
                                       color = NA),
        panel.background = element_rect(fill = background,
                                        color = NA),
        panel.grid = element_blank(),
        axis.text = element_text(color = text, face = "bold", family = "Tw Cen MT", size = 12),
        axis.title = element_text(color = text, family = "Tw Cen MT", size = 14))
}

#cumsum plot
cumsum_plot <- books_2020 %>% 
  group_by(month_finished_read, month_date) %>% 
  summarise(book_num = n_distinct(book),
            words_read = sum(length_in_words)) %>% 
  mutate(month_finished_read = factor(month_finished_read, levels = month.name)) %>% 
  ungroup() %>% 
  arrange(month_date) %>% 
  mutate(sum = cumsum(words_read)) %>% 
  ggplot() +
  geom_step(aes(month_date, sum),
            color = "#34A0A4",
            size = 1.25) +
  geom_line(aes(month_date, words_read),
            color = "#99D98C",
            size = 1.25) +
  scale_x_date(date_breaks = "month", labels = label_date_short()) +
  scale_y_continuous(labels = label_number(scale = 1/1000000, suffix = "M"),
                     breaks = pretty_breaks(n = 5)) +
  labs(y = "Words read (Millions)", x = NULL) +
  theme(panel.grid.major.y = element_line(color = lines),
        plot.background = element_rect(color = NA),
        panel.background = element_rect(color = NA))

#Avg rating of authors  
authors_plot <- books_2020 %>% 
  add_count(author) %>% 
  filter(n > 1) %>% 
  group_by(author) %>% 
  summarise(avg_rating = mean(rating_0_to_10)) %>% 
  mutate(author = `Encoding<-`(author, "UTF-8")) %>% 
  ggplot(aes(avg_rating, fct_reorder(author, avg_rating), label = round(avg_rating, digits = 1))) +
  geom_segment(aes(y = fct_reorder(author, avg_rating),
                   yend = author,
                   x = 0,
                   xend = avg_rating),
               color = lines,
               size = 1) +
  geom_point(size = 8.5,
             color = "#D9ED92") +
  geom_text(color = background,
            family = "Tw Cen MT") +
  labs(y = NULL, x = "Average Book Rating") +
  theme(axis.title.x = element_text(color = text, family = "Tw Cen MT", size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_rect(color = NA),
        panel.background = element_rect(color = NA))
  
#Favorite Genres
genres_plot <- books_2020 %>% 
  mutate(genre = case_when(genre == "Data Science" ~ "Non-Fiction",
                           genre == "Zombies/Apocalyptic" ~ "Apocalyptic",
                           genre == "Historical Fiction" ~ "Fiction",
                           genre == "Magical Realism" ~ "Fiction",
                           genre == "Dystopia" ~ "Fiction",
                           genre == "Young Adult" ~ "Drama",
                           TRUE ~ genre),
         color = case_when(genre == "Adventure" ~ "#1A759F",
                           genre == "Non-Fiction" ~ "#99D98C",
                           genre == "Science Fiction" ~ "#168AAD",
                           genre == "Fantasy" ~ "#34A0A4",
                           genre == "Fiction" ~ "#52B69A",
                           genre == "Drama" ~ "#B5E48C",
                           genre == "Apocalyptic" ~ "#D9ED92",
                           genre == "Philosophy" ~ "#76C893")) %>% 
  group_by(genre, color) %>% 
  summarise(total = n_distinct(book)) %>% 
  ggplot(aes(1, total, group = fct_reorder(genre, total), label = paste0(genre, "\n", total), fill = color)) +
  geom_col(position = "stack") +
  geom_bar_text(size = 14,
                position = "stack",
                family = "Tw Cen MT") +
  scale_fill_identity() +
  labs(y = "Genre\nNumber of books") +
  theme(plot.background = element_rect(color = NA),
        panel.background = element_rect(color = NA),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Text plots
#Text theme
text_theme <- function() {
  theme(plot.background = element_rect(color = NA),
        panel.background = element_rect(color = NA),
        plot.margin = margin(0, 1.2, 0, 0, unit = "cm"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
}

#Text next to authors plot.
author_text <- ggplot() +
  geom_richtext(aes(x = 0, y = 2,
                label = paste0("<span style='color:#DCE9F9;font-size:35px;'>", auxiliar_text[2], "</span>","<span style='color:#DCE9F9'>", " authors including<br>",  books_2020[7, 2], ",<br>", books_2020[11, 2], ",<br>", books_2020[37, 2], "<br>and many<br> others.</span>")),
                family = "Tw Cen MT",
                fontface = "bold",
                hjust = 0,
                size = 5.5,
                fill = background,
                color = background) +
  scale_x_continuous(limits = c(0, 3), expand = c(0, 0)) +
  text_theme() 

#Text next to word count plot
word_text <- ggplot() +
  geom_richtext(aes(x = 0, y = 2,
                    label = paste0("<span style='color:#DCE9F9;font-size:35px;'>", round(auxiliar_text[4] / 1000000, digits = 1), "</span>","<span style='color:#DCE9F9'>", " million words across<br>10 months. You can see<br>the</span><span style='color:#34A0A4'> cumulative sum</span><span style='color:#DCE9F9'> and<br>the</span><span style='color:#99D98C'> monthly count.</span>")),
                family = "Tw Cen MT",
                fontface = "bold",
                hjust = 0,
                size = 5.5,
                fill = background,
                color = background) +
  scale_x_continuous(limits = c(0, 3), expand = c(0, 0)) +
  text_theme() +
  theme(plot.margin = margin(0, 0, 0, 1.2, unit = "cm"))

#Text alongside genre plot
genre_text <- ggplot() +
  geom_richtext(aes(x = 0, y = 2,
                    label = paste0("<span style='color:#DCE9F9;font-size:35px;'>", auxiliar_text[3], "</span>","<span style='color:#DCE9F9'>", " genres including ",  books_2020[7, 9], ",<br>", books_2020[11, 9], ", ", books_2020[3, 9], " and many<br>others.</span>")),
                family = "Tw Cen MT",
                fontface = "bold",
                hjust = 0,
                size = 5.5,
                fill = background,
                color = background) +
  scale_x_continuous(limits = c(0, 3), expand = c(0, 0)) +
  text_theme() 

patchwork <- (author_text | authors_plot) / (cumsum_plot | word_text) / (genre_text | genres_plot) & book_theme()

patchwork +
  plot_annotation(
    title = "My book stats 2020",
    subtitle = paste0("<span style='color:#DCE9F9'>In 2020 I set a goal for myself: to read 30 books. I ended<br>up reading</span> <span style='color:#B5E48C;'>", auxiliar_text[1], "</span><span style='color:#DCE9F9'> books. Here's a summary of my year:</span>"),
    caption = "Visualization: **@luisfreii** | Source: **@luisfreii**") &
  theme(plot.title = element_markdown(color = text, hjust = 0.5, size = 30, family = "Tw Cen MT",
                                      margin = margin(0.5, 0, 0.5, 0, unit = "cm")),
        plot.subtitle = element_markdown(color = text, hjust = 0.5, size = 15, family = "Tw Cen MT"),
        plot.caption = element_markdown(color = text, hjust = 0.5, size = 10, family = "Tw Cen MT"))

ggsave("book_summary.png",
       width = 9.27,
       height = 11.69,
       dpi = 320,
       type = "cairo-png")


