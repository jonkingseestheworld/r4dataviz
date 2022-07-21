library(ggplot2)
library(dplyr)
library(tidyr)
#library(ggstream)
library(MetBrewer)
library(patchwork)
library(forcats)


sysfonts::font_add_google(name = "Lora", "Lora")
sysfonts::font_add_google(name = "Kanit", "Kanit")
sysfonts::font_add_google(name = "Roboto", "Roboto")
showtext::showtext_auto()


# read data
technology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv') 


technology <- technology %>%
  mutate(country = countrycode::countrycode(iso3c, origin = "iso3c", destination = 'country.name'))

chile_df <- technology %>%
  filter(iso3c == "CHL", category == "Communications")



labels <- chile_df %>%
  filter(year > 1950, variable != "telephone") %>%
  add_count(label, wt = value) %>%  ## equivalent to groupby(label) %>% mutate( x = sum(y))
  filter(n > 10000000) %>%
  .$label %>% unique()


compare <- data.frame(year = 1951:2020, label = rep(labels, 70)) %>%
  expand(year, label)



for(ctry in c("DEU", "CHL", "USA", "THA", "CAF", "AFG", "IRN", "ISR")) {
  assign(ctry, technology %>%
           filter(iso3c == ctry, category == "Communications") %>%
           filter(year > 1950, variable != "telephone", label %in% labels) %>%
           right_join(compare, by = c("year", "label")) %>%
           group_by(year) %>% 
           mutate(value = value/sum(value, na.rm = TRUE),
                  value = ifelse(is.na(value), 0, value)) %>%
           ggplot(aes(x = year, y = value, fill = label)) +
           geom_area(position = "fill", colour = "black", size = .2) +
           #geom_stream(type = "proportional", bw = 1.5) +
           #geom_stream_label(aes(label = label), bw = 1.5, type = "proportional") +
           scale_fill_manual(values=met.brewer("Nizami")) +
           labs(subtitle = countrycode::countrycode(ctry, origin = "iso3c", destination = "country.name"),
                fill = NULL) +
           theme_void() +
           theme(axis.text.x = element_text(),
                 axis.ticks.x = element_line())) 
}



deu <- technology %>%
  filter(iso3c == "DEU", category == "Communications") %>%
  filter(year > 1950, label %in% labels) %>% 
  right_join(compare, by = c("year", "label")) %>%
  group_by(year) %>% 
  mutate(value = value/sum(value, na.rm = TRUE),
         value = ifelse(is.na(value), 0, value)) %>% 
  ggplot(aes(x = year, y = value, fill = label)) +
  geom_area(position = "fill", colour = "black", size = .2) +
  #geom_stream_label(aes(label = label), bw = 1.5, type = "proportional") +
  scale_fill_manual(values=met.brewer("Nizami")) +
  theme(legend.position = "none")


DEU + CAF + THA + 
  ISR + USA + AFG + 
  IRN + CHL + guide_area() + 
  plot_layout(guides = 'collect', ncol = 3) +
  plot_annotation(title = "Communication technology over time (selected countries)",
                  caption = "Data: NBER (The CHAT dataset: 10.3386/w15319)") &
  theme(text = element_text(family = "Roboto"),
        legend.text = element_text(size = 12),
        plot.subtitle = element_text(size = 15),
        plot.title = element_text(size = 23, family = "Kanit"))



ggsave("wk29_technology/plot.png", width = 10, height = 9)

