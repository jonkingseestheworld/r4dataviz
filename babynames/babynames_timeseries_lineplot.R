# plot of gender neutral babynames over time

# remotes::install_version("Rttf2pt1", version = "1.3.8") # needed for windows machine
# install.packages("extrafont")
# install.packages("geomtextpath")

library(extrafont)
library(tidyverse)
library(geomtextpath)
library(MetBrewer)

# loadfonts(device = "win")

babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

names_keep <- babynames %>% 
  mutate(sex_full = case_when(
    sex == "F" ~ "female",
    sex == "M" ~ "male"
  )) %>% 
  pivot_wider(id_cols = c(year, name), values_from = n, names_from = sex_full) %>% 
  mutate(
    total = female + male,
    ratio = female/male,
    logratio = log10(ratio)) %>% 
  filter(!is.na(logratio),  
         #logratio <= 4, logratio >= -4,
         total >= 1000,
         female >= 100, male >= 100)

names_highlight <- names_keep %>% 
  filter(abs(logratio) < 1) %>% 
  group_by(name) %>%
  summarize( n_years = length(unique(year)), 
             med_year = round(median(year))
  ) %>% 
  arrange(desc(n_years)) %>% 
  head(10) %>% 
  left_join(names_keep %>% select(name, med_year = year, ratio)) %>% 
  mutate(name = fct_reorder(name, ratio))

names_keep$highlight <- NA
names_keep$highlight[names_keep$name %in% names_highlight$name] <- "Top 10" 

ggplot() +
  geom_hline(yintercept = 1, size = 0.25) +
  geom_line(data = names_keep %>% filter(is.na(highlight) ), 
            aes(x = year, y = ratio, group = name), 
            size =.2, alpha = .5, color = "#333333",
  ) +
  geom_line(data = names_keep %>% filter(highlight == "Top 10") %>%
              mutate(name = factor(name, levels = levels(names_highlight$name))), 
            aes(x = year, y = ratio, group = name, color = name), 
            size = 2.5, alpha = 1 ) +
  geom_label(data = names_highlight, 
             aes(x = med_year, y = ratio, group = name, 
                 label = name, fill = name), color = "#ffffff", 
             size = 6, alpha = 1 ) +
  scale_x_continuous(breaks = seq(1880, 2020, 10), expand = c(0.015, 0)) +
  scale_color_manual(values = met.brewer("Redon", 10)) +
  scale_fill_manual(values = met.brewer("Redon", 10)) +
  scale_y_log10(breaks = c(.001, 0.01, 0.1, 1, 10, 100, 1000) , 
                labels = c("1000 times\nmore boys", "100 times\nmore boys", "10 times\nmore boys", 
                           "Equal number of\ngirls and boys", 
                           "10 times\nmore girls", "100 times\nmore girls", "1000 times\nmore girls")) +
  labs(
    title = "Historical Top 10 of Gender-Neutral Baby Names in the U.S.",
    subtitle = paste0("Gender ratios of ", length(unique(names_keep$name)), 
                      " names commonly given to both boys and girls annually*, 1880 to 2017."),
    caption = paste0("Source: babynames R package | Chart by @ChampMath, 2022.\n",
                     "*Minimum 1000 annual U.S. births, including 100 boys and 100 girls.\n",
                     "Top 10 criterion = most years within 1 to 10 ratios.")
  ) +
  coord_cartesian() +
  theme_minimal(base_family = "Arial", base_size = 20) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.y = element_text(size = 16),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    plot.title = element_text( size = 28),
    plot.subtitle = element_text(size = 20, margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(margin = margin(20, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid = element_line(color = "#eeeeee", size = .5)
  )


ggsave("babynames/gender_neutral.png", #device = ragg::agg_png, 
       width = 14, height = 10, units = "in", dpi = 320)
