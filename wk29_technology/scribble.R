

abc <- technology %>%
  filter(category == "Energy",
         year > 1950,
         iso3c %in% c("DEU", "CHL", "USA", "THA", "CAF", "AFG", "IRN", "ISR"))

abc <- abc %>%
  mutate(
    period = case_when( year < 2000 ~ "1900s",
                        year >= 2000 ~ "2000s")
  ) %>%
  group_by( variable, period, country ) %>%
  summarise( med_value = median(value) ) %>%
  ungroup()


abc %>%
  group_by(country, period) %>%
  #summarise( sum = sum()) %>%
  mutate( sum = sum(med_value) ,
            perc = med_value/sum(med_value) ) %>%
 # arrange( country, period) 
  ggplot(aes(x = variable, y = perc)) +
  geom_bar( aes(group = period), stat = "identity" ) +
  facet_wrap(~country, ncol = 4)
