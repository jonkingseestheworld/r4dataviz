# install.packages("waffle", repos = "https://cinc.rud.is")
# install.packages("waffle", repos = "https://cinc.rud.is")


library(waffle)
library(dplyr)
library(ggplot2)

data.frame(
  parts = factor(rep(month.abb[1:3], 3), levels=month.abb[1:3]),
  vals = c(10, 25, 36, 6, 14, 40, 30, 20, 10),
  col = rep(c("blue", "black", "red"), 3),
  fct = c(rep("Thing 1", 3),
          rep("Thing 2", 3),
          rep("Thing 3", 3))
) -> xdf

xdf %>%
  count(parts, wt = vals) %>%
  mutate( perc = round(n*100/sum(n))) %>%
  ggplot(aes(fill = parts, values = perc)) +
  geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) 
