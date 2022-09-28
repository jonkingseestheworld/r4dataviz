# TidyTuesday | September 27, 2022 | Week 39
# Data source is arts.gov by way of Data is Plural



# load libraries ---------------------------------------------------------------
library(tidyverse)
library(circlize)


library(showtext)
library(scales)

# add font ----------------------------------------------------------------
font_add_google("Cabin Sketch", "cabin")
font_add_google("Lato", "lato")

showtext_auto()


# load data
artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv')


# data preprocessing

artists_by_ethn <- artists %>%
  group_by(race, type) %>%
  summarise( artists_n = sum(artists_n, na.rm=T)  ) %>%
  ungroup() %>%
  mutate( 
    category = case_when(type %in% c("Actors", "Announcers", "Dancers And Choreographers" ,
                                     "Entertainers", "Music Directors And Composers",
                                     "Musicians", "Producers And Directors") ~ "Entertainment",
                         T ~ "Others") 
  ) 

#chordDiagram(artists_by_ethn)

## transform data to wide, matrix format
entertainer_by_ethn_wide <-
  artists_by_ethn %>% 
  filter(category == "Entertainment") %>%
  mutate(
    race = case_when(grepl("African-American", race) ~ "Afri-Am",
                     T ~ race)
  ) %>%
  mutate(
    type = case_when(grepl("Announcers", type) ~ "Announ" ,
                     grepl("Dancers And Choreographers", type) ~ "Dancers+" ,
                     grepl("Entertainers", type) ~ "Entertain" ,
                     grepl("Music Directors And Composers" , type) ~ "Composers+",
                     grepl("Producers And Directors" , type) ~ "Prod/Dir",
                     T ~ type
                     )
  ) %>%
  pivot_wider( names_from = type, values_from = artists_n ) %>%
  column_to_rownames(var = "race")   %>%
  select(-category) %>%
  as.matrix()


# plotting

circos.par(start.degree = 270)
#circos.axis(labels=F, major.tick = F)
chordDiagram(entertainer_by_ethn_wide, transparency = 0.2,
             annotationTrack = c("name", "grid"),
             preAllocateTracks = 1, 
             direction.type = c("diffHeight", "arrows"),
             )   
#, big.gap=30, , start.degree = 85, clock.wise=F
# directional = 0
circos.clear()




#               grepl("^Architects", type) ~ "Archit" ,
#               grepl("Fine Artists, Art Directors, And Animators", type) ~ "F-Artists+",
#               grepl("Landscape Architects", type) ~ "L-Archit",

#               grepl("Photographers"  , type) ~ "Photogph",
#               grepl("Writers And Authors" , type) ~ "Authors+",
