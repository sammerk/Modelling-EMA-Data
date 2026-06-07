# Data Preparation
library(tidyverse)
library(haven)
data_star <- read_sav(
  "https://raw.githubusercontent.com/sammerk/did_data/master/STAR.sav"
)

#codebook::label_browser_static(data_star)

write_sav(
  data_star %>%
    select(
      gktchid,
      g1tchid,
      g2tchid,
      g3tchid,
      g4tchid,
      gktmathss,
      g1tmathss,
      g2tmathss,
      g3tmathss,
      g4tmathss,
      gktreadss,
      g1treadss,
      g2treadss,
      g3treadss,
      g4treadss,
      gkfreelunch,
      g1freelunch,
      g2freelunch,
      g3freelunch,
      g4nfreelunch,
      gkclasssize,
      g1classsize,
      g2classsize,
      g3classsize,
      gkclasstype,
      g1classtype,
      g2classtype,
      g3classtype,
      gkclasstype,
      g1classtype,
      g2classtype,
      g3classtype,
      g1selfconcraw,
      g2selfconcraw,
      g3selfconcraw
    ) %>%
    mutate(
      classID = case_when(
        !is.na(gktchid) ~ gktchid,
        !is.na(g1tchid) ~ g1tchid,
        !is.na(g2tchid) ~ g2tchid,
        T ~ g3tchid
      )
    ),
  "data_star_workshop.sav"
)
