# hent supplerende data fra tal og data
library(tidyverse)

files <- dir("data/old_tilm_data_tal_og_data/", full.names = TRUE, recursive = TRUE)


# vi er interesserede i "all_event_details.csv"
# "current_events_details.csv"
# og alt der starter med "event_details"

filer <- tibble(path = files)
gl_tilm_tal_og_data <- filer %>% 
  filter(str_detect(path, "event_details")) %>% 
  mutate(data = map(path, read_csv2)) %>% 
  mutate(rækker = map(data, nrow)) %>% 
  unnest(rækker) %>% 
  filter(rækker > 0) %>% 
  pull(data) %>% 
  bind_rows() 


NB køn her er gemt som en sandsynlighed for at personen er mand. 
Meget lave tal betyder at sandsynligheden taler for at det er en kvinde.
Er tallet meget høj (tæt på 1), at det er en mand.
Tallene i "køn" kolonnen skal derfor kategoriseres. Vi antager at TRUE her matcher
det foretrukne køn. Så hvis tallet er <0.5 skal der returneres TRUE

Bemærk også at vi i den samlede har køn gemt i fornavns kolonnen.