library("tidyverse")
library("dplyr")

dfraw <- read.csv("brawlstars.csv")

df <- dfraw %>% slice(rep(row_number(), 2))
df <- dfraw %>% filter(Score == 1) %>% mutate(Result = ifelse(Result == "Red", "Blue", "Red")) %>% bind_rows(df)

allbrawlers <- dfraw[c("Pick.1", "Pick.2", "Pick.3", "Pick.4", "Pick.5", "Pick.6")] %>% unlist() %>% unique()
