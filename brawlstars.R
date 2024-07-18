library("tidyverse")
library("dplyr")
library("ggplot2")

dfraw <- read.csv("brawlstars.csv")

df <- dfraw %>% slice(rep(row_number(), 2))
df <- dfraw %>% filter(Score == 1) %>% mutate(Result = ifelse(Result == "Red", "Blue", "Red")) %>% bind_rows(df)

allbrawlers <- dfraw[c("Pick.1", "Pick.2", "Pick.3", "Pick.4", "Pick.5", "Pick.6")] %>% unlist() %>% unique()
numbrawlers <- length(allbrawlers)

blue <- df[c("Pick.1", "Pick.4", "Pick.5")]
red <- df[c("Pick.2", "Pick.3", "Pick.6")]
games <- bind_cols(df["Result"], blue, red)

winrates <- data.frame(matrix(0, nrow =  numbrawlers, ncol = 3))
rownames(winrates) <- allbrawlers
colnames(winrates) <- c("wins", "games", "wins/games")

for (i in 1:(nrow(games)))
{
  winrates[games$Pick.1[i], "games"] <- winrates[games$Pick.1[i], "games"] + 1
  winrates[games$Pick.4[i], "games"] <- winrates[games$Pick.4[i], "games"] + 1
  winrates[games$Pick.5[i], "games"] <- winrates[games$Pick.5[i], "games"] + 1
  
  winrates[games$Pick.2[i], "games"] <- winrates[games$Pick.2[i], "games"] + 1
  winrates[games$Pick.3[i], "games"] <- winrates[games$Pick.3[i], "games"] + 1
  winrates[games$Pick.6[i], "games"] <- winrates[games$Pick.6[i], "games"] + 1
  
  if (games$Result[i] == "Blue")
  {
    winrates[games$Pick.1[i], "wins"] <- winrates[games$Pick.1[i], "wins"] + 1
    winrates[games$Pick.4[i], "wins"] <- winrates[games$Pick.4[i], "wins"] + 1
    winrates[games$Pick.5[i], "wins"] <- winrates[games$Pick.5[i], "wins"] + 1
  }
  else
  {
    winrates[games$Pick.2[i], "wins"] <- winrates[games$Pick.2[i], "wins"] + 1
    winrates[games$Pick.3[i], "wins"] <- winrates[games$Pick.3[i], "wins"] + 1
    winrates[games$Pick.6[i], "wins"] <- winrates[games$Pick.6[i], "wins"] + 1
  }
}

winrates["wins/games"] <- winrates["wins"] / winrates["games"]
winrates <- arrange(winrates, wins/games)

winrates$brawler <- rownames(winrates)
winrates$brawler <- factor(winrates$brawler, levels = winrates$brawler)

write.csv(winrates, "winrates.csv", row.names = TRUE)

winplot <- ggplot(winrates, aes(x = brawler, y = wins/games, fill = brawler)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = games), hjust = -0.5, vjust = 0.5) +
  labs(x = "brawler", y = "winrate", title = "Win Rates per Brawler (with number of games)") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none", 
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)) + 
  coord_flip()

ggsave("winrates.png", plot = winplot, dpi = 300)
