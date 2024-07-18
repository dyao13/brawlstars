library("tidyverse")
library("dplyr")
library("ggplot2")

dfraw <- read.csv("brawlstars.csv")

df <- dfraw %>% slice(rep(row_number(), 2))
df <- dfraw %>% filter(Score == 1) %>% mutate(Result = ifelse(Result == "Red", "Blue", "Red"), W.L = ifelse(W.L == "W", "L", "W")) %>% bind_rows(df)

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

winplot <- ggplot(winrates[winrates["games"] >= 6, ], aes(x = brawler, y = wins/games, fill = brawler)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = games), hjust = -0.5, vjust = 0.5) +
  ylim(0, 1) + 
  labs(x = "brawler", y = "winrate", title = "Win Rates per Brawler (with number of games)") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none", 
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)) + 
  coord_flip()
winplot

ggsave("winrates.png", plot = winplot, dpi = 300)

wins <- df %>% filter(W.L == "W")
losses <- df %>% filter(W.L == "L")

wins <- wins %>% mutate(Brawler.1 = if_else(Result == "Blue", Pick.1, Pick.2),
                        Brawler.2 = if_else(Result == "Blue", Pick.4, Pick.3),
                        Brawler.3 = if_else(Result == "Blue", Pick.5, Pick.6)) %>% 
  select(Brawler.1, Brawler.2, Brawler.3)
wins <- bind_cols(c("W"), wins)

losses <- losses %>% mutate(Brawler.1 = if_else(Result == "Red", Pick.1, Pick.2),
                            Brawler.2 = if_else(Result == "Red", Pick.4, Pick.3),
                            Brawler.3 = if_else(Result == "Red", Pick.5, Pick.6)) %>% 
  select(Brawler.1, Brawler.2, Brawler.3)
losses <- bind_cols(c("L"), losses)

ourgames <- bind_rows(wins, losses)
colnames(ourgames) <- c("W/L", "Brawler.1", "Brawler.2", "Brawler.3")

ourbrawlers <- ourgames[c("Brawler.1", "Brawler.2", "Brawler.3")] %>% unlist() %>% unique()

ourwinrates <- data.frame(matrix(0, nrow = length(ourbrawlers), ncol = 3))
rownames(ourwinrates) <- ourbrawlers
colnames(ourwinrates) <- c("wins", "games", "wins/games")

for (i in 1:nrow(ourgames))
{
  ourwinrates[ourgames$Brawler.1[i], "games"] <- ourwinrates[ourgames$Brawler.1[i], "games"] + 1
  ourwinrates[ourgames$Brawler.2[i], "games"] <- ourwinrates[ourgames$Brawler.2[i], "games"] + 1
  ourwinrates[ourgames$Brawler.3[i], "games"] <- ourwinrates[ourgames$Brawler.3[i], "games"] + 1
  
  if (ourgames$"W/L"[i] == "W")
  {
    ourwinrates[ourgames$Brawler.1[i], "wins"] <- ourwinrates[ourgames$Brawler.1[i], "wins"] + 1
    ourwinrates[ourgames$Brawler.2[i], "wins"] <- ourwinrates[ourgames$Brawler.2[i], "wins"] + 1
    ourwinrates[ourgames$Brawler.3[i], "wins"] <- ourwinrates[ourgames$Brawler.3[i], "wins"] + 1
  }
}

ourwinrates["wins/games"] <- ourwinrates["wins"] / ourwinrates["games"]
ourwinrates <- arrange(ourwinrates, wins/games)

write.csv(ourwinrates, "ourwinrates.csv", row.names = TRUE)

ourwinrates$brawler <- rownames(ourwinrates)
ourwinrates$brawler <- factor(ourwinrates$brawler, levels = ourwinrates$brawler)

ourwinplot <- ggplot(ourwinrates[ourwinrates["games"] >= 4, ], aes(x = brawler, y = wins/games, fill = brawler)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = games), hjust = -0.5, vjust = 0.5) +
  ylim(0, 1) + 
  labs(x = "brawler", y = "winrate", title = "Win Rates per Brawler (with number of games)") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none", 
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)) + 
  coord_flip()
ourwinplot

ggsave("ourwinrates.png", plot = ourwinplot, dpi = 300)
