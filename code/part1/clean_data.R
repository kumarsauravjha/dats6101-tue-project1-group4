nfl_data = read.csv("dataset/nfl_players.csv")

nfl_data = nfl_data[, c("position_group", "position", "height", "weight", "years_of_experience", "team_abbr", "entry_year", "rookie_year")]

nfl_data$position_group = as.factor(nfl_data$position_group)
nfl_data$position = as.factor(nfl_data$position)
nfl_data$team_abbr = as.factor(nfl_data$team_abbr)

nfl_data_clean = na.omit(nfl_data)
nfl_data_clean = subset(nfl_data_clean, nfl_data_clean$entry_year >= 2000)

write.csv(nfl_data_clean, "dataset/nfl_players_clean.csv", row.names = F)