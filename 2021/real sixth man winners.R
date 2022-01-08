library(tidyverse)

advanced=read_csv("Advanced.csv")
per_game=read_csv("Player Per Game.csv")
team_summaries=read_csv("Team Summaries.csv") %>% mutate(g_total=w+l) %>% 
  rename(tm=abbreviation) %>% select(season,tm,g_total)

#sixth man winners if sixth to ninth on team in minutes played
smoy_winners=advanced %>% filter(tm !="TOT") %>% left_join(.,per_game) %>%
  left_join(.,team_summaries) %>% mutate(g_percent=g/g_total) %>% filter(g_percent>=0.5) %>% 
  mutate(mp_per_game=mp/g,.after="mp") %>% arrange(desc(mp_per_game)) %>% 
  group_by(tm,season) %>% slice(6:9) %>% group_by(season) %>% slice_max(vorp,n=5) %>% 
  select(season,player,pos:experience,tm,g,gs,mp_per_game,vorp)

smoy_winners_ppg=per_game %>% filter(tm !="TOT") %>% 
  left_join(.,team_summaries) %>% mutate(g_percent=g/g_total) %>% filter(g_percent>=0.5) %>%
  filter(!is.na(mp_per_game)) %>% arrange(desc(mp_per_game)) %>% group_by(tm,season) %>% 
  slice(6:9) %>% group_by(season) %>% slice_max(pts_per_game,n=5) %>% 
  select(season,player,pos:experience,tm,g,mp_per_game,pts_per_game)
