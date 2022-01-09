library(tidyverse)
library(rvest)
library(polite)
library(janitor)

advanced=read_csv("Data/Advanced.csv")
per_game=read_csv("Data/Player Per Game.csv")
team_summaries=read_csv("Data/Team Summaries.csv") %>% mutate(g_total=w+l) %>% 
  rename(tm=abbreviation) %>% select(season,tm,g_total)
teams=advanced %>% filter(season==2022,tm != "TOT") %>% distinct(tm) %>% arrange(tm) %>% pull(tm)

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

#best 5th starter (must have started 40% of a team's games)
best_worst_starter_winners=advanced %>% filter(tm !="TOT",season>1981) %>% left_join(.,per_game) %>%
  left_join(.,team_summaries) %>% mutate(start_percent=gs/g_total) %>% 
  filter(start_percent>=0.5) %>% mutate(mp_per_game=mp/g,.after="mp") %>% 
  arrange(desc(mp_per_game)) %>% group_by(tm,season) %>% slice(1:5) %>% slice_min(vorp) %>% 
  group_by(season) %>% slice_max(vorp,n=5) %>% select(season,player,pos:experience,tm,g_total,gs,mp_per_game,vorp)
#  ungroup() %>% filter(season==2022) %>% select(season,player,pos:experience,tm,g_total,gs,mp_per_game,vorp)


#contract over & under-performance
bbref_bow=bow("https://www.basketball-reference.com/",
              user_agent = "Sumitro Datta",force=TRUE,delay = 10)
contracts=tibble()
for (x in teams) {
  session=nod(bbref_bow,path=paste0("contracts/",x,".html"))
  teams_contracts=scrape(session) %>% html_nodes(xpath='//*[(@id = "contracts")]') %>% 
    html_table() %>% .[[1]] %>% row_to_names(row_number=1) %>% clean_names() %>% 
    filter(age != "",x2021_22 != "") %>% select(player,x2021_22) %>% rename(salary=x2021_22) %>% 
    mutate(salary=parse_number(salary),tm=x)
  contracts=bind_rows(contracts,teams_contracts)
  print(x)
}

View(left_join(contracts,advanced %>% filter(season==2022)) %>% select(player,experience,salary,ws) %>% 
  mutate(ws_per_dollar=abs(ws/salary)))
