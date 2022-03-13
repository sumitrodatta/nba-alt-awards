library(tidyverse)
library(RSelenium)
library(rvest)
library(polite)
library(janitor)
library(ggdark)

advanced=read_csv("Data/Advanced.csv") %>% filter(season==2022) %>%
  #if player played for multiple teams in season, only take total row
  mutate(tm=ifelse(tm=="TOT","1TOT",tm)) %>% 
  group_by(player_id,season) %>% arrange(tm) %>% slice(1) %>% 
  mutate(tm=ifelse(tm=="1TOT","TOT",tm)) %>% 
  arrange(season,player) %>% ungroup()
per_game=read_csv("Data/Player Per Game.csv") %>% filter(season==2022) %>%
  #if player played for multiple teams in season, only take total row
  mutate(tm=ifelse(tm=="TOT","1TOT",tm)) %>% 
  group_by(player_id,season) %>% arrange(tm) %>% slice(1) %>% 
  mutate(tm=ifelse(tm=="1TOT","TOT",tm)) %>% 
  arrange(season,player) %>% ungroup()
team_summaries=read_csv("Data/Team Summaries.csv") %>% filter(season==2022) %>% mutate(g_total=w+l) %>% 
  rename(tm=abbreviation) %>% select(season,tm,g_total) %>% head(.,-1) %>% add_row(season=2022,tm="TOT",g_total=mean(.$g_total))
teams=advanced %>% filter(season==2022,tm != "TOT") %>% distinct(tm) %>% arrange(tm) %>% pull(tm)
play_by_play=read_csv("Data/Player Play by Play.csv") %>% filter(season==2022)

bbref_bow=bow("https://www.basketball-reference.com/",
              user_agent = "Sumitro Datta",force=TRUE,delay = 10)

#The Real Sixth Man of the Year (presented by Brent Barry)*

#sixth man winners if sixth to ninth on team in minutes played
smoy_candidates=advanced %>% left_join(.,per_game) %>%
  left_join(.,team_summaries) %>% mutate(g_percent=g/g_total) %>% filter(g_percent>=0.5,tm != "TOT") %>% 
  filter(!is.na(mp_per_game)) %>% arrange(desc(mp_per_game)) %>% 
  group_by(tm,season) %>% slice(6:9) %>% ungroup()

smoy_candidates %>% slice_max(vorp,n=5) %>% 
  select(player,pos:experience,tm,g,gs,mp_per_game,vorp)

smoy_candidates %>% slice_max(pts_per_game,n=5) %>% 
  select(player,pos:experience,tm,g,gs,mp_per_game,pts_per_game)

# The Deadshot Award (presented by Ray Allen/Reggie Miller)

#best qualifying 3 point percentage (Basketball-Reference)

per_game %>% left_join(.,team_summaries) %>% mutate(g_percent=g/g_total) %>% filter(g_percent>=0.7,x3p_per_game>=1) %>%
  slice_max(x3p_percent,n=5) %>% select(player,x3p_per_game,x3p_percent)

# The Stormtrooper Award

#worst qualifying 2 point percentage (Basketball-Reference)

per_game %>% left_join(.,team_summaries) %>% mutate(g_percent=g/g_total) %>% filter(g_percent>=0.7,fg_per_game>=300/82) %>%
  slice_min(x2p_percent,n=5) %>% select(player,x2p_per_game,x2p_percent)

# The "If He Dies, He Dies" Award (presented by Tom Thibodeau, sponsored by Ivan Drago)

#most minutes played per game (Basketball-Reference) (credit to FurryCrew for the idea)

per_game %>% left_join(.,team_summaries) %>% mutate(g_percent=g/g_total) %>% filter(g_percent>=0.7) %>%
  slice_max(mp_per_game,n=5) %>% select(player,mp_per_game)

#alternatively: most total minutes played (Basketball-Reference) (credit to FrankEMartindale for the idea)

per_game %>% left_join(.,team_summaries) %>% mutate(g_percent=g/g_total,total_mp=g*mp_per_game) %>% 
  filter(g_percent>=0.7) %>% slice_max(total_mp,n=5) %>% select(player,total_mp)

# The Weakest Link award (sponsored by Jack Link's Beef Jerky, presented by the 2015 Atlanta Hawks Starting 5)*

#best 5th starter (must have started 50% of a team's games) (credit to memeticengineering for the idea)
best_worst_starter_candidates=advanced %>% left_join(.,per_game) %>%
  left_join(.,team_summaries) %>% mutate(start_percent=gs/g_total) %>% 
  filter(start_percent>=0.5) %>% mutate(mp_per_game=mp/g,.after="mp") %>% 
  arrange(desc(mp_per_game)) %>% group_by(tm,season) %>% slice(1:5) %>% slice_min(vorp) %>% ungroup() 

best_worst_starter_candidates %>% slice_max(vorp,n=5) %>% select(player,pos:experience,tm,g_total,gs,mp_per_game,vorp)

# The "This Game Has Always Been, And Will Always Be, About Buckets" Award (https://www.youtube.com/watch?v=-xYejfYxT4s)

per_game %>% left_join(.,team_summaries) %>% mutate(g_percent=g/g_total) %>% filter(g_percent>=0.7) %>% 
  mutate(pts_as_percent_of_other_stats=pts_per_game/(pts_per_game+trb_per_game+ast_per_game+stl_per_game+blk_per_game)) %>% 
  slice_max(pts_as_percent_of_other_stats,n=5) %>% 
  select(player,pts_per_game,trb_per_game,ast_per_game,stl_per_game,blk_per_game,pts_as_percent_of_other_stats)

# The Empty Calorie Stats Award (sponsored by Pop-Tarts)*

# highest percentile rank within position in usage, descending VORP, descending TS% (credit to eewap for the idea)

empty_stats_df=advanced %>% 
  left_join(.,team_summaries) %>% mutate(g_percent=g/g_total) %>% filter(g_percent>=0.5) %>%
  mutate(pos=word(pos,sep="-")) %>% group_by(pos) %>%
    mutate(ts_rank=percent_rank(desc(ts_percent)),
           usg_rank=percent_rank(usg_percent),
           vorp_rank=percent_rank(desc(vorp)),
           empty_stats=ts_rank+usg_rank+vorp_rank) %>% ungroup()

empty_stats_df %>% 
  slice_max(empty_stats,n=5) %>% select(player,pos:experience,ts_percent,usg_percent,vorp,ts_rank:empty_stats)

# The "Can’t Win With These Cats" Award (sponsored by Scar from The Lion King, presented by Kevin Durant in a fake mustache)*

# highest difference in on/off splits between best & median player on team (credit to eewap for the idea)

pbp_filtered=play_by_play %>% filter(tm !="TOT") %>% 
  left_join(.,team_summaries) %>% mutate(g_percent=g/g_total) %>% filter(g_percent>=0.5)

on_off_leaders=pbp_filtered %>% group_by(tm) %>%
  slice_max(net_plus_minus_per_100_poss) %>% ungroup() %>% select(seas_id:player,tm:mp,net_plus_minus_per_100_poss,g_percent)

on_off_median=pbp_filtered %>% group_by(tm) %>% arrange(desc(net_plus_minus_per_100_poss)) %>%
  slice(-1) %>% summarize(median=median(net_plus_minus_per_100_poss))

on_off_diff=left_join(on_off_leaders,on_off_median) %>%  mutate(npm_diff=net_plus_minus_per_100_poss-median)

on_off_diff %>% slice_max(npm_diff,n=5) %>% select(player:npm_diff)

player_heights=tibble()
for (x in teams) {
  session=nod(bbref_bow,path=paste0("teams/",x,"/2022.html"))
  team_heights=scrape(session) %>% html_nodes(css="#all_roster") %>% 
    html_table() %>% .[[1]] %>% 
    clean_names() %>% select(player:wt)
  player_heights=bind_rows(player_heights,team_heights)
  print(x)
}

final_heights=player_heights %>% mutate(player=str_trim(word(player,sep="\\(TW\\)"))) %>%
  separate(ht,into=c("ht_ft","ht_in"),convert=TRUE) %>% mutate(full_in_ht=12*ht_ft+ht_in) %>%
  mutate(player=case_when(str_detect(player,"Walker IV")~"Lonnie Walker",
                          str_detect(player,"Otto")~"Otto Porter",
                          str_detect(player,"Sviato")~"Svi Mykhailiuk",
                          str_detect(player,"Clax")~"Nic Claxton",
                          TRUE~player))

pos_percents_w_heights=play_by_play %>% select(seas_id:player,pos,tm:c_percent) %>% filter(tm!="TOT") %>% 
  replace_na(list(pg_percent=0,sg_percent=0,sf_percent=0,pf_percent=0,c_percent=0)) %>%
  mutate(guard_percent=pg_percent+sg_percent,
         small_wing_percent=sg_percent+sf_percent,
         big_wing_percent=sf_percent+pf_percent,
         big_percent=pf_percent+c_percent) %>% pivot_longer(cols=guard_percent:big_percent) %>%
    group_by(seas_id) %>% slice_max(value,n=1) %>% ungroup() %>% 
    left_join(.,final_heights %>% select(-pos)) %>% 
  mutate(name=case_when(full_in_ht>=84~"big_percent", #evan mobley classified as big_wing
                        full_in_ht<=72~"guard_percent", #markus howard classified as small_wing
                        TRUE~name))

pos_percents_w_heights %>% filter(mp>50) %>% ggplot(aes(x=full_in_ht,fill=name)) + 
  geom_bar() + dark_theme_grey()

write_csv(pos_percents_w_heights,"Player Positions.csv")

# The "Master Baiter" Award (sponsored by Bass Pro Shops & Kleenex)

# most 3-point shooting fouls drawn (PBPStats.com)

driver<- rsDriver(browser=c("firefox"))
remDr <- driver[["client"]]
remDr$open()
remDr$navigate("https://www.pbpstats.com/totals/nba/player?Season=2021-22&SeasonType=Regular%20Season&Type=Player&StatType=Totals&Table=FTs")
Sys.sleep(20)
a<-remDr$findElement(using="xpath",value='//*[contains(concat( " ", @class, " " ), concat( " ", "footer__row-count__select", " " ))]')
a$clickElement()
Sys.sleep(10)
a$clickElement()
b<-remDr$findElement(using="xpath",value="//*/option[@value = '500']")
b$clickElement()

ft_source=read_html(remDr$getPageSource()[[1]]) %>% html_nodes("table") %>% .[[1]] %>% html_table() %>% 
  select(-1) %>% rename_with(.fn=~word(.,1,sep="\n")) %>% slice(-1) %>% clean_names() %>% 
  mutate(three_pt_shooting_fouls_drawn=x3pt_sfd+x3pt_and_1s)

remDr$close()
driver$server$stop()

ft_source %>% slice_max(three_pt_shooting_fouls_drawn,n=5) %>% 
  select(name,x3pt_sfd,x3pt_and_1s,three_pt_shooting_fouls_drawn)

# The Stonks Award

#contract overperformance by fewest contract $ per Win Share (credit to memeticengineering for the idea)
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

salary_performance=left_join(contracts,read_csv("Data/Advanced.csv") %>% filter(season==2022)) %>% 
  select(player,experience,age,salary,vorp) %>% 
  mutate(vorp_per_million=vorp/salary*1000000)
#remove any player with <=4 years of experience (rookie contract)
salary_performance %>% filter(experience > 4) %>% arrange(desc(vorp_per_million))
#remove minimum salaried players
salary_performance %>% 
  group_by(age) %>% mutate(min_salary=(salary==min(salary))) %>% ungroup() %>% 
  filter(min_salary==FALSE) %>%
  filter(experience > 4) %>% arrange(desc(vorp_per_million))
#remove players whose salary is less than 5% of cap
salary_performance %>% mutate(percent_of_cap=salary/112414000) %>% filter(percent_of_cap>0.05) %>%
  filter(experience > 4) %>% arrange(desc(vorp_per_million))

options(scipen=999)
