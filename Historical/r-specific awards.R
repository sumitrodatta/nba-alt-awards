library(tidyverse)
library(rvest)
library(polite)
library(janitor)

advanced=read_csv("Data/Advanced.csv") %>%
  #if player played for multiple teams in season, only take total row
  mutate(tm=ifelse(tm=="TOT","1TOT",tm)) %>% 
  group_by(player_id,season) %>% arrange(tm) %>% slice(1) %>% 
  mutate(tm=ifelse(tm=="1TOT","TOT",tm)) %>% 
  arrange(season,player) %>% ungroup()
per_game=read_csv("Data/Player Per Game.csv") %>%
  #if player played for multiple teams in season, only take total row
  mutate(tm=ifelse(tm=="TOT","1TOT",tm)) %>% 
  group_by(player_id,season) %>% arrange(tm) %>% slice(1) %>% 
  mutate(tm=ifelse(tm=="1TOT","TOT",tm)) %>% 
  arrange(season,player) %>% ungroup()
totals=read_csv("Data/Player Totals.csv") %>%
  #if player played for multiple teams in season, only take total row
  mutate(tm=ifelse(tm=="TOT","1TOT",tm)) %>% 
  group_by(player_id,season) %>% arrange(tm) %>% slice(1) %>% 
  mutate(tm=ifelse(tm=="1TOT","TOT",tm)) %>% 
  arrange(season,player) %>% ungroup()

career_totals=totals %>% group_by(player_id,player) %>% 
  summarize(first_seas=min(season),last_seas=max(season),
            across(c(g:fga,x3p:x3pa,x2p:x2pa,ft:fta,orb:pts),~sum(.,na.rm=TRUE))) %>% ungroup() %>%
  mutate(fg_percent=ifelse(fga==0,0,fg/fga),
         x3p_percent=ifelse(x3pa==0,0,x3p/x3pa),
         x2p_percent=ifelse(x2pa==0,0,x2p/x2pa),
         ft_percent=ifelse(fta==0,0,ft/fta))

team_wins_losses=read_csv("Data/Team Summaries.csv") %>% mutate(g_total=w+l) %>% 
  rename(tm=abbreviation) %>% select(season,lg,tm,g_total)
avg_tot=team_wins_losses %>% filter(!is.na(g_total)) %>% group_by(season,lg) %>% 
  summarize(g_total=mean(g_total)) %>% ungroup() %>%
  mutate(tm="TOT")
full_team_wins_losses=bind_rows(team_wins_losses,avg_tot)
play_by_play=read_csv("Data/Player Play by Play.csv")

bbref_bow=bow("https://www.basketball-reference.com/",
              user_agent = "Sumitro Datta",force=TRUE,delay = 10)

#The Real Sixth Man of the Year*

#sixth man winners if sixth to ninth on team in minutes played, must have played more than 50% of games and started less than 50% of games
smoy_candidates=advanced %>% left_join(.,per_game) %>%
  left_join(.,team_wins_losses) %>% mutate(g_percent=g/g_total,gs_percent=gs/g) %>% 
  filter(g_percent>=0.5) %>% 
  #games started full coverage started in 1982
  filter(season>=1982) %>%
  filter(!is.na(mp_per_game)) %>% arrange(desc(mp_per_game)) %>% 
  group_by(tm,season) %>% slice(6:9) %>% ungroup() %>%
  filter(gs_percent<=0.5) %>% mutate(pts=round(pts_per_game*g))

smoy_candidates %>% slice_max(vorp,n=5) %>% select(player,season,pos:experience,tm,g_total,gs,mp_per_game,vorp)

smoy_candidates %>% slice_max(pts_per_game,n=5) %>% select(player,season,pos:experience,tm,g_total,gs,mp_per_game,pts_per_game)

career_sixman_totals=smoy_candidates %>% group_by(player_id,player) %>% 
  summarize(sixman_eligible_seas=n(),career_vorp=sum(vorp),career_g=sum(g),career_gs=sum(gs),career_mp=sum(mp),
            career_mp_per_game=career_mp/career_g,career_pts=sum(pts),career_pts_per_game=career_pts/career_g) %>% 
  ungroup()

career_sixman_totals %>%
  slice_max(career_vorp,n=5)

career_sixman_totals %>%
  slice_max(career_pts,n=5)

# The Deadshot Award (presented by Ray Allen/Reggie Miller)

#best qualifying 3 point percentage (Basketball-Reference)

per_game %>% filter(x3p_per_game*g>82,x3p_per_game>=1) %>%
  slice_max(x3p_percent,n=5) %>% select(player,season,x3p_per_game,x3p_percent)

career_totals %>%
  #use lower threshold of ABA (125 vs 250)
  filter(x3p>=125) %>%
  slice_max(x3p_percent,n=5) %>% select(player,x3p,x3p_percent)

# The Stormtrooper Award

#worst qualifying 2 point percentage (Basketball-Reference)

per_game %>% filter(fg_per_game*g>=300,fg_per_game>=300/82) %>%
  slice_min(x2p_percent,n=5) %>% select(player,season,x2p_per_game,x2p_percent)

career_totals %>%
  filter(fg>=2000) %>%
  slice_min(x2p_percent,n=5) %>% select(player,x2p,x2p_percent)

# The "If He Dies, He Dies" Award (presented by Tom Thibodeau, sponsored by Ivan Drago)

#most minutes played per game (Basketball-Reference) (credit to FurryCrew for the idea)

per_game %>% left_join(.,full_team_wins_losses) %>% mutate(g_percent=g/g_total) %>% filter(g_percent>=0.7) %>%
  slice_max(mp_per_game,n=5) %>% select(player,season,mp_per_game)

career_totals %>%
  mutate(mp_per_game=mp/g) %>%
  #use lower threshold of aba (7500 mp vs 400 g)
  filter(mp>7500) %>%
  slice_max(mp_per_game,n=5) %>% select(player,mp,mp_per_game)

#alternatively: most total minutes played (Basketball-Reference) (credit to FrankEMartindale for the idea)

per_game %>% left_join(.,full_team_wins_losses) %>% mutate(g_percent=g/g_total,total_mp=g*mp_per_game) %>% 
  filter(g_percent>=0.7) %>% slice_max(total_mp,n=5) %>% select(player,season,total_mp)

career_totals %>%
  slice_max(mp,n=5) %>% select(player,mp)

# The Weakest Link award (sponsored by Jack Link's Beef Jerky, presented by the 2015 Atlanta Hawks Starting 5)*

#best 5th starter (must have started 50% of a team's games) (credit to memeticengineering for the idea)
best_worst_starter_candidates=read_csv("Data/Advanced.csv") %>% left_join(.,read_csv("Data/Player Per Game.csv")) %>%
  left_join(.,team_wins_losses) %>% mutate(start_percent=gs/g_total) %>% 
  filter(start_percent>=0.5) %>% mutate(mp_per_game=mp/g,.after="mp") %>% 
  arrange(desc(mp_per_game)) %>% group_by(tm,season) %>% slice(1:5) %>% slice_min(vorp) %>% ungroup() 

best_worst_starter_candidates %>% slice_max(vorp,n=5) %>% select(player,season,pos:experience,tm,g_total,gs,mp_per_game,vorp)

career_best_worst_starter_totals=best_worst_starter_candidates %>% group_by(player_id,player) %>% 
  summarize(best_worst_starter_elig=n(),career_vorp=sum(vorp),career_g=sum(g),career_gs=sum(gs),career_mp=sum(mp),
            career_mp_per_game=career_mp/career_g) %>% ungroup()

career_best_worst_starter_totals %>%
  slice_max(career_vorp,n=5)

# The "This Game Has Always Been, And Will Always Be, About Buckets" Award (https://www.youtube.com/watch?v=-xYejfYxT4s)

# highest points as percentage of counting stats (rebounds, assists, steals, blocks)

pts_as_percent_counting_stats=per_game %>% left_join(.,team_wins_losses) %>% 
  mutate(g_percent=g/g_total) %>% filter(g_percent>=0.7) %>% 
  mutate(pts_as_percent_of_other_stats=pts_per_game/(pts_per_game+trb_per_game+ast_per_game+stl_per_game+blk_per_game))

pts_as_percent_counting_stats %>% 
  slice_max(pts_as_percent_of_other_stats,n=5) %>% 
  select(player,season,pts_per_game,trb_per_game,ast_per_game,stl_per_game,blk_per_game,pts_as_percent_of_other_stats)

career_totals %>% 
  #first season where steals & blocks recorded
  filter(first_seas>=1974) %>% 
  mutate(pts_as_percent_of_other_stats=pts/(pts+trb+ast+stl+blk)) %>% 
  filter(mp>7500) %>%
  slice_max(pts_as_percent_of_other_stats,n=5) %>%
  select(player,pts,trb,ast,stl,blk,pts_as_percent_of_other_stats)


# The Empty Calorie Stats Award (sponsored by Pop-Tarts)*

# highest percentile rank within position in usage, descending VORP, descending TS% (credit to eewap for the idea)

empty_stats_df=advanced %>% 
  left_join(.,team_wins_losses) %>% mutate(g_percent=g/g_total) %>% filter(g_percent>=0.5) %>%
  mutate(pos=word(pos,sep="-")) %>% group_by(pos,season) %>%
    mutate(ts_rank=percent_rank(desc(ts_percent)),
           usg_rank=percent_rank(usg_percent),
           vorp_rank=percent_rank(desc(vorp)),
           empty_stats=ts_rank+usg_rank+vorp_rank) %>% ungroup()

empty_stats_df %>% 
  slice_max(empty_stats,n=5) %>% select(player,season,pos:experience,ts_percent,usg_percent,vorp,ts_rank:empty_stats)

write_csv(empty_stats_df %>% 
            select(player,season,pos:experience,ts_percent,usg_percent,vorp,ts_rank:empty_stats),"Output Data/Empty Stats.csv")

# The "Can’t Win With These Cats" Award (sponsored by Scar from The Lion King, presented by Kevin Durant in a fake mustache)*

# highest difference in on/off splits between best & median player on team (credit to eewap for the idea)

pbp_filtered=play_by_play %>% filter(tm !="TOT") %>% 
  left_join(.,team_wins_losses) %>% mutate(g_percent=g/g_total) %>% filter(g_percent>=0.5)

on_off_leaders=pbp_filtered %>% group_by(tm,season) %>%
  slice_max(net_plus_minus_per_100_poss) %>% ungroup() %>% select(seas_id:player,tm:mp,net_plus_minus_per_100_poss,g_percent)

on_off_median=pbp_filtered %>% group_by(tm,season) %>% arrange(desc(net_plus_minus_per_100_poss)) %>%
  slice(-1) %>% summarize(median=median(net_plus_minus_per_100_poss)) %>% ungroup()

on_off_diff=left_join(on_off_leaders,on_off_median) %>%  mutate(npm_diff=net_plus_minus_per_100_poss-median)

on_off_diff %>% slice_max(npm_diff,n=5) %>% select(season,player:npm_diff)

write_csv(on_off_diff %>% select(player:npm_diff),"Output Data/On-Off Difference between Best & Median Player.csv")

# The "Anchors Aweigh" Award*

# biggest difference in on/off splits between worst & median player on team

on_off_trailers=pbp_filtered %>% group_by(tm,season) %>%
  slice_min(net_plus_minus_per_100_poss) %>% ungroup() %>% select(seas_id:player,tm:mp,net_plus_minus_per_100_poss,g_percent)

on_off_diff_trail=left_join(on_off_trailers,on_off_median) %>%  mutate(npm_diff=net_plus_minus_per_100_poss-median)

on_off_diff_trail %>% slice_min(npm_diff,n=5) %>% select(season,player:npm_diff)