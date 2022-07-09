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

#sixth man winners if sixth to ninth on team in minutes played
#must have played more than 50% of games and started less than 50% of games 
#credit to KokiriEmerald for the reasoning behind re-implementing the starting criteria

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

write_csv(career_sixman_totals,"Output Data/Career 6th Man Totals.csv")

career_sixman_totals %>%
  slice_max(career_vorp,n=5)

career_sixman_totals %>%
  slice_max(career_pts,n=5)

# The Deadshot Award (presented by Ray Allen/Reggie Miller)

#best qualifying 3 point percentage (Basketball-Reference)

#scrape because of differing rate mins between seasons

session=nod(bow=bbref_bow,path="leaders/fg3_pct_season.html")

x3pt_percent_seas_lead=scrape(session) %>%
  html_nodes(css='#stats_TOT') %>%
  html_table() %>% .[[1]] %>% clean_names() %>% select(-rank) %>%
  mutate(season=as.numeric(str_sub(season,end=4))+1)

x3pt_percent_seas_lead %>% slice_max(x3p_percent,n=5)

career_totals %>%
  #use lower threshold of ABA (125 vs 250)
  filter(x3p>=125) %>%
  slice_max(x3p_percent,n=5) %>% select(player,x3p,x3p_percent)

# The Stormtrooper Award

#worst qualifying 2 point percentage (Basketball-Reference)

#scrape because of differing rate mins between seasons
# in addition, 2P trailers start from introduction of 3-point line (so differs from FG trailers)

session=nod(bow=bbref_bow,path="trailers/fg2_pct_season.html")

x2pt_percent_seas_trail=scrape(session) %>%
  html_nodes(css='#stats_TOT') %>%
  html_table() %>% .[[1]] %>% clean_names() %>% select(-rank) %>%
  mutate(season=as.numeric(str_sub(season,end=4))+1)

x2pt_percent_seas_trail %>% slice_min(x2p_percent,n=5)

session=nod(bow=bbref_bow,path="trailers/fg2_pct_career.html")

x2pt_percent_career_trail=scrape(session) %>%
  html_nodes(css='#tot') %>%
  html_table() %>% .[[1]] %>% clean_names() %>% select(-rank)

x2pt_percent_career_trail %>% slice_min(x2p_percent,n=5)

# The Black Hole Award

#most FGAs per assist (credit to Moose4KU for the idea)

field_goals_per_ast=totals %>%
  left_join(.,team_wins_losses) %>% mutate(g_percent=g/g_total) %>% 
  filter(g_percent>=0.5) %>% mutate(fga_per_ast=fga/ast) %>%
  select(player,season,g,mp,fga,ast,fga_per_ast)

field_goals_per_ast %>%
  slice_max(fga_per_ast,n=5) %>% arrange(desc(fga_per_ast))

field_goals_per_ast_career=career_totals %>% mutate(fga_per_ast=fga/ast) %>%
  #recording of minutes first started in 1952
  filter(first_seas>=1952,mp>=7500) %>%
  select(player,g,mp,fga,ast,fga_per_ast)

field_goals_per_ast_career %>%
  slice_max(fga_per_ast,n=5) %>% arrange(desc(fga_per_ast))

# The Hot Potato Award

#fewest FGAs per assist (credit to Moose4KU for the idea & ajayod for the name)

field_goals_per_ast %>%
  slice_min(fga_per_ast,n=5) %>% arrange(fga_per_ast)

field_goals_per_ast_career %>%
  slice_min(fga_per_ast,n=5) %>% arrange(fga_per_ast)

# The Most Expendable Player Award (sponsored by the National Basketball Referees Association)

#highest personal fouls per 36 minutes (credit to PsychoM & BrightGreenLED for the idea)

totals %>%
  left_join(.,team_wins_losses) %>% mutate(g_percent=g/g_total) %>% 
  filter(g_percent>=0.5,mp/g>=12) %>% 
  mutate(fouls_per_36_minutes=pf/mp*36) %>% select(player,season,mp,pf,fouls_per_36_minutes) %>%
  slice_max(fouls_per_36_minutes,n=5)

career_totals %>% 
  #recording of minutes first started in 1952
  filter(first_seas>=1952,mp>=7500) %>%
  mutate(fouls_per_36_minutes=pf/mp*36) %>% select(player,mp,pf,fouls_per_36_minutes) %>%
  slice_max(fouls_per_36_minutes,n=5)

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

write_csv(career_best_worst_starter_totals,"Output Data/Career Best Worst Starter Totals.csv")

# The "This Game Has Always Been, And Will Always Be, About Buckets" Award (https://www.youtube.com/watch?v=-xYejfYxT4s)

# highest points as percentage of counting stats (rebounds, assists, steals, blocks)

pts_as_percent_counting_stats=per_game %>% left_join(.,team_wins_losses) %>% 
  mutate(g_percent=g/g_total) %>% filter(g_percent>=0.7) %>% 
  mutate(pts_as_percent_of_other_stats=pts_per_game/(pts_per_game+trb_per_game+ast_per_game+stl_per_game+blk_per_game)) %>%
  filter(!is.na(pts_as_percent_of_other_stats)) %>% 
  select(player,season,pts_per_game,trb_per_game,ast_per_game,stl_per_game,blk_per_game,pts_as_percent_of_other_stats)

pts_as_percent_counting_stats %>% 
  slice_max(pts_as_percent_of_other_stats,n=5)

write_csv(pts_as_percent_counting_stats ,"Output Data/Seasonal Pts as Percent of Counting Stats.csv")

pts_as_percent_counting_stats_career=career_totals %>% 
  #first season where steals & blocks recorded
  filter(first_seas>=1974) %>% 
  mutate(pts_as_percent_of_other_stats=pts/(pts+trb+ast+stl+blk)) %>% 
  filter(mp>7500) %>% select(player,pts,trb,ast,stl,blk,pts_as_percent_of_other_stats)

pts_as_percent_counting_stats_career %>% 
  slice_max(pts_as_percent_of_other_stats,n=5)

write_csv(pts_as_percent_counting_stats_career,"Output Data/Career Pts as Percent of Counting Stats.csv")


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

write_csv(empty_stats_df %>% filter(!is.na(empty_stats)) %>%
            select(player,season,pos:experience,ts_percent,usg_percent,vorp,ts_rank:empty_stats),"Output Data/Empty Stats.csv")

# The "Can’t Win With These Cats" Award (sponsored by Scar from The Lion King, presented by Kevin Durant in a fake mustache)*

# highest difference in on/off splits between best & median player on team (credit to eewap for the idea)

pbp_filtered=play_by_play %>% filter(tm !="TOT") %>% 
  left_join(.,team_wins_losses) %>% mutate(g_percent=g/g_total) %>% filter(g_percent>=0.5,mp/g>=10)

on_off_leaders=pbp_filtered %>% group_by(tm,season) %>%
  slice_max(net_plus_minus_per_100_poss) %>% ungroup() %>% select(seas_id:player,tm:mp,net_plus_minus_per_100_poss,g_percent)

on_off_median=pbp_filtered %>% group_by(tm,season) %>% arrange(desc(net_plus_minus_per_100_poss)) %>%
  slice(-1) %>% summarize(median=median(net_plus_minus_per_100_poss)) %>% ungroup()

on_off_diff=left_join(on_off_leaders,on_off_median) %>%  mutate(npm_diff=net_plus_minus_per_100_poss-median)

on_off_diff %>% slice_max(npm_diff,n=5) %>% select(season,player:npm_diff)

# The "Anchors Aweigh" Award (presented by Ron Burgundy)*

# biggest difference in on/off splits between worst & median player on team

on_off_trailers=pbp_filtered %>% group_by(tm,season) %>%
  slice_min(net_plus_minus_per_100_poss) %>% ungroup() %>% select(seas_id:player,tm:mp,net_plus_minus_per_100_poss,g_percent)

on_off_diff_trail=left_join(on_off_trailers,on_off_median) %>%  mutate(npm_diff=net_plus_minus_per_100_poss-median)

on_off_diff_trail %>% slice_min(npm_diff,n=5) %>% select(season,player:npm_diff)

write_csv(bind_rows(on_off_diff,on_off_diff_trail) %>% 
            select(season,player:npm_diff),"Output Data/On-Off Difference between Extreme & Median Player.csv")

# The Stonks Award

#contract overperformance by highest VORP per % of salary cap (credit to memeticengineering for the idea)

#salaries up to 2020 recorded in previous project

team_abbrevs=advanced %>% filter(season %in% 2021:2022,tm != "TOT") %>% distinct(season,tm) %>% arrange(season,tm)

new_contracts=tibble()
for (i in seq(nrow(team_abbrevs))){
  session=nod(bow=bbref_bow,path=paste0("teams/",
                  team_abbrevs$tm[i],"/",
                  team_abbrevs$season[i],".html"))
  new_season=scrape(session) %>% html_nodes(xpath = '//comment()') %>%
    html_text() %>% paste(collapse='') %>% read_html() %>% 
    html_nodes(xpath=paste0('//*[(@id = "div_salaries2")]')) %>% 
    html_nodes("table") %>% .[[1]] %>% html_table()
  colnames(new_season)=c("Rk","player","salary")
  new_season=new_season %>% mutate(salary=parse_number(salary)) %>% select(-Rk) %>% 
    mutate(tm=team_abbrevs$tm[i],season=team_abbrevs$season[i])
  new_contracts=rbind(new_contracts,new_season)
  print(paste(team_abbrevs$tm[i],team_abbrevs$season[i]))
}

juniors=c("Troy Brown","Vernon Carey","Tim Hardaway","Larry Nance","Michael Porter",
          "Dennis Smith","Kelly Oubre","Kevin Porter","Kenyon Martin","Jaren Jackson",
          "Kira Lewis","Wendell Carter","Derrick Jones","Gary Trent")

new_contracts=new_contracts %>% 
  mutate(player=case_when(player %in% juniors~paste(player,"Jr."),
                          str_detect(player,"Gary Payton")~"Gary Payton II",
                          str_detect(player,"Xavier Tillman")~"Xavier Tillman Sr.",
                          str_detect(player,"Louzada")~"Didi Louzada",
                          str_detect(player,"Bamba")~"Mo Bamba",
                          str_detect(player,"Bagley")~"Marvin Bagley III",
                          str_detect(player,"Glenn Robinson")~"Glenn Robinson III",
                          str_detect(player,"Iwundu")~"Wesley Iwundu",
                          str_detect(player,"Clax")~"Nicolas Claxton",
                          str_detect(player,"James Ennis III")~"James Ennis",
                          str_detect(player,"Charlie Brown Jr.")~"Charlie Brown",
                          str_detect(player,"Otto Porter Jr.")~"Otto Porter",
                          str_detect(player,"Freedom")~"Enes Kanter",
                          str_detect(player,"Danuel House")~"Danuel House",
                          str_detect(player,"Woodard")~"Robert Woodard",
                          str_detect(player,"Walker IV")~"Lonnie Walker",
                          str_detect(player,"Mykhailiuk")~"Sviatoslav Mykhailiuk",
                          str_detect(player,"Juancho Hernangómez")~"Juan Hernangómez",
                          TRUE~player))

contracts=read_csv("Data/Player Salaries.csv") %>% bind_rows(.,new_contracts)

write_csv(contracts,"Data/Updated Contracts (as of 2022).csv")

cap_hist=read_csv("Data/Salary Cap History.csv")

contracts_and_vorp=left_join(contracts,read_csv("Data/Advanced.csv")) %>% 
  left_join(.,cap_hist) %>%
  select(player_id,player,tm,season,experience,salary,vorp,cap) %>% 
  arrange(player,desc(season)) %>% 
  #for players paid but didn't play in season
  group_by(player) %>% fill(player_id,.direction="up") %>% ungroup() %>% 
  replace_na(list(vorp=0)) %>% 
  #multiple contracts w/same team in same season
  group_by(player_id,player,tm,season) %>% mutate(salary=sum(salary),vorp=sum(vorp)) %>% slice(1) %>% ungroup() %>%
  mutate(percent_of_cap=salary/cap,vorp_per_percent_of_cap=vorp/percent_of_cap,
         wins_added=vorp*2.7, predicted_sal=wins_added/41*cap, 
         act_minus_predict=salary-predicted_sal,
         diff_as_percent_of_cap=act_minus_predict/cap)

contracts_and_vorp %>%
#remove any player with <=4 years of experience (rookie contract), salary less than 5% of cap
  filter(experience > 4,percent_of_cap>0.05) %>% arrange(desc(vorp_per_percent_of_cap)) %>%
  select(player:vorp_per_percent_of_cap) %>% select(-c(experience,cap))

careerwise_contracts_and_vorp=contracts_and_vorp %>% group_by(player_id,player) %>% 
  summarize(experience=max(experience,na.rm = TRUE),
            across(c(salary,vorp,percent_of_cap,wins_added,predicted_sal,act_minus_predict,diff_as_percent_of_cap),
                   .fns=sum,.names="career_{.col}"),
            career_vorp_per_percent_of_cap=career_vorp/career_percent_of_cap) %>% ungroup() 

careerwise_contracts_and_vorp %>%
  arrange(desc(career_vorp_per_percent_of_cap)) %>% filter(experience>4) %>%
  select(player:career_percent_of_cap,career_vorp_per_percent_of_cap)

# alternatively, using wins added & difference between predicted and actual salary (credit to ZandrickEllison for the idea)

contracts_and_vorp %>%
  #remove any player with <=4 years of experience (rookie contract), salary less than 5% of cap
  filter(experience > 4,percent_of_cap>0.05) %>% arrange(diff_as_percent_of_cap) %>%
  select(player:percent_of_cap,wins_added:diff_as_percent_of_cap) %>% select(-c(experience,cap,act_minus_predict))

careerwise_contracts_and_vorp %>%
  arrange(career_diff_as_percent_of_cap) %>% filter(experience>4) %>%
  select(player:career_vorp,career_wins_added:career_diff_as_percent_of_cap)

write_csv(contracts_and_vorp,"Output Data/Seasonal Contract Performance.csv")
write_csv(careerwise_contracts_and_vorp,"Output Data/Career Contract Performance.csv")
