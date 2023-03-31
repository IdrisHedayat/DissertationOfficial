# This file is where i try to work with prem data coding points won, form , 

# This file is where i begin to try manipulatiing premier league dataset

library(INLA)
library(tidyverse)


# This file is used if we are using the fixture list updated for double game weeks (i.e postponed games)
prem2223test <- read_excel("fbref2223dgw.xlsx")

# #file with unadjusted gw column.
# prem2223test <- read_excel("fbref2223.xlsx")

prem2223test[c("HG", "AG")] <- as.data.frame(do.call(rbind, strsplit(as.character(prem2223test$Score), "–", fixed = TRUE)))  
prem2223test <- prem2223test[, !(names(prem2223test) %in% c("Time", "Attendance", "Referee", "Match Report","Notes"))]
# prem2223test$played=

# Schedule for Premier Laegeue 22-23
premschedtest <- as_tibble(prem2223test) %>% 
  rename(
    Location = Venue
    
  ) %>% 
  mutate(
    Date=as.Date(Date, format = "%d/%m/%Y"),
    tournament="Premier League",
    ID_game=row_number(),
    Location=case_when(
      Location=="Selhurst Park" ~ "Crystal Palace",
      Location=="Craven Cottage" ~ "Fulham",
      Location=="Tottenham Hotspur Stadium" ~ "Tottenham",
      Location=="St James' Park" ~ "Newcastle Utd",
      Location=="Elland Road" ~ "Leeds United",
      Location=="Vitality Stadium" ~ "Bournemouth",
      Location=="Goodison Park" ~ "Everton",
      Location=="King Power Stadium" ~ "Leicester City",
      Location=="Old Trafford" ~ "Manchester Utd",
      Location=="London Stadium" ~ "West Ham",
      Location=="Villa Park" ~ "Aston Villa",
      Location=="Etihad Stadium" ~ "Manchester City",
      Location=="St. Mary's Stadium" ~ "Southampton",
      Location=="Molineux Stadium" ~ "Wolves",
      Location=="Emirates Stadium" ~ "Arsenal",
      Location=="The American Express Community Stadium" ~ "Brighton",
      Location=="Brentford Community Stadium" ~ "Brentford",
      Location=="The City Ground" ~ "Nott'ham Forest",
      Location=="Stamford Bridge" ~ "Chelsea",
      Location=="Anfield" ~ "Liverpool"
    )
  ) %>% rename(
    home_team = Home,
    away_team = Away,
    date=Date,
    Round.Number = Wk,
    home_score = HG,
    away_score = AG,
  )

# this is what was added in the euro file
#   tournament="UEFA Euro",
#   home_score=NA,
#   away_score=NA,
#   neutral=FALSE
# ) %>% rename(
#   country=Location,
#   home_team=Home.Team,
#   away_team=Away.Team,
#   ID_game=Match.Number,
#   date=Date
# )


# Long format
premschedtest=(
  premschedtest %>% select(ID_game,date,home_team,home_score,tournament,Location,Round.Number) %>% 
    rename(Team=home_team,Goal=home_score) 
) %>% 
  bind_rows(
    premschedtest %>% select(ID_game,date,away_team,away_score,tournament,Location,Round.Number) %>% 
      rename(Team=away_team,Goal=away_score)
  ) %>% arrange(ID_game) %>% mutate(Home=case_when(
    (Team==Location)~1,
    TRUE~0
  ))





premfootietest=premschedtest

# creates opponent variable 


premfootietest=premfootietest %>% 
  group_by(ID_game) %>% mutate(
    Opponent=c(Team[2],Team[1])
  ) %>% ungroup() %>% 
  select(ID_game,date,Team,Goal,Opponent,tournament,Location,Home,everything()) 


### Trying to create points won 

premfootietest=premfootietest %>% group_by(ID_game) %>% 
  mutate(
    points_won=case_when(
      (Goal[1]>Goal[2])~c(3,0),
      (Goal[1]==Goal[2])~c(1,1),
      (Goal[1]<Goal[2])~c(0,3),
      # If the game has not been played yet, nobody wins any points...]
      (is.na(Goal[1]) & is.na(Goal[2]))~c(NA_real_,NA_real_)
    )
  ) %>% ungroup()


# Creating a ' total_points ' to track the points tally, for form etc. as well as the days_since_last variable

premfootietest=premfootietest %>% 
  group_by(Team) %>% 
  mutate(total_points=cumsum(points_won),
         days_since_last=as.numeric(date-lag(date))
         ) %>%ungroup()



# Compute differences in ranks & points, and relative strength
premfootietest=premfootietest %>% group_by(ID_game) %>% 
  mutate(
    diff_point=c(total_points[1]-total_points[2],total_points[2]-total_points[1]),
    # diff_rank=c(rank[2]-rank[1],rank[1]-rank[2]),
    rel_strength=c(total_points[1]/(total_points[1]+total_points[2]),total_points[2]/(total_points[1]+total_points[2]))
  ) %>% ungroup()



# Creates the "form" variable --- based on the proportion of points won in the last 3 games (weighted by the relative strength)
premfootietest=premfootietest %>% 
  group_by(Team) %>% 
  mutate(form=lag(zoo::rollsumr(points_won/rel_strength,3,fill=NA))) %>% ungroup()


#perhaps create a total goal scored column and conceded column, and maybe a goal difference column? I could even make a column that would use Goal difference- difference between each team ?
# could experiment more with these, i.e. total GS diff, or making variables that similar to form track goalscoring form over past few games.
premfootietest <- premfootietest %>%
  group_by(ID_game) %>%
  mutate(GC = c(Goal[2],Goal[1]),
         GD = c(as.numeric(Goal[1])-as.numeric(Goal[2]),as.numeric(Goal[2])-as.numeric(Goal[1])),
         GDdiff = c(GD[1]-GD[2],GD[2]-GD[1])) %>%
  ungroup() %>% group_by(Team) %>% 
  mutate(total_GC = cumsum(GC),
         total_G = cumsum(Goal),
         total_GD = cumsum(GD)) %>% ungroup()


# Calculate rank for each given round
premfootietest = premfootietest %>%
  group_by(Round.Number) %>%
  mutate(rank = dense_rank(desc(total_points))) %>%
  ungroup() %>% group_by(ID_game) %>% 
  mutate(diff_rank=c(rank[1]-rank[2],rank[2]-rank[1]))











