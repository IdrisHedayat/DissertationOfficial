library(INLA)
library(tidyverse)
library(readxl)

#unlike premier league dataset, no need to adjust for double gameweeks/rearranged fixtures
bundes2223 = read_excel("bundes2223raw.xlsx")


bundes2223[c("HG", "AG")] <- as.data.frame(do.call(rbind, strsplit(as.character(bundes2223$Score), "–", fixed = TRUE)))  
bundes2223 <- bundes2223[, !(names(bundes2223) %in% c("Time", "Attendance", "Referee", "Match Report","Notes"))]


# Schedule for Premier Laegeue 22-23
bulisched <- as_tibble(bundes2223) %>% 
  rename(
    Location = Venue
  ) %>% 
  mutate(
    Date=as.Date(Date, format = "%d/%m/%Y"),
    tournament="Bundesliga",
    HG = as.numeric(HG),
    AG = as.numeric(AG),
    ID_game=row_number(),
    Location=case_when(
      Location == "Deutsche Bank Park" ~ "Eint Frankfurt",
      Location == "Volkswagen Arena" ~ "Wolfsburg",
      Location == "WWK Arena" ~ "Augsburg",
      Location == "Stadion An der Alten Försterei" ~ "Union Berlin",
      Location == "Stadion im Borussia-Park" ~ "M'Gladbach",
      Location == "Vonovia Ruhrstadion" ~ "Bochum",
      Location == "Signal Iduna Park" ~ "Dortmund",
      Location == "Mercedes-Benz Arena" ~ "Stuttgart",
      Location == "RheinEnergieSTADION" ~ "Köln",
      Location == "Europa-Park Stadion" ~ "Freiburg",
      Location == "PreZero Arena" ~ "Hoffenheim",
      Location == "Wohninvest-Weserstadion" ~ "Werder Bremen",
      Location == "BayArena" ~ "Leverkusen",
      Location == "Red Bull Arena" ~ "RB Leipzig",
      Location == "Olympiastadion Berlin" ~ "Hertha BSC",
      Location == "Veltins-Arena" ~ "Schalke 04",
      Location == "Mewa Arena" ~ "Mainz 05",
      Location == "Allianz Arena" ~ "Bayern Munich"
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
bulisched=(
  bulisched %>% select(ID_game,date,home_team,home_score,tournament,Location,Round.Number) %>% 
    rename(Team=home_team,Goal=home_score) 
) %>% 
  bind_rows(
    bulisched %>% select(ID_game,date,away_team,away_score,tournament,Location,Round.Number) %>% 
      rename(Team=away_team,Goal=away_score)
  ) %>% arrange(ID_game) %>% mutate(Home=case_when(
    (Team==Location)~1,
    TRUE~0
  ))





bundesfootie=bulisched

# creates opponent variable 

# bundesfootie = bundesfootie %>% 
#   mutate(
#     Goal = Goal %>% as.numeric()
#   )


bundesfootie=bundesfootie %>% 
  group_by(ID_game) %>% mutate(
    Opponent=c(Team[2],Team[1])
  ) %>% ungroup() %>% 
  select(ID_game,date,Team,Goal,Opponent,tournament,Location,Home,everything()) 


### Trying to create points won , ' total_points ' to track the points tally, for form etc. as well as the days_since_last variable

bundesfootie=bundesfootie %>% group_by(ID_game) %>% 
  mutate(
    points_won=case_when(
      (Goal[1]>Goal[2])~c(3,0),
      (Goal[1]==Goal[2])~c(1,1),
      (Goal[1]<Goal[2])~c(0,3),
      # If the game has not been played yet, nobody wins any points...]
      (is.na(Goal[1]) & is.na(Goal[2]))~c(NA_real_,NA_real_)
    )
  ) %>% ungroup()%>% 
  group_by(Team) %>% 
  mutate(total_points=cumsum(points_won),
         days_since_last=as.numeric(date-lag(date))
  ) %>%ungroup()



# Compute differences in ranks & points, and relative strength
bundesfootie=bundesfootie %>% group_by(ID_game) %>% 
  mutate(
    diff_point=c(total_points[1]-total_points[2],total_points[2]-total_points[1]),
    # diff_rank=c(rank[2]-rank[1],rank[1]-rank[2]),
    rel_strength=c(total_points[1]/(total_points[1]+total_points[2]),total_points[2]/(total_points[1]+total_points[2]))
  ) %>% ungroup()



# Creates the "form" variable --- based on the proportion of points won in the last 3 games (weighted by the relative strength)
bundesfootie=bundesfootie %>% 
  group_by(Team) %>% 
  mutate(game_number = row_number(),
         form= ifelse( game_number >= 5, lag(zoo::rollsumr(points_won/15,5,fill=NA)), NA_real_)) %>% 
  ungroup()


#perhaps create a total goal scored column and conceded column, and maybe a goal difference column? I could even make a column that would use Goal difference- difference between each team ?
# could experiment more with these, i.e. total GS diff, or making variables that similar to form track goalscoring form over past few games.
bundesfootie <- bundesfootie %>%
  group_by(ID_game) %>%
  mutate(GC = c(Goal[2],Goal[1]),
         GD = c(as.numeric(Goal[1])-as.numeric(Goal[2]),as.numeric(Goal[2])-as.numeric(Goal[1])),
         GDdiff = c(GD[1]-GD[2],GD[2]-GD[1])) %>%
  ungroup() %>% group_by(Team) %>% 
  mutate(total_GC = cumsum(GC),
         total_G = cumsum(Goal),
         total_GD = cumsum(GD),
         GCpg = total_GC/game_number,
         Gpg = total_G / game_number,) %>% ungroup()  %>% 
  group_by(ID_game) %>% mutate(ID=cur_group_id()) %>% ungroup() 

###### There is a problem with calculating rank here, it doesnt accurately monitor league position at the time, perhaps consider an ELO ranking instead ############
# Calculate rank for each given round.  

bundesfootie = bundesfootie %>%
  group_by(Round.Number) %>%
  mutate(rank = dense_rank(desc(total_points))) %>%
  ungroup() %>% group_by(ID_game) %>% 
  mutate(diff_rank=c(rank[1]-rank[2],rank[2]-rank[1])) 


saveRDS(bundesfootie,"BundesFootie2223.rds")
