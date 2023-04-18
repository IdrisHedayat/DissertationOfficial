library(INLA)
library(tidyverse)
library(readxl)

#unlike premier league dataset, no need to adjust for double gameweeks/rearranged fixtures
laliga2223 = read_excel("laliga2223raw.xlsx")


laliga2223[c("HG", "AG")] <- as.data.frame(do.call(rbind, strsplit(as.character(laliga2223$Score), "–", fixed = TRUE)))  
laliga2223 <- laliga2223[, !(names(laliga2223) %in% c("Time", "Attendance", "Referee", "Match Report","Notes"))]


# Schedule for Premier Laegeue 22-23
laligasched <- as_tibble(laliga2223) %>% 
  rename(
    Location = Venue
  ) %>% 
  mutate(
    Date=as.Date(Date, format = "%d/%m/%Y"),
    tournament="La Liga",
    HG = as.numeric(HG),
    AG = as.numeric(AG),
    ID_game=row_number(),
    Location=case_when(
      Location=="Estadio El Sadar" ~ "Osasuna",
      Location=="Estadio de Balaídos" ~ "Celta Vigo",
      Location=="Estadio Municipal José Zorrilla" ~ "Valladolid",
      Location=="Camp Nou" ~ "Barcelona",
      Location=="Estadio Nuevo Mirandilla" ~ "Cádiz",
      Location=="Estadio de Mestalla" ~ "Valencia",
      Location=="Power Horse Stadium" ~ "Almería",
      Location=="San Mamés" ~ "Athletic Club",
      Location=="Coliseum Alfonso Pérez" ~ "Getafe",
      Location=="Estadio Benito Villamarín" ~ "Betis",
      Location=="RCDE Stadium" ~ "Espanyol",
      Location=="Estadio Ramón Sánchez Pizjuán" ~ "Sevilla",
      Location=="Iberostar Estadi" ~ "Mallorca",
      Location=="Reale Arena" ~ "Real Sociedad",
      Location=="Estadio Santiago Bernabéu" ~ "Real Madrid",
      Location=="Estadio Manuel Martínez Valero" ~ "Elche",
      Location=="Estadi Municipal de Montilivi" ~ "Girona",
      Location=="Estadio Ciudad de Valencia" ~ "Villarreal",
      Location=="Estadio del Rayo Vallecano" ~ "Rayo Vallecano"
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
laligasched=(
  laligasched %>% select(ID_game,date,home_team,home_score,tournament,Location,Round.Number) %>% 
    rename(Team=home_team,Goal=home_score) 
) %>% 
  bind_rows(
    laligasched %>% select(ID_game,date,away_team,away_score,tournament,Location,Round.Number) %>% 
      rename(Team=away_team,Goal=away_score)
  ) %>% arrange(ID_game) %>% mutate(Home=case_when(
    (Team==Location)~1,
    TRUE~0
  ))





laligafootie=laligasched

# creates opponent variable 

# laligafootie = laligafootie %>% 
#   mutate(
#     Goal = Goal %>% as.numeric()
#   )


laligafootie=laligafootie %>% 
  group_by(ID_game) %>% mutate(
    Opponent=c(Team[2],Team[1])
  ) %>% ungroup() %>% 
  select(ID_game,date,Team,Goal,Opponent,tournament,Location,Home,everything()) 


### Trying to create points won , ' total_points ' to track the points tally, for form etc. as well as the days_since_last variable

laligafootie=laligafootie %>% group_by(ID_game) %>% 
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
laligafootie=laligafootie %>% group_by(ID_game) %>% 
  mutate(
    diff_point=c(total_points[1]-total_points[2],total_points[2]-total_points[1]),
    # diff_rank=c(rank[2]-rank[1],rank[1]-rank[2]),
    rel_strength=c(total_points[1]/(total_points[1]+total_points[2]),total_points[2]/(total_points[1]+total_points[2]))
  ) %>% ungroup()



# Creates the "form" variable --- based on the proportion of points won in the last 3 games (weighted by the relative strength)
laligafootie=laligafootie %>% 
  group_by(Team) %>% 
  mutate(game_number = row_number(),
         form= ifelse( game_number >= 5, lag(zoo::rollsumr(points_won/15,5,fill=NA)), NA_real_)) %>% 
  ungroup()


#perhaps create a total goal scored column and conceded column, and maybe a goal difference column? I could even make a column that would use Goal difference- difference between each team ?
# could experiment more with these, i.e. total GS diff, or making variables that similar to form track goalscoring form over past few games.
laligafootie <- laligafootie %>%
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

laligafootie = laligafootie %>%
  group_by(Round.Number) %>%
  mutate(rank = dense_rank(desc(total_points))) %>%
  ungroup() %>% group_by(ID_game) %>% 
  mutate(diff_rank=c(rank[1]-rank[2],rank[2]-rank[1])) 


saveRDS(laligafootie,"LaLigaFootie2223.rds")

