#######  Packages  and setting up   #######
library(INLA)
library(tidyverse)
library(readxl)
library(gridExtra)


#### source footiemaker #####
# this is the dataset that is used to create each league's "footie dataset"

source("FootieMaker.R")

#function creates tibble dataframes of the specific league resulting in columns:
# ID_game: the unique identifier for the game being played
# Date: the date the game was played
# Team: A specifc football team involved in each game (there are 2 for each ID_game)
# Goal: The amount of goals the  team scored in the game
# Opponent: the team that was the away team in the game
# League: One of 5 top European Leagues , for instance La Liga
# Location: the Club who's Venue was  used (e.g if played at Manchester United's venue Old Trafford, Location is just "Manchester Utd")
# Home: Whether the team in the specific row played at home (=1) or away (=0)
# Round.Number = The fixture lists has been split into round for simulation ideally by total gameweeks, however some leagues eg premier league have double gameweeks where more than one game is played per gameweek, so more simulations rounds than total gameweeks 
# points_won: the amount of points obtained by the team (three for a victory, one for a draw, and zero for a defeat).
# total_points is the cumulative amount of points the team has amassed overall in the league.
# days_since_last: how many days have passed since the team's last match?
# diff_point: The number of points that separate the specific team and the opposition of the match. 
# rel_strength: Relative strength of the team in relation to the opposing team
# game_number: Amount of games the team has played in the league thus far. 
# Form: measure of Teams recent performances calculated as a fraction from points obtained and total points possible (15) from  their past 5 games 
# GC: The total number of league goals that the team has conceded.
# Goal difference (GD): the  team's overall league goal difference (goals scored minus goals conceded).
# GDdiff: The goal-difference difference between the team and their opposition
# total_GC: the total goals the team has allowed in the league thus far total_G: the total goals the  side has scored in the league thus far total_Goal difference (GD): the team's overall league goal difference (goals scored minus goals conceded)
# GCpg: the teams current average for goals conceded by the team per game.
# Gpg: the team's overall league average for goals scored per game.
# rank: where the  team currently stands in the league 
# diff_rank: The difference in where team stands in relation to the opponent 
# num:  a unique identifier for each case of game_ID and team collectively, i.e to distinguish between each row.



######### Source Utilities ##############

source("UtilityFunctions.R")

#above sources the following functions: 
# time_trend() , "for the RW2 model of over time performance" - doesnt work when i try to run anyways
# attack_defense() , plot of attack_defence effects
# team_strength() , shows attack or defence effets of each team depending on what is specified
# make_scored() ,   Post-processing Used to predict the number of goals scored in.a new game
# plot_joint() ,  # Plots the joint posterior distributions of all the possible scores
# outcome_predict() , gives win % predictions for each team specified 
# joint_marginal() ,  gives the plot of the joint posterior probabilities for specified teams
# updated_unplayed() ,  extracts the unplayed fixtures for a given round and inputs the most common result ( i think this should instead be inputting just the sample mean of the column of goals scored for the team rounded?)
# update_footie() , updates the premfootie dataset with the goal predictions
# var_updater() ,  updates the other variables in the dataset as a result of the goal predictions
# roundN , does the above 3 for a given round N , e.g round 24
# datprep() , preps the data as it should be before being using in the inla model






######  Premier League    #######


# This file is used if we are using the fixture list updated for double game weeks (i.e postponed games)
prem2223 = read_excel("prem2223raw.xlsx")

# #file with unadjusted gw column.
# prem2223 <- read_excel("fbref2223.xlsx")

prem2223[c("HG", "AG")] <- as.data.frame(do.call(rbind, strsplit(as.character(prem2223$Score), "–", fixed = TRUE)))  
prem2223 <- prem2223[, !(names(prem2223) %in% c("Time", "Attendance", "Referee", "Match Report","Notes"))]
# prem2223$played=

# Schedule for Premier Laegeue 22-23
premsched <- as_tibble(prem2223) %>% 
  rename(
    Location = Venue
  ) %>% 
  mutate(
    Date=as.Date(Date, format = "%d/%m/%Y"),
    tournament="Premier League",
    HG = as.numeric(HG),
    AG = as.numeric(AG),
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



plfootie = FootieMaker(premsched)


saveRDS(plfootie,"PremFootie2223.rds")



###### La Liga     #######



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
      Location=="Estadio del Rayo Vallecano" ~ "Rayo Vallecano",
      Location=="Estadio Cívitas Metropolitano" ~ "Atlético Madrid"
    )
  ) %>% rename(
    home_team = Home,
    away_team = Away,
    date=Date,
    Round.Number = Wk,
    home_score = HG,
    away_score = AG,
  )



laligafootie = FootieMaker(laligasched)


saveRDS(laligafootie,"LaLigaFootie2223.rds")



######   Ligue 1    ######




#unlike premier league dataset, no need to adjust for double gameweeks/rearranged fixtures
ligue2223 = read_excel("ligue12223raw.xlsx")


ligue2223[c("HG", "AG")] <- as.data.frame(do.call(rbind, strsplit(as.character(ligue2223$Score), "–", fixed = TRUE)))  
ligue2223 <- ligue2223[, !(names(ligue2223) %in% c("Time", "Attendance", "Referee", "Match Report","Notes"))]


# Schedule for Premier Laegeue 22-23
liguesched <- as_tibble(ligue2223) %>% 
  rename(
    Location = Venue
  ) %>% 
  mutate(
    Date=as.Date(Date, format = "%d/%m/%Y"),
    tournament="Ligue 1",
    HG = as.numeric(HG),
    AG = as.numeric(AG),
    ID_game=row_number(),
    Location=case_when(
      Location=="Matmut Stadium de Gerland" ~ "Lyon",
      Location=="Stade de la Meinau" ~ "Strasbourg",
      Location=="Stade Gabriel Montpied" ~ "Clermont Foot",
      Location=="Stadium Municipal" ~ "Toulouse",
      Location=="Stade Bollaert-Delelis" ~ "Lens",
      Location=="Stade Raymond Kopa" ~ "Angers",
      Location=="Decathlon Arena - Stade Pierre-Mauroy" ~ "Lille",
      Location=="Stade Raoul-Barrière" ~ "Montpellier",
      Location=="Roazhon Park" ~ "Rennes",
      Location=="Orange Vélodrome" ~ "Marseille",
      Location=="Stade de la Beaujoire - Louis Fonteneau" ~ "Nantes",
      Location=="Stade Louis II." ~ "Monaco",
      Location=="Parc des Princes" ~ "Paris S-G",
      Location=="Stade François Coty" ~ "Ajaccio",
      Location=="Stade Auguste-Delaune" ~ "Reims",
      Location=="Stade de l'Abbé Deschamps" ~ "Auxerre",
      Location=="Stade de l'Aube" ~ "Troyes",
      Location=="Stade de Nice" ~ "Nice",
      Location=="Stade Francis-Le Blé" ~ "Brest",
      Location=="Stade Yves Allainmat - Le Moustoir" ~ "Lorient"
    )
  ) %>% rename(
    home_team = Home,
    away_team = Away,
    date=Date,
    Round.Number = Wk,
    home_score = HG,
    away_score = AG,
  )



liguefootie = FootieMaker(liguesched)


saveRDS(liguefootie,"LigueFootie2223.rds")


######    Serie A      #######



#unlike premier league dataset, no need to adjust for double gameweeks/rearranged fixtures
serie2223 = read_excel("seriea2223raw.xlsx")


serie2223[c("HG", "AG")] <- as.data.frame(do.call(rbind, strsplit(as.character(serie2223$Score), "–", fixed = TRUE)))  
serie2223 <- serie2223[, !(names(serie2223) %in% c("Time", "Attendance", "Referee", "Match Report","Notes"))]


# Schedule for Serie A 22-23
# please note that there are teams that share stadiums, so this has to be sorted out:

seriesched <- as_tibble(serie2223) %>% 
  rename(
    Location = Venue
  ) %>% 
  mutate(
    Date=as.Date(Date, format = "%d/%m/%Y"),
    tournament="Serie A",
    HG = as.numeric(HG),
    AG = as.numeric(AG),
    ID_game=row_number(),
    Location=case_when(
      Location=="Stadio Comunale Luigi Ferraris" ~ "Sampdoria",
      Location=="Stadio Giuseppe Meazza" & Home == "Inter" ~ "Inter",
      Location=="Stadio Giuseppe Meazza" & Home == "Milan" ~ "Milan",
      Location=="U-Power Stadium" ~ "Monza",
      Location=="Stadio Comunale Via Del Mare" ~ "Lecce",
      Location=="Stadio Artemio Franchi" ~ "Fiorentina",
      Location=="Stadio Olimpico" & Home == "Lazio" ~ "Lazio",
      Location=="Stadio Olimpico" & Home == "Roma" ~ "Roma",
      Location=="Stadio Alberto Picco" ~ "Spezia",
      Location=="Stadio Arechi" ~ "Salernitana",
      Location=="Stadio Marc'Antonio Bentegodi" ~ "Hellas Verona",
      Location=="Allianz Stadium" ~ "Juventus",
      Location=="Stadio Olimpico di Torino" ~ "Torino",
      Location=="Dacia Arena" ~ "Udinese",
      Location=="Mapei Stadium - Città del Tricolore" ~ "Sassuolo",
      Location=="Stadio Diego Armando Maradona" ~ "Napoli",
      Location=="Stadio Renato Dall'Ara" ~ "Bologna",
      Location=="Gewiss Stadium" ~ "Atalanta",
      Location=="Stadio Giovanni Zini" ~ "Cremonese",
      Location=="Stadio Carlo Castellani" ~ "Empoli"
      )
  ) %>% rename(
    home_team = Home,
    away_team = Away,
    date=Date,
    Round.Number = Wk,
    home_score = HG,
    away_score = AG,
  )




seriefootie = FootieMaker(seriesched)

saveRDS(seriefootie,"SerieFootie2223.rds")



#######  Bundesliga    #######


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


bundesfootie = FootieMaker(bulisched)


saveRDS(bundesfootie,"BundesFootie2223.rds")





######### Formula ##############

eq = Goal ~ Home + 
  # diff_point +
  # diff_rank +
  # form +
  # rel_strength +
  # days_since_last +
  # Gpg +
  # GCpg +
  # GDdiff +
  f(factor(Team), model = "iid") +     # f() is used to define General Gasuain Model in INLA formula
  f(factor(Opponent), model = "iid")  
# Time component wek by team to account for difference in time performance
# f(id_date,model="rw2",replicate=as.numeric(factor(Team))) #+
# Overdispersion to account for extra goals
# f(num,model="iid") 



######### Modelling Premier League ##############

r=25

pldata=
  # Here "fixes" the data
  plfootie %>% 
  arrange(date) %>% mutate(
    form=case_when((is.nan(form)|is.infinite(form))~0,TRUE~form),
    # And scales all the continuous covariates for ease of fitting
    # diff_point=scale(diff_point,scale=TRUE),
    # diff_rank=scale(diff_rank,scale=TRUE),
    # days_since_last=scale(days_since_last,scale=TRUE),
    id_date=date %>% as.factor() %>% as.numeric()
  ) %>% 
  # Then filters only the games in a given round (for prediction)
  filter(Round.Number%in%c(NA,1:r)) # Here I have changed the code to 1:r to ensure it keeps the updated data too




mprem=inla(formula = eq,
             data=pldata,
             family="poisson",
             control.predictor=list(compute=TRUE,link=1),
             control.compute=list(config=TRUE,dic=TRUE))
summary(mprem)
t5team_strength(mprem,plfootie,"attack")  

t5team_strength(mprem,plfootie,"defense")  




######### Modelling La Liga ##############

# la liga only up to round.number 23
r=23

laligadata=
  # Here "fixes" the data
  laligafootie %>% 
  arrange(date) %>% mutate(
    form=case_when((is.nan(form)|is.infinite(form))~0,TRUE~form),
    # And scales all the continuous covariates for ease of fitting
    # diff_point=scale(diff_point,scale=TRUE),
    # diff_rank=scale(diff_rank,scale=TRUE),
    # days_since_last=scale(days_since_last,scale=TRUE),
    id_date=date %>% as.factor() %>% as.numeric()
  ) %>% 
  # Then filters only the games in a given round (for prediction)
  filter(Round.Number%in%c(NA,1:r)) # Here I have changed the code to 1:r to ensure it keeps the updated data too




mlaliga=inla(formula = eq,
       data=laligadata,
       family="poisson",
       control.predictor=list(compute=TRUE,link=1),
       control.compute=list(config=TRUE,dic=TRUE))
summary(mlaliga)




t5team_strength(mlaliga,laligafootie,"attack")  

t5team_strength(mlaliga,laligafootie,"defense")  

# t5team_strength_table = function(m, pf, effect = "attack") {
#   options(dplyr.show_progress = FALSE)
#   ii = pf %>% mutate(Team2 = Team, Team = factor(Team)) %>% mutate(Team = as.numeric(Team)) %>%
#     filter(date >= "2021-06-11") %>% select(ID, date, Team2, Team, everything()) %>% with(unique(Team))
#   labs = (pf %>% with(levels(as.factor(Team))))[sort(ii)]
#   
#   if (effect == "attack") {
#     result = (m$summary.random$`factor(Team)` %>% as_tibble() %>% filter(ID %in% labs) %>%
#                 mutate(Mean = mean, `2.5%` = `0.025quant`, `97.5%` = `0.975quant`)) %>%
#       select(Team = `ID`, attack, attack025, attack975)
#   }
#   if (effect == "defense") {
#     result = (m$summary.random$`factor(Opponent)` %>% as_tibble() %>% filter(ID %in% labs) %>%
#                 mutate(defense = mean, defense025 = `0.025quant`, defense975 = `0.975quant`)) %>%
#       select(Team = `ID`, defense, defense025, defense975)
#   }
#   return(result)
# }

t5team_strength_table = function(m, pf) {
  options(dplyr.show_progress = FALSE)
  ii = pf %>% mutate(Team2 = Team, Team = factor(Team)) %>% mutate(Team = as.numeric(Team)) %>%
    filter(date >= "2021-06-11") %>% select(ID, date, Team2, Team, everything()) %>% with(unique(Team))
  labs = (pf %>% with(levels(as.factor(Team))))[sort(ii)]
  
  attack_summary = (m$summary.random$`factor(Team)` %>% as_tibble() %>% filter(ID %in% labs) %>%
                      mutate(attaean = mean, attack025 = `0.025quant`, attack_median = `0.5quant`, attack975 = `0.975quant`)) %>%
    select(Team = `ID`, attack_mean, attack025, attack_median, attack975)
  
  defense_summary = (m$summary.random$`factor(Opponent)` %>% as_tibble() %>% filter(ID %in% labs) %>%
                       mutate(defense_mean = mean, defense025 = `0.025quant`, defense_median = `0.5quant`, defense975 = `0.975quant`)) %>%
    select(Team = `ID`, defense_mean, defense025, defense_median, defense975)
  
  result = left_join(attack_summary, defense_summary, by = "Team")
  return(result)
}

dissolaligaattdef = t5team_strength_table(mlaliga,laligafootie)
saveRDS(dissolaligaattdef, "dissolaligaattdef.RDS")
# t5_timetrend now works below, but for some reason teams like real madrid, barca, atletico are decreasing over time ? maybe i have to run with a more full dataset i.e past seasons, or maybe i need to run at the end of this season i.e after gw38 ?
time_trendt5time_trend(mlaliga,laligafootie)

# Post-processing Used to predict the number of goals scored in.a new game

# 
# 
# liga23 = t5make_scored(round=23,dt=laligadata,model=mlaliga,nsims=1000)
# 
# 
# 
# ligafootie23 = roundN(23,scored = liga23,roundata = laligadata ,prevfootie = laligafootie) 


####### la liga  Round 24 
####
# r=24
# lldata24 = datprep(ligafoote23,r)
# 
# 
# mliga24=runINLA(formu = eq,dat=lldata24)
# 
# t5team_strength(mliga24, lldata24)
# 
# liga24 = t5make_scored(round=24,dt=lldata24,model=mliga24,nsims=1000)
# 
# ligafootie24 = roundN(24,scored = liga24,roundata = lldata24 ,prevfootie = ligafootie23) 

# the above method can be condensed into a single function that returns all the above: below



################################################.
###### Ninla - trying making it quicker ########
################################################.


#### currently only running nsims=100 just to make it quicker for now

#after running roundNinla it returns a list of 3 tibbles: a tbl dataframe (in the function as footieN that is the new footie dataset),
# a tibble dataframe (rN in function) of the scored predictions from running INLA, 
# and the model itself (inlam in function) which can be extract if we want to get the team effects at a certain point



ligafootie23 = roundNinla(23,laligafootie) 



testscored1 = t5make_scored(23,laligadata,mlaliga,nsims=100)
#why isnt the above making values instead its a function??
testmpo = mpo_updated_unplayed(23,laligadata,testscored)

# ligaframe23 = ligafootie23$footieN      # the new footie
# ligascored23 = ligafootie23$rN       # the new scored predictions
# ligamodel23 = ligafootie23$inlam        #the new inla model used

# summary(mlaliga)
# summary(ligamodel23)

view(ligafootie23)

ligar24 = roundNinla(24,ligafootie23$footieN) 
ligar25 = roundNinla(25,ligar24$footieN) 
ligar26 = roundNinla(26,ligar25$footieN) 
ligar27 = roundNinla(27,ligar26$footieN) 
ligar28 = roundNinla(28,ligar27$footieN) 
ligar29 = roundNinla(29,ligar28$footieN) 
ligar30 = roundNinla(30,ligar29$footieN) 
ligar31 = roundNinla(31,ligar30$footieN) 
ligar32 = roundNinla(32,ligar31$footieN) 
ligar33 = roundNinla(33,ligar32$footieN) 
ligar34 = roundNinla(34,ligar33$footieN) 
ligar35 = roundNinla(35,ligar34$footieN) 
ligar36 = roundNinla(36,ligar35$footieN) 
ligar37 = roundNinla(37,ligar36$footieN) 
ligar38 = roundNinla(38,ligar37$footieN) 

t5team_strength(ligar38$inlam, ligar38$footieN)
ligar38$inlam

ligar38test = roundNinla(38,laligafootie)
laligafinaltable = t5tablerounrR(38,ligar38$footieN)
laligafinaltable


# la liga works but the model isnt optimal, 


######### Modelling Ligue 1 ##############

# ligue1 only up to round.number 25
r=25

liguedata=
  # Here "fixes" the data
  liguefootie %>% 
  arrange(date) %>% mutate(
    form=case_when((is.nan(form)|is.infinite(form))~0,TRUE~form),
    # And scales all the continuous covariates for ease of fitting
    # diff_point=scale(diff_point,scale=TRUE),
    # diff_rank=scale(diff_rank,scale=TRUE),
    # days_since_last=scale(days_since_last,scale=TRUE),
    id_date=date %>% as.factor() %>% as.numeric()
  ) %>% 
  # Then filters only the games in a given round (for prediction)
  filter(Round.Number%in%c(NA,1:r)) # Here I have changed the code to 1:r to ensure it keeps the updated data too



mligue=inla(formula = eq,
             data=liguedata,
             family="poisson",
             control.predictor=list(compute=TRUE,link=1),
             control.compute=list(config=TRUE,dic=TRUE))
summary(mligue)
t5team_strength(mligue,liguefootie)

liguefootie25 = roundNinla(25,liguefootie) 
liguer26 = roundNinla(26,liguefootie25$footieN) 
liguer27 = roundNinla(27,liguer26$footieN) 
liguer28 = roundNinla(28,liguer27$footieN) 
liguer29 = roundNinla(29,liguer28$footieN) 
liguer30 = roundNinla(30,liguer29$footieN) 
liguer31 = roundNinla(31,liguer30$footieN) 
liguer32 = roundNinla(32,liguer31$footieN) 
liguer33 = roundNinla(33,liguer32$footieN) 
liguer34 = roundNinla(34,liguer33$footieN) 
liguer35 = roundNinla(35,liguer34$footieN) 
liguer36 = roundNinla(36,liguer35$footieN) 
liguer37 = roundNinla(37,liguer36$footieN) 
liguer38 = roundNinla(38,liguer37$footieN) 


ligue1finaltable = t5tablerounrR(38,liguer38$footieN)
ligue1finaltable

######### Modelling Serie A ##############

# seriea only up to round.number 24
r=24

seriedata=
  # Here "fixes" the data
  seriefootie %>% 
  arrange(date) %>% mutate(
    form=case_when((is.nan(form)|is.infinite(form))~0,TRUE~form),
    # And scales all the continuous covariates for ease of fitting
    # diff_point=scale(diff_point,scale=TRUE),
    # diff_rank=scale(diff_rank,scale=TRUE),
    # days_since_last=scale(days_since_last,scale=TRUE),
    id_date=date %>% as.factor() %>% as.numeric()
  ) %>% 
  # Then filters only the games in a given round (for prediction)
  filter(Round.Number%in%c(NA,1:r)) # Here I have changed the code to 1:r to ensure it keeps the updated data too



mserie=inla(formula = eq,
            data=seriedata,
            family="poisson",
            control.predictor=list(compute=TRUE,link=1),
            control.compute=list(config=TRUE,dic=TRUE))
mseriesum = summary(mserie)
mseriesum

saveRDS(mseriesum, "mseriesum.rds")

sink("mseriesum.txt")
print(summary(mserie))
sink()
mseriesum$hyperpar

t5team_strength(mserie,seriefootie,"defense")

seriefootie24 = roundNinla(24,seriefootie) 
serier25 = roundNinla(25,seriefootie24$footieN) 
serier26 = roundNinla(26,serier25$footieN) 
serier27 = roundNinla(27,serier26$footieN) 
serier28 = roundNinla(28,serier27$footieN) 
serier29 = roundNinla(29,serier28$footieN) 
serier30 = roundNinla(30,serier29$footieN) 
serier31 = roundNinla(31,serier30$footieN) 
serier32 = roundNinla(32,serier31$footieN) 
serier33 = roundNinla(33,serier32$footieN) 
serier34 = roundNinla(34,serier33$footieN) 
serier35 = roundNinla(35,serier34$footieN) 
serier36 = roundNinla(36,serier35$footieN) 
serier37 = roundNinla(37,serier36$footieN) 
serier38 = roundNinla(38,serier37$footieN) 


serieAfinaltable = t5tablerounrR(38,serier38$footieN)
serieAfinaltable


######### Modelling Bundesliga ##############

# bundes only up to round.number 22
r=22


bundesdata = t5datprep(bundesfootie,r=22)

mbundes=inla(formula = eq,
            data= bundesdata,
            family = "poisson",
            control.predictor=list(compute=TRUE,link=1),
            control.compute=list(config=TRUE,dic=TRUE))

t5team_strength(mbundes,bundesfootie)


r22bundes = t5make_scored(22,dt= bundesdata, model=mbundes,nsims=1000)



bundesfootie22 = roundNinlampo(22,bundesfootie) 
bundesr23 = roundNinlampo(23,bundesfootie22$footieN) 
bundesr24 = roundNinlampo(24,bundesr23$footieN) 
bundesr25 = roundNinlampo(25,bundesr24$footieN) 
bundesr26 = roundNinlampo(26,bundesr25$footieN) 
bundesr27 = roundNinlampo(27,bundesr26$footieN) 
bundesr28 = roundNinlampo(28,bundesr27$footieN) 
bundesr29 = roundNinlampo(29,bundesr28$footieN) 
bundesr30 = roundNinlampo(30,bundesr29$footieN) 
bundesr31 = roundNinlampo(31,bundesr30$footieN) 
bundesr32 = roundNinlampo(32,bundesr31$footieN) 
bundesr33 = roundNinlampo(33,bundesr32$footieN) 
bundesr34 = roundNinlampo(34,bundesr33$footieN) 


Bundesligafinaltable = t5tablerounrR(34,bundesr34$footieN)
Bundesligafinaltable

saveRDS(Bundesligafinaltable, "bulibaselinetableMPO")


###### plots for dissertation #####


## attack and defense plots

llattplot = t5team_strength(mlaliga,laligafootie,"attack") + ylab("Teams")
lldefplot = t5team_strength(mlaliga,laligafootie,"defense") + ylab("Teams")
llattdef = grid.arrange(llattplot,lldefplot,ncol=2) 
ggsave("llattdef.png",llattdef,width = 8, height = 4, dpi = 300)


lljoinedattdef = t5attack_defense(mlaliga,laligafootie)
ggsave("joinedllattdef.png",lljoinedattdef,width = 8, height = 4, dpi = 300)

l1attplot = t5team_strength(mligue,liguefootie,"attack") + ylab("Teams")
l1defplot = t5team_strength(mligue,liguefootie,"defense") + ylab("Teams")
l1attdef = grid.arrange(l1attplot,l1defplot,ncol=2) 
ggsave("l1attdef.png",l1attdef,width = 8, height = 4, dpi = 300)



saattplot = t5team_strength(mserie,seriefootie,"attack") + ylab("Teams")
sadefplot = t5team_strength(mserie,seriefootie,"defense") + ylab("Teams")
saattdef = grid.arrange(saattplot,sadefplot,ncol=2) 
ggsave("saattdef.png",saattdef,width = 8, height = 4, dpi = 300)
 

buliattplot = t5team_strength(mbundes,bundesfootie,"attack") + ylab("Teams")
bulidefplot = t5team_strength(mbundes,bundesfootie,"defense") + ylab("Teams")
buliattdef = grid.arrange(buliattplot,bulidefplot,ncol=2) 
ggsave("buliattdef.png",buliattdef,width = 8, height = 4, dpi = 300)


plattplot = t5team_strength(mprem,plfootie,"attack") + ylab("Teams")
pldefplot = t5team_strength(mprem,plfootie,"defense") + ylab("Teams")
plattdef = grid.arrange(plattplot,pldefplot,ncol=2) 
ggsave("plattdef.png",plattdef,width = 8, height = 4, dpi = 300)



# 
# t5attdef = grid.arrange(llattplot,lldefplot,plattplot,pldefplot,l1attplot,l1defplot,saattplot,sadefplot,buliattplot,bulidefplot, ncol=2)
# ggsave("t5attdef.png",t5attdef, height = 40, width=40)


## example of simulation
#### 


testscored1 = t5make_scored(23,laligadata,mlaliga,nsims=1000)
mpotablexample = testscored1 %>% with(table(.[[5]],.[[6]])) %>% prop.table() %>%
  as_tibble(.name_repair = ~vctrs::vec_as_names(c("Cadiz","Rayo Vallecano","prop"),quiet=TRUE)) %>%
  mutate(across(where(is.character),as.numeric))


# mpo_updated_unplayed = function(N,roundata, scored) {
  
  gwNuTEST = laligadata %>% filter(is.na(Goal) & (Round.Number == 23))
  
  # for(i in 1:nrow(gwNu)) {
    # Extract the home and away teams for the i-th row
    gwNteam = as.character(gwNuTEST[g, "Team"])
    gwNopponent = as.character(gwNuTEST[g, "Opponent"])
    
    extractdataTEST = testscored1 %>% with(table(.[[gwNteam]],.[[gwNopponent]])) %>% prop.table() %>%
     as_tibble(.name_repair = ~vctrs::vec_as_names(c(gwNteam,gwNopponent,"n"),quiet=TRUE)) 
    #  mutate(across(where(is.character),as.numeric))
    
    max_row_index = which.max(extractdataTEST$n)
    max_row = extractdataTEST[max_row_index, ]
    
    # Extract the predicted goals for the home and away teams from the most likely result "max_row"
    team_goals = max_row[1]
    # away_goals = max_row[2] turns out this not needed since we are doing specific team rows in order
    as.
    gwNuTEST$Goal[1] = as.numeric(team_goals)
    view(gwNuTEST)
    
  # }
  
  return(gwNu %>% select(ID_game, Team, Goal, Opponent,num))
# }

    
    
### example of most probable outcome table
mpotablexample = mpotablexample %>% arrange(desc(prop))
mpotablexample 

saveRDS(mpotablexample, "Example MPO Table")

joint_marginal()
### example joint_marginal
cadizvallecanojm = joint_marginal("Cádiz","Rayo Vallecano", testscored1)
ggsave("CadizVallecanoJM.png",cadizvallecanojm, height = 6, width = 6)



#### comparing models, example bundesliga ####







  
fbuli1  =  formulacomp = Goal ~ Home + rel_strength + f(factor(Team), model = "iid") +    f(factor(Opponent), model = "iid")      

mbundes1=inla(formula = fbuli1,
             data= bundesdata,
             family = "poisson",
             control.predictor=list(compute=TRUE,link=1),
             control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))

mbundes1$waic$waic
mbundes1$dic$dic

fbuli2  =  formulacomp = Goal ~ Home + diff_rank + f(factor(Team), model = "iid") +    f(factor(Opponent), model = "iid")      
mbundes2=inla(formula = formulacomp,
              data= bundesdata,
              family = "poisson",
              control.predictor=list(compute=TRUE,link=1),
              control.compute=list(config=TRUE,dic=TRUE))
# etc.. (maybe too long)


# Compute all possible combinations of covariates
library(gtools)

covariates = list("rel_strength",
                  "diff_rank",
                  "form",
                  "days_since_last",
                  "Gpg",
                  "GCpg",
                  "GDdiff")

# Generate all possible combinations of covariates
all_combinations <- list()
for (i in 1:length(covariates)) {
  all_combinations[[i]] <- combn(covariates, i, simplify = FALSE)
}

all_combinations <- unlist(all_combinations, recursive = FALSE)

allcomb = all_combinations
### there are therfore 127 possible combinations:


# doing it in chunks of 10 instead of all 127 at once


# Number of combinations to run at once
chunk_size <- 10

# Number of chunks
num_chunks <- ceiling(length(all_combinations) / chunk_size)

# Fit the models and calculate WAIC
waic_values <- list()

for (chunk in 1:num_chunks) {
  start_index <- (chunk - 1) * chunk_size + 1
  end_index <- min(chunk * chunk_size, length(all_combinations))
  
  for (i in start_index:end_index) {
    covariate_formula <- paste(all_combinations[[i]], collapse = " + ")
    current_formula <- paste("Goal ~ Home +", covariate_formula, "+ f(factor(Team), model = 'iid') + f(factor(Opponent), model = 'iid')")
    current_model <- inla(
      formula = as.formula(current_formula),
      data = bundesdata,
      family = "poisson",
      control.predictor = list(compute = TRUE, link = 1),
      control.compute = list(config = TRUE, dic = TRUE, waic = TRUE)
    )
    waic_values[[paste(current_formula)]] <- current_model$waic$waic
  }
  
  # Save the results for the current chunk
  saveRDS(waic_values, paste0("waic_values_chunk_", chunk, ".rds"))
  
  # Clear the waic_values list for the next chunk
  waic_values <- list()
}

# Combine the results from all chunks
waic_values_combined <- list()
for (chunk in 1:num_chunks) {
  waic_values_combined <- c(waic_values_combined, readRDS(paste0("waic_values_chunk_", chunk, ".rds")))
}

sorted_waic_values <- waic_values_combined[order(unlist(waic_values_combined))]

# Select the top 10 models
top_10_models <- head(sorted_waic_values, 10)

# Create a data frame with the formulas and WAIC values
top_10_models_df <- data.frame(
  Formula = names(top_10_models),
  WAIC = unlist(top_10_models),
  stringsAsFactors = FALSE
)

top_10_models = as.tibble(top_10_models_df)

saveRDS(top_10_models,"Top10Models.rds")


top_10_models2 = top_10_models

new_names = c("Home+ diff_rank + Gpg + GCpg + GDdiff + Att + Def",
              "Home + Gpg + GCpg + GDdiff + Att + Def",
              "Home + diff_rank + days_since_last + Gpg + GCpg + GDdiff + Att + Def",
              "Home + diff_rank + form + Gpg + GCpg + GDdiff + Att + Def",
              "Home + form + Gpg + GCpg + GDdiff + Att + Def",
              "Home + rel_strength + diff_rank + Gpg + GCpg + GDdiff + Att + Def",
              "Home + rel_strength + Gpg + GCpg + GDdiff + Att + Def",
              "Home + rel_strength + form + Gpg + GCpg + GDdiff + Att + Def",
              "Home + days_since_last + Gpg + GCpg + GDdiff + Att + Def",
              "Home + diff_rank + form + days_since_last + Gpg + GCpg + GDdiff + Att + Def")

top_10_models2 = top_10_models2 %>% rename(Formulae = Formula)

top_10_models2$Formulae = new_names 
top_10_models2$WAIC = round(top_10_models2$WAIC, 3)

saveRDS(top_10_models2,"Top10Models2.rds")



OpFormula = Home + diff_rank + Gpg + GCpg + GDdiff + 
  f(factor(Team), model = "iid") +    
  f(factor(Opponent), model = "iid")  


OFtc = Goal ~ Home + diff_rank + Gpg + GCpg + GDdiff + 
  f(factor(Team), model = "iid") +    
  f(factor(Opponent), model = "iid")  +
# Time component wek by team to account for difference in time performance
 f(id_date,model="rw2",replicate=as.numeric(factor(Team))) #+
# Overdispersion to account for extra goals
# f(num,model="iid") 


OFo = Goal ~  Home + diff_rank + Gpg + GCpg + GDdiff + 
  f(factor(Team), model = "iid") +    
  f(factor(Opponent), model = "iid")  +
  # Time component wek by team to account for difference in time performance
  # f(id_date,model="rw2",replicate=as.numeric(factor(Team))) #+
# Overdispersion to account for extra goals 
  f(num,model="iid") 


OFtco = Goal ~  Home + diff_rank + Gpg + GCpg + GDdiff + 
  f(factor(Team), model = "iid") +    
  f(factor(Opponent), model = "iid") +
  # Time component wek by team to account for difference in time performance
  f(id_date,model="rw2",replicate=as.numeric(factor(Team))) +
  # Overdispersion to account for extra goals 
  f(num,model="iid") 


mtc = inla(formula = OFtc,
                data= bundesdata,
                family = "poisson",
                control.predictor=list(compute=TRUE,link=1),
                control.compute=list(config=TRUE,dic=TRUE,waic = TRUE))
mtc$waic$waic
# 988.8109

mo = inla(formula = OFo,
          data= bundesdata,
          family = "poisson",
          control.predictor=list(compute=TRUE,link=1),
          control.compute=list(config=TRUE,dic=TRUE,waic = TRUE))
mo$waic$waic

mtco = inla(formula = OFtco,
          data= bundesdata,
          family = "poisson",
          control.predictor=list(compute=TRUE,link=1),
          control.compute=list(config=TRUE,dic=TRUE,waic = TRUE))
mtco$waic$waic


####### Model validation ######

#### trying serie a with the optimal vs baseline model (optimal model based on bundesliga so maybe thats why it performs worse?)
# we consider the Serie A 21-22 season for model validation, using the optimal model and baseline models:
# we remove data only keeping the first half
seriea2122 = presched("seriea2122.xlsx")

VenueHome(seriea2122)

serie2122sched <- as_tibble(seriea2122) %>% 
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
      Location=="Stadio Atleti Azzurri d'Italia"~"Atalanta", 
      Location=="Stadio Renato Dall'Ara"~"Bologna", 
      Location=="Unipol Domus"~"Cagliari", 
      Location=="Stadio Carlo Castellani"~"Empoli",
      Location=="Stadio Artemio Franchi"~"Fiorentina", 
      Location=="Stadio Comunale Luigi Ferraris"~"Genoa", 
      Location=="Stadio Marc'Antonio Bentegodi"~"Hellas Verona", 
      Location=="Stadio Giuseppe Meazza" & Home == "Inter" ~"Inter", 
      Location=="Allianz Stadium"~"Juventus", 
      Location=="Stadio Olimpico" & Home == "Lazio"~"Lazio", 
      Location=="Stadio Giuseppe Meazza" & Home == "Milan" ~"Milan", 
      Location=="Stadio Diego Armando Maradona"~"Napoli", 
      Location=="Stadio Olimpico" & Home == "Roma" ~"Roma", 
      Location=="Stadio Arechi"~"Salernitana", 
      Location=="Stadio Comunale Luigi Ferraris"~"Sampdoria", 
      Location=="Mapei Stadium - Città del Tricolore"~"Sassuolo", 
      Location=="Stadio Alberto Picco"~"Spezia", 
      Location=="Stadio Olimpico di Torino"~"Torino", 
      Location=="Dacia Arena"~"Udinese", 
      Location=="Stadio Pierluigi Penzo"~"Venezia"
    )
  ) %>% rename(
    home_team = Home,
    away_team = Away,
    date=Date,
    Round.Number = Wk,
    home_score = HG,
    away_score = AG,
  )


serie2122footie = FootieMaker(serie2122sched)


saveRDS(serie2122footie,"serie2122footie.rds")


OpFormula = Goal ~  Home + diff_rank + Gpg + GCpg + GDdiff + 
  f(factor(Team), model = "iid") +    
  f(factor(Opponent), model = "iid")  


r=20

serie2122data=
  # Here "fixes" the data
  serie2122footie %>% 
  arrange(date) %>% mutate(
    form=case_when((is.nan(form)|is.infinite(form))~0,TRUE~form),
    # And scales all the continuous covariates for ease of fitting
    # diff_point=scale(diff_point,scale=TRUE),
    # diff_rank=scale(diff_rank,scale=TRUE),
    # days_since_last=scale(days_since_last,scale=TRUE),
    id_date=date %>% as.factor() %>% as.numeric()
  ) %>% 
  # Then filters only the games in a given round (for prediction)
  filter(Round.Number%in%c(NA,1:r)) # Here I have changed the code to 1:r to ensure it keeps the updated data too



mserie2122=inla(formula = OpFormula,
            data=serie2122data,
            family="poisson",
            control.predictor=list(compute=TRUE,link=1),
            control.compute=list(config=TRUE,dic=TRUE))
mserie2122sum = summary(mserie2122)
mserie2122sum

saveRDS(mserie2122sum, "mserie2122sum.rds")



t5team_strength(mserie2122,serie2122footie,"attack")

bserie2122footie20 = roundNinlampo(20,serie2122footie,frm = eq)
bserie2122r21 = roundNinlampo(21,bserie2122footie20$footieN,frm = eq)
bserie2122r22 = roundNinlampo(22,bserie2122r21$footieN,frm = eq)
bserie2122r23 = roundNinlampo(23,bserie2122r22$footieN,frm = eq)
bserie2122r24 = roundNinlampo(24,bserie2122r23$footieN,frm = eq)
bserie2122r25 = roundNinlampo(25,bserie2122r24$footieN,frm = eq) 
bserie2122r26 = roundNinlampo(26,bserie2122r25$footieN,frm = eq) 
bserie2122r27 = roundNinlampo(27,bserie2122r26$footieN,frm = eq) 
bserie2122r28 = roundNinlampo(28,bserie2122r27$footieN,frm = eq) 
bserie2122r29 = roundNinlampo(29,bserie2122r28$footieN,frm = eq) 
bserie2122r30 = roundNinlampo(30,bserie2122r29$footieN,frm = eq) 
bserie2122r31 = roundNinlampo(31,bserie2122r30$footieN,frm = eq) 
bserie2122r32 = roundNinlampo(32,bserie2122r31$footieN,frm = eq) 
bserie2122r33 = roundNinlampo(33,bserie2122r32$footieN,frm = eq) 
bserie2122r34 = roundNinlampo(34,bserie2122r33$footieN,frm = eq) 
bserie2122r35 = roundNinlampo(35,bserie2122r34$footieN,frm = eq) 
bserie2122r36 = roundNinlampo(36,bserie2122r35$footieN,frm = eq) 
bserie2122r37 = roundNinlampo(37,bserie2122r36$footieN,frm = eq) 
bserie2122r38 = roundNinlampo(38,bserie2122r37$footieN,frm = eq) 


serie2122Afinaltablebaseline = t5tablerounrR(38,bserie2122r38$footieN)
serie2122Afinaltablebaseline
saveRDS(serie2122Afinaltablebasine, "serie2122AfinaltableOPFORM.rds" )


### using baselien
serie2122footie20 = roundNinlampo(20,serie2122footie,frm=Opformula)
serie2122r21 = roundNinlampo(21,serie2122footie20$footieN,frm=Opformula)
serie2122r22 = roundNinlampo(22,serie2122r21$footieN,frm=Opformula)
serie2122r23 = roundNinlampo(23,serie2122r22$footieN,frm=Opformula)
serie2122r24 = roundNinlampo(24,serie2122r23$footieN,frm=Opformula)
serie2122r25 = roundNinlampo(25,serie2122r24$footieN,frm=Opformula) 
serie2122r26 = roundNinlampo(26,serie2122r25$footieN,frm=Opformula) 
serie2122r27 = roundNinlampo(27,serie2122r26$footieN,frm=Opformula) 
serie2122r28 = roundNinlampo(28,serie2122r27$footieN,frm=Opformula) 
serie2122r29 = roundNinlampo(29,serie2122r28$footieN,frm=Opformula) 
serie2122r30 = roundNinlampo(30,serie2122r29$footieN,frm=Opformula) 
serie2122r31 = roundNinlampo(31,serie2122r30$footieN,frm=Opformula) 
serie2122r32 = roundNinlampo(32,serie2122r31$footieN,frm=Opformula) 
serie2122r33 = roundNinlampo(33,serie2122r32$footieN,frm=Opformula) 
serie2122r34 = roundNinlampo(34,serie2122r33$footieN,frm=Opformula) 
serie2122r35 = roundNinlampo(35,serie2122r34$footieN,frm=Opformula) 
serie2122r36 = roundNinlampo(36,serie2122r35$footieN,frm=Opformula) 
serie2122r37 = roundNinlampo(37,serie2122r36$footieN,frm=Opformula) 
serie2122r38 = roundNinlampo(38,serie2122r37$footieN,frm=Opformula) 


serie2122AfinaltableOPFORM = t5tablerounrR(38,serie2122r38$footieN)
serie2122AfinaltableOPFORM
saveRDS(serie2122AfinaltableOPFORM, "serie2122AfinaltableOPFORM.rds" )



######  reading table observed output

sa2122tab = read_excel("sa2122tab.xlsx") %>%
  as.tibble() %>%
  mutate(total_points = Pts,
         Team = Squad) %>%
  select(Team, total_points) %>%
  arrange(Team)

sa2122OPFORM = serie2122AfinaltableOPFORM %>%
  select(Team, total_points) %>%
  arrange(Team)

sa2122baseline = serie2122Afinaltablebaseline %>%
  select(Team, total_points) %>%
  arrange(Team)


# Calculate points difference for baseline model
sa2122baseline$Points_Difference <- abs(sa2122baseline$total_points - sa2122tab$total_points)

# Calculate points difference for optimized model
sa2122OPFORM$Points_Difference <- abs(sa2122OPFORM$total_points - sa2122tab$total_points)

# Combine the results
comparison_table <- data.frame(
  Team = sa2122tab$Team,
  Observed_Points = sa2122tab$total_points,
  Baseline_Points = sa2122baseline$total_points,
  Baseline_Difference = sa2122baseline$Points_Difference,
  Optimized_Points = sa2122OPFORM$total_points,
  Optimized_Difference = sa2122OPFORM$Points_Difference
) 

# Print the comparison
print(comparison_table)
comptab=as.tibble(comparison_table)
saveRDS(comptab, "comptabsa2122")

# Calculate the sum of squared differences for the baseline model
sum_sq_diff_baseline <- sum(sa2122baseline$Points_Difference ^ 2)

# Calculate the sum of squared differences for the optimized model
sum_sq_diff_optimized <- sum(sa2122OPFORM$Points_Difference ^ 2)

# Print the results
cat("Mean squared error for the baseline model:", sum_sq_diff_baseline/20, "\n")
cat("Mean squared error for the optimized model:", sum_sq_diff_optimized/20, "\n")



##### buli model validation ####