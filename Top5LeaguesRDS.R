#######  Packages  and setting up   #######
library(INLA)
library(tidyverse)
library(readxl)


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
prem2223 = read_excel("fbref2223r44.xlsx")

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
summary(mserie)
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



bundesfootie22 = roundNinla(22,bundesfootie) 
bundesr23 = roundNinla(23,bundesfootie22$footieN) 
bundesr24 = roundNinla(24,bundesr23$footieN) 
bundesr25 = roundNinla(25,bundesr24$footieN) 
bundesr26 = roundNinla(26,bundesr25$footieN) 
bundesr27 = roundNinla(27,bundesr26$footieN) 
bundesr28 = roundNinla(28,bundesr27$footieN) 
bundesr29 = roundNinla(29,bundesr28$footieN) 
bundesr30 = roundNinla(30,bundesr29$footieN) 
bundesr31 = roundNinla(31,bundesr30$footieN) 
bundesr32 = roundNinla(32,bundesr31$footieN) 
bundesr33 = roundNinla(33,bundesr32$footieN) 
bundesr34 = roundNinla(34,bundesr33$footieN) 


Bundesligafinaltable = t5tablerounrR(34,bundesr34$footieN)
Bundesligafinaltable
