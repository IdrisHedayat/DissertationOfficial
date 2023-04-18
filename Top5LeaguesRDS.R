#######  Packages    #######
library(INLA)
library(tidyverse)
library(readxl)



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


#### source footiemaker #####
source("FootieMaker.R")


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


######### Source Utilities ##############

source("UtilityFunctions.R")


######### Setting up Modelling ##############

eq = Goal ~ Home + 
  f(factor(Team), model = "iid") +     # f() is used to define General Gasuain Model in INLA formula
  f(factor(Opponent), model = "iid") # +
# Time component wek by team to account for difference in time performance
# f(id_date,model="rw2",replicate=as.numeric(factor(Team))) +
# Overdispersion to account for extra goals
# f(num,model="iid") +





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

# Post-processing Used to predict the number of goals scored in.a new game



liga23 = t5make_scored(round=23,dt=laligadata,model=mlaliga,nsims=1000)



ligafootie23 = roundN(23,scored = liga23,roundata = laligadata ,prevfootie = laligafootie) 


####### la liga  Round 24 #######
r=24
lldata24 = datprep(ligafootie23,r)


mliga24=runINLA(formu = eq,dat=lldata24)

t5team_strength(mliga24, lldata24)

liga24 = t5make_scored(round=24,dt=lldata24,model=mliga24,nsims=1000)

ligafootie24 = roundN(24,scored = liga24,roundata = lldata24 ,prevfootie = ligafootie23) 

####### la liga  Round 25 #######
r=25
lldata25 = datprep(ligafootie24,r)


mliga25=runINLA(formu = eq,dat=lldata25)

t5team_strength(mliga25, lldata25)

liga25 = t5make_scored(round=25,dt=lldata25,model=mliga25,nsims=1000)

ligafootie25 = roundN(25,scored = liga25,roundata = lldata25 ,prevfootie = ligafootie24) 

###### Ninla - trying making it quicker #######
roundNinla = function(n,prevfootie){
  df = datprep(prevfootie,n)
  
  
  inlam = runINLA(formu = eq , dat=df)

  set.seed(2223)
  
  rN = make_scored(round=n,dt=df, model = inlam,nsims=1000)

  footieN = roundN(N=n,scored=rN,roundata = df , prevfootie=prevfootie)
  
  return(footieN)
}

ligafootie26 = roundNinla(26,ligafootie25)
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
t5team_strength(mligue,liguefootie)




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
t5team_strength(mserie,seriefootie)




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

