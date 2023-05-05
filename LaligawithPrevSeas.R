source("FootieMaker.R")

###### Schedule for La Liga 19-20,     season_id = 1 ###### 

laliga1920 = presched("laliga1920fbref.xlsx")

lvhtable(laliga1920)

VenueHome(laliga1920)

laliga1920sched <- as_tibble(laliga1920) %>% 
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
      Location=="Estadio de Mendizorroza"~"Alavés", 
      Location=="San Mamés"~"Athletic Club",
      Location=="Estadio Wanda Metropolitano"~"Atlético Madrid",
      Location=="Camp Nou"~"Barcelona",
      Location=="Estadio Benito Villamarín"~"Betis", 
      Location=="Estadio de Balaídos"~"Celta Vigo", 
      Location=="Estadio Municipal de Ipurúa"~"Eibar",
      Location=="RCDE Stadium"~"Espanyol", 
      Location=="Coliseum Alfonso Pérez"~"Getafe", 
      Location=="Estadio Nuevo Los Cármenes"~"Granada", 
      Location=="Estadio Municipal de Butarque"~"Leganés",
      Location=="Estadio Ciudad de Valencia"~"Levante", 
      Location=="Iberostar Estadi"~"Mallorca", 
      Location=="Estadio El Sadar"~"Osasuna",
      Location=="Estadio Santiago Bernabéu"~"Real Madrid",
      Location=="Estadio Municipal de Anoeta"~"Real Sociedad",
      Location=="Estadio Ramón Sánchez Pizjuán"~"Sevilla",
      Location=="Estadio de Mestalla"~"Valencia", 
      Location=="Estadio Municipal José Zorrilla"~"Valladolid",
      Location=="Estadio de la Cerámica"~"Villarreal"
    ),
    season_id = 1
  ) %>% rename(
    home_team = Home,
    away_team = Away,
    date=Date,
    Round.Number = Wk,
    home_score = HG,
    away_score = AG,
  )

laliga1920footie = MultiSznFootieMaker(laliga1920sched)

# Schedule for La Liga 20-21 ,     season_id = 2 ###### 
laliga2021 = presched("laliga2021fbref.xlsx")

VenueHome(laliga2021)

laliga2021sched <- as_tibble(laliga2021) %>% 
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
      Location=="Estadio de Mendizorroza"~"Alavés", 
      Location=="San Mamés"~"Athletic Club",
      Location=="Estadio Wanda Metropolitano"~"Atlético Madrid",
      Location=="Camp Nou"~"Barcelona", 
      Location=="Estadio Benito Villamarín"~"Betis", 
      Location=="Estadio Ramón de Carranza"~"Cádiz", 
      Location=="Estadio de Balaídos"~"Celta Vigo", 
      Location=="Estadio Municipal de Ipurúa"~"Eibar",
      Location=="Estadio Manuel Martínez Valero"~"Elche",
      Location=="Coliseum Alfonso Pérez"~"Getafe", 
      Location=="Estadio Nuevo Los Cármenes"~"Granada", 
      Location=="Estadio El Alcoraz"~"Huesca", 
      Location=="Estadio Ciudad de Valencia"~"Levante",
      Location=="Estadio El Sadar"~"Osasuna", 
      Location=="Estadio Alfredo Di Stéfano"~"Real Madrid",
      Location=="Estadio Municipal de Anoeta"~"Real Sociedad", 
      Location=="Estadio Ramón Sánchez Pizjuán"~"Sevilla", 
      Location=="Estadio de Mestalla"~"Valencia", 
      Location=="Estadio Municipal José Zorrilla"~"Valladolid", 
      Location=="Estadio de la Cerámica"~"Villarreal"
    ),
    season_id = 2
  ) %>% rename(
    home_team = Home,
    away_team = Away,
    date=Date,
    Round.Number = Wk,
    home_score = HG,
    away_score = AG,
  )


laliga2021footie = MultiSznFootieMaker(laliga2021sched)




# Schedule for La Liga 21-22,     season_id = 3 ###### 


laliga2122 = read_excel("laliga2122fbref.xlsx")


laliga2122[c("HG", "AG")] <- as.data.frame(do.call(rbind, strsplit(as.character(laliga2122$Score), "–", fixed = TRUE)))  
laliga2122 <- laliga2122[, !(names(laliga2122) %in% c("Time", "Attendance", "Referee", "Match Report","Notes"))]



VenueHome(laliga2122)

laliga2122sched <- as_tibble(laliga2122) %>% 
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
      Location=="Estadio de Mendizorroza" ~ "Alavés", 
      Location=="San Mamés"~"Athletic Club", 
      Location=="Estadio Wanda Metropolitano"~"Atlético Madrid", 
      Location=="Camp Nou"~"Barcelona", 
      Location=="Estadio Benito Villamarín"~"Betis", 
      Location=="Estadio Nuevo Mirandilla"~"Cádiz", 
      Location=="Estadio de Balaídos"~"Celta Vigo", 
      Location=="Estadio Manuel Martínez Valero"~"Elche", 
      Location=="RCDE Stadium"~"Espanyol", 
      Location=="Coliseum Alfonso Pérez"~"Getafe", 
      Location=="Estadio Nuevo Los Cármenes"~"Granada",
      Location=="Estadio Ciudad de Valencia"~"Levante", 
      Location=="Iberostar Estadi"~"Mallorca", 
      Location=="Estadio El Sadar"~"Osasuna", 
      Location=="Estadio del Rayo Vallecano"~"Rayo Vallecano", 
      Location=="Estadio Santiago Bernabéu"~"Real Madrid", 
      Location=="Estadio Municipal de Anoeta"~"Real Sociedad", 
      Location=="Estadio Ramón Sánchez Pizjuán"~"Sevilla", 
      Location=="Estadio de Mestalla"~"Valencia", 
      Location=="Estadio de la Cerámica"~"Villarreal"
    ),
    season_id = 3
  ) %>% rename(
    home_team = Home,
    away_team = Away,
    date=Date,
    Round.Number = Wk,
    home_score = HG,
    away_score = AG,
  )

laliga2122footie = MultiSznFootieMaker(laliga2122sched)


# Schedule for La Liga 22-23,     season_id = 4 ###### 
laliga2223 = presched("laliga2223raw.xlsx")

laliga2223sched <- as_tibble(laliga2223) %>% 
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
    ),
    season_id = 4
  ) %>% rename(
    home_team = Home,
    away_team = Away,
    date=Date,
    Round.Number = Wk,
    home_score = HG,
    away_score = AG,
  )



laliga2223footie = MultiSznFootieMaker(laliga2223sched)





##### now we have laliga1920footie laliga2021footie laliga2122footie, and laliga2223footie
##### we want to merge this into one tbl_df ######



#### first try fixing the data before running inla model

# alllaliga =  bind_rows(
#   laliga1920footie %>% mutate(season_id = case_when(date >= as.Date("2019-08-16") & date <= as.Date("2020-07-19") ~ 1)),
#   laliga2021footie %>% mutate(season_id = case_when(date >= as.Date("2020-09-11") & date <= as.Date("2021-05-23") ~ 2)),
#   laliga2122footie %>% mutate(season_id = case_when(date >= as.Date("2021-08-13") & date <= as.Date("2022-05-22") ~ 3)),
#   laliga2223footie %>% mutate(season_id = case_when(date >= as.Date("2022-08-12") ~ 4))
# )
# alllaliga <- alllaliga %>%
#   mutate(
#     Round.Number = ifelse(date < as.Date("2023-02-24"), 1, Round.Number)
#   )
#### above i make all the Round.numbers = 1 before the date of the games that are yet to be simulated, i dont know if i should make this NA or 0 ?




###### modelling #####

alllaligadata=
  # Here "fixes" the data
  alllaliga %>% 
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


eqn = Goal ~ Home + season_id + 
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
  
malllaliga=inla(formula = eqn,
               data=alllaligadata,
               family="poisson",
               control.predictor=list(compute=TRUE,link=1),
               control.compute=list(config=TRUE,dic=TRUE))
summary(malllaliga)

t5team_strength(malllaliga,alllaliga)


#### so i am now able to predict with all data from 2019-2023 


###### trying to do it in on calling instead? ????#####
laliga_combined <- rbind(laliga1920, laliga2021, laliga2122, laliga2223)

lvhtable(laliga_combined)
nrow(lvhtable(laliga_combined))
VenueHome(laliga_combined)



testallsched<- as_tibble(laliga_combined) %>% 
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
      Location=="Estadio de Mendizorroza"~"Alavés",
      Location=="Power Horse Stadium"~"Almería", 
      Location=="San Mamés"~"Athletic Club", 
      Location=="Estadio Cívitas Metropolitano"~"Atlético Madrid",
      Location=="Estadio Wanda Metropolitano"~"Atlético Madrid", 
      Location=="Camp Nou"~"Barcelona", 
      Location=="Estadio Benito Villamarín"~"Betis", 
      Location=="Estadio Nuevo Mirandilla"~"Cádiz", 
      Location=="Estadio Ramón de Carranza"~"Cádiz", 
      Location=="Estadio de Balaídos"~"Celta Vigo", 
      Location=="Estadio Municipal de Ipurúa"~"Eibar", 
      Location=="Estadio Manuel Martínez Valero"~"Elche", 
      Location=="RCDE Stadium"~"Espanyol",
      Location=="Coliseum Alfonso Pérez"~"Getafe", 
      Location=="Estadi Municipal de Montilivi"~"Girona", 
      Location=="Estadio Nuevo Los Cármenes"~"Granada", 
      Location=="Estadio El Alcoraz"~"Huesca",
      Location=="Estadio Municipal de Butarque"~"Leganés", 
      Location=="Estadio Ciudad de Valencia"~"Levante",
      Location=="Iberostar Estadi"~"Mallorca", 
      Location=="Estadio El Sadar"~"Osasuna", 
      Location=="Estadio del Rayo Vallecano"~"Rayo Vallecano", 
      Location=="Estadio Alfredo Di Stéfano"~"Real Madrid",
      Location=="Estadio Santiago Bernabéu"~"Real Madrid", 
      Location=="Estadio Municipal de Anoeta"~"Real Sociedad",
      Location=="Reale Arena"~"Real Sociedad", 
      Location=="Estadio Ramón Sánchez Pizjuán"~"Sevilla", 
      Location=="Estadio de Mestalla"~"Valencia", 
      Location=="Estadio Municipal José Zorrilla"~"Valladolid",
      Location=="Estadio Ciudad de Valencia"~"Villarreal",
      Location=="Estadio de la Cerámica"~"Villarreal"
    )
  ) %>% rename(
    home_team = Home,
    away_team = Away,
    date=Date,
    Round.Number = Wk,
    home_score = HG,
    away_score = AG,
  ) %>%
  mutate(
    Round.Number = ifelse(date < as.Date("2023-02-24"), NA, Round.Number),
    season_id = case_when(
    date >= as.Date("2019-08-16") & date <= as.Date("2020-07-19") ~ 1,
    date >= as.Date("2020-09-11") & date <= as.Date("2021-05-23") ~ 2,
    date >= as.Date("2021-08-13") & date <= as.Date("2022-05-22") ~ 3,
    date >= as.Date("2022-08-12") ~ 4
  ))

AllLaLiga = MultiSznFootieMaker(testallsched)
r=23
AllLaLigaData = datprep(AllLaLiga,23)

malllaliga=inla(formula = eqn,
                data=AllLaLigaData,
                family="poisson",
                control.predictor=list(compute=TRUE,link=1),
                control.compute=list(config=TRUE,dic=TRUE))
summary(malllaliga)

t5team_strength(malllaliga,AllLaLiga)



seriea2122 = presched("seriea2122.xlsx")

