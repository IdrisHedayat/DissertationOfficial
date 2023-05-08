source("FootieMaker.R")
source("UtilityFunctions.R")
#### combining all 4 seasons ####

laliga2223 = presched("laliga2223raw.xlsx")
laliga2122 = presched("laliga2122fbref.xlsx")
laliga2021 = presched("laliga2021fbref.xlsx")
laliga1920 = presched("laliga1920fbref.xlsx")

laliga_combined <- rbind(laliga1920, laliga2021, laliga2122, laliga2223)

lvhtable(laliga_combined)
nrow(lvhtable(laliga_combined))
VenueHome(laliga_combined)

laligaallsched<- as_tibble(laliga_combined) %>% 
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
    Round.Number = ifelse(date < as.Date("2023-02-23"), NA, Round.Number),
    # Round.Number = case_when(
    #         date <= as.Date("2023-02-23") ~ 1,
    #         date >= as.Date("2022-05-24") ~ 2
    # ),
    season_id = case_when(
      date <= as.Date("2020-07-19") ~ 1,
      date >= as.Date("2020-07-20") & date <= as.Date("2021-05-23") ~ 2,
      date >= as.Date("2021-05-24") & date <= as.Date("2022-05-22") ~ 3,
      date >= as.Date("2022-05-23") ~ 4
    ))

Alllaliga = MultiSznFootieMaker(laligaallsched)

#### data starts at round 22
AlllaligaData = datprep(Alllaliga,22)

#### testing out alllaliga inla #####

######### Formula ##############

eqB = Goal ~ Home + 
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

Malllaliga=inla(formula = eqB,
              data=AlllaligaData,
              family="poisson",
              control.predictor=list(compute=TRUE,link=1),
              control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
summary(Malllaliga)
# Watanabe-Akaike information criterion (WAIC) ...: 6788.99

t5team_strength(Malllaliga,Alllaliga)

##### WAIC COMBOS #####

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
      data = AlllaligaData,
      family = "poisson",
      control.predictor = list(compute = TRUE, link = 1),
      control.compute = list(config = TRUE, dic = TRUE, waic = TRUE)
    )
    waic_values[[paste(current_formula)]] <- current_model$waic$waic
  }
  
  # Save the results for the current chunk
  saveRDS(waic_values, paste0("llwaic_values_chunk_", chunk, ".rds"))
  
  # Clear the waic_values list for the next chunk
  waic_values <- list()
}

# Combine the results from all chunks
waic_values_combined <- list()
for (chunk in 1:num_chunks) {
  waic_values_combined <- c(waic_values_combined, readRDS(paste0("llwaic_values_chunk_", chunk, ".rds")))
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
# Renaming the formula column
top_10_models = top_10_models %>%
  mutate(Formula= gsub("f\\(factor\\(Team\\), model = 'iid'\\)", "Att", Formula)) %>%
  mutate(Formula = gsub("f\\(factor\\(Opponent\\), model = 'iid'\\)", "Def", Formula))

top_10_models

saveRDS(top_10_models,"alllaliga_top_10_models.rds")





##### Model Validation With laliga 21-22 HALFWAY SEASON ONLY TRAINING DATA #######
#### this is only if we are using 21/22 seasons which doesnt make sense because of overfitting

# laliga2122val = presched("laliga2122trainingse.xlsx")
# 
# laliga2122valsched <- as_tibble(laliga2122val) %>% 
#   rename(
#     Location = Venue
#   ) %>% 
#   mutate(
#     Date=as.Date(Date, format = "%d/%m/%Y"),
#     tournament="Bundesliga",
#     HG = as.numeric(HG),
#     AG = as.numeric(AG),
#     ID_game=row_number(),
#     Location=case_when(
#       Location=="Estadio de Mendizorroza"~"Alavés",
#       Location=="Power Horse Stadium"~"Almería", 
#       Location=="San Mamés"~"Athletic Club", 
#       Location=="Estadio Cívitas Metropolitano"~"Atlético Madrid",
#       Location=="Estadio Wanda Metropolitano"~"Atlético Madrid", 
#       Location=="Camp Nou"~"Barcelona", 
#       Location=="Estadio Benito Villamarín"~"Betis", 
#       Location=="Estadio Nuevo Mirandilla"~"Cádiz", 
#       Location=="Estadio Ramón de Carranza"~"Cádiz", 
#       Location=="Estadio de Balaídos"~"Celta Vigo", 
#       Location=="Estadio Municipal de Ipurúa"~"Eibar", 
#       Location=="Estadio Manuel Martínez Valero"~"Elche", 
#       Location=="RCDE Stadium"~"Espanyol",
#       Location=="Coliseum Alfonso Pérez"~"Getafe", 
#       Location=="Estadi Municipal de Montilivi"~"Girona", 
#       Location=="Estadio Nuevo Los Cármenes"~"Granada", 
#       Location=="Estadio El Alcoraz"~"Huesca",
#       Location=="Estadio Municipal de Butarque"~"Leganés", 
#       Location=="Estadio Ciudad de Valencia"~"Levante",
#       Location=="Iberostar Estadi"~"Mallorca", 
#       Location=="Estadio El Sadar"~"Osasuna", 
#       Location=="Estadio del Rayo Vallecano"~"Rayo Vallecano", 
#       Location=="Estadio Alfredo Di Stéfano"~"Real Madrid",
#       Location=="Estadio Santiago Bernabéu"~"Real Madrid", 
#       Location=="Estadio Municipal de Anoeta"~"Real Sociedad",
#       Location=="Reale Arena"~"Real Sociedad", 
#       Location=="Estadio Ramón Sánchez Pizjuán"~"Sevilla", 
#       Location=="Estadio de Mestalla"~"Valencia", 
#       Location=="Estadio Municipal José Zorrilla"~"Valladolid",
#       Location=="Estadio Ciudad de Valencia"~"Villarreal",
#       Location=="Estadio de la Cerámica"~"Villarreal")
#   ) %>% rename(
#     home_team = Home,
#     away_team = Away,
#     date=Date,
#     Round.Number = Wk,
#     home_score = HG,
#     away_score = AG,
#   )

##### Set seed ######
set.seed(12345)

##### introducing baseline and optimal formula based on waic value list #####

BaselineForm =  Goal ~ Home + 
  f(factor(Team), model = "iid") +
  f(factor(Opponent), model = "iid")  

OpForm = Goal ~ Home + 
  Gpg +
  GCpg +
  GDdiff +
  f(factor(Team), model = "iid") +
  f(factor(Opponent), model = "iid")  


##### code if doing validation using only 21-22 season, but this leads to overfitting, so baseline does better #####


# laliga2122footie = FootieMaker(laliga2122valsched)
# laliga2122dat = datprep(laliga2122footie,23)
# 
# 
# mbaselinelaligaval = inla(formula = BaselineForm,
#                       data=laliga2122dat,
#                       family="poisson",
#                       control.predictor=list(compute=TRUE,link=1),
#                       control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
# summary(mbaselinelaligaval)
# 
# 
# blaliga2122footie23 = roundNinlampo(23,laliga2122footie,frm = BaselineForm)
# blaliga2122r24 = roundNinlampo(24,blaliga2122footie23$footieN,frm = BaselineForm)
# blaliga2122r25 = roundNinlampo(25,blaliga2122r24$footieN,frm = BaselineForm) 
# blaliga2122r26 = roundNinlampo(26,blaliga2122r25$footieN,frm = BaselineForm) 
# blaliga2122r27 = roundNinlampo(27,blaliga2122r26$footieN,frm = BaselineForm) 
# blaliga2122r28 = roundNinlampo(28,blaliga2122r27$footieN,frm = BaselineForm) 
# blaliga2122r29 = roundNinlampo(29,blaliga2122r28$footieN,frm = BaselineForm) 
# blaliga2122r30 = roundNinlampo(30,blaliga2122r29$footieN,frm = BaselineForm) 
# blaliga2122r31 = roundNinlampo(31,blaliga2122r30$footieN,frm = BaselineForm) 
# blaliga2122r32 = roundNinlampo(32,blaliga2122r31$footieN,frm = BaselineForm) 
# blaliga2122r33 = roundNinlampo(33,blaliga2122r32$footieN,frm = BaselineForm) 
# blaliga2122r34 = roundNinlampo(34,blaliga2122r33$footieN,frm = BaselineForm) 
# 
# laliga2122finaltablebaseline = t5tablerounrR(34,blaliga2122r34$footieN)
# laliga2122finaltablebaseline
# saveRDS(laliga2122finaltablebaseline, "alllaliga2122valfinaltablebaseline.rds" )
# 
# 
# ### using optimal formula
# 
# 
# mopformlaligaval = inla(formula = OpForm,
#                       data=laliga2122dat,
#                       family="poisson",
#                       control.predictor=list(compute=TRUE,link=1),
#                       control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
# 
# summary(mopformlaligaval)
# 
# 
# laliga2122footie2 = laliga2122footie
# laliga2122footie232 = roundNinlampo(23,laliga2122footie2,frm=OpForm)
# laliga2122r242 = roundNinlampo(24,laliga2122footie232$footieN,frm=OpForm)
# laliga2122r252 = roundNinlampo(25,laliga2122r242$footieN,frm=OpForm) 
# laliga2122r262 = roundNinlampo(26,laliga2122r252$footieN,frm=OpForm) 
# laliga2122r272 = roundNinlampo(27,laliga2122r262$footieN,frm=OpForm) 
# laliga2122r282 = roundNinlampo(28,laliga2122r272$footieN,frm=OpForm) 
# laliga2122r292 = roundNinlampo(29,laliga2122r282$footieN,frm=OpForm) 
# laliga2122r302 = roundNinlampo(30,laliga2122r292$footieN,frm=OpForm) 
# laliga2122r312 = roundNinlampo(31,laliga2122r302$footieN,frm=OpForm) 
# laliga2122r322 = roundNinlampo(32,laliga2122r312$footieN,frm=OpForm) 
# laliga2122r332 = roundNinlampo(33,laliga2122r322$footieN,frm=OpForm) 
# laliga2122r342 = roundNinlampo(34,laliga2122r332$footieN,frm=OpForm) 
# 
# laliga2122finaltableOPFORM = t5tablerounrR(34,laliga2122r342$footieN)
# laliga2122finaltableOPFORM
# saveRDS(laliga2122finaltableOPFORM, "laliga2122AfinaltableOPFORM.rds" )
# 
#     ### laliga Mod Val Results ###
# ######  reading table observed output
# 
# bl2122tab = read_excel("laliga2122tab.xlsx") %>%
#   as.tibble() %>%
#   mutate(total_points = Pts,
#          Team = Squad) %>%
#   select(Team, total_points) %>%
#   arrange(Team)
# 
# bl2122OPFORM = laliga2122finaltableOPFORM %>%
#   select(Team, total_points) %>%
#   arrange(Team)
# 
# bl2122baseline = laliga2122finaltablebaseline %>%
#   select(Team, total_points) %>%
#   arrange(Team)
# 
# 
# # Calculate points difference for baseline model
# bl2122baseline$Points_Difference <- abs(bl2122baseline$total_points - bl2122tab$total_points)
# 
# # Calculate points difference for optimized model
# bl2122OPFORM$Points_Difference <- abs(bl2122OPFORM$total_points - bl2122tab$total_points)
# 
# # Combine the results
# comparison_table <- data.frame(
#   Team = bl2122tab$Team,
#   Observed_Points = bl2122tab$total_points,
#   Baseline_Points = bl2122baseline$total_points,
#   Baseline_Difference = bl2122baseline$Points_Difference,
#   Optimized_Points = bl2122OPFORM$total_points,
#   Optimized_Difference = bl2122OPFORM$Points_Difference
# ) 
# 
# # Print the comparison
# print(comparison_table)
# comptab=as.tibble(comparison_table)
# saveRDS(comptab, "comptabbl2122")
# 
# # Calculate the sum of squared differences for the baseline model
# sum_sq_diff_baseline <- sum(bl2122baseline$Points_Difference ^ 2)
# 
# # Calculate the sum of squared differences for the optimized model
# sum_sq_diff_optimized <- sum(bl2122OPFORM$Points_Difference ^ 2)
# 
# # Print the results
# cat("Mean squared error for the baseline model:", sum_sq_diff_baseline/20, "\n")
# cat("Mean squared error for the optimized model:", sum_sq_diff_optimized/20, "\n")
# 







#### this time we are using 3 seasons up to halfway the 21/22 season point####
laliga2122val = presched("laliga2122trainingse.xlsx")
laliga_combined_val2122 <- rbind(laliga1920, laliga2021, laliga2122val)

lvhtable(laliga_combined)
nrow(lvhtable(laliga_combined))
VenueHome(laliga_combined)

laligaallsched <- as_tibble(laliga_combined_val2122) %>% 
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
    Round.Number = ifelse(date <= as.Date("2022-02-17"), NA, Round.Number),
    # Round.Number = case_when(
    #         date <= as.Date("2023-02-23") ~ 1,
    #         date >= as.Date("2022-05-24") ~ 2
    # ),
    season_id = case_when(
      date <= as.Date("2020-07-19") ~ 1,
      date >= as.Date("2020-07-20") & date <= as.Date("2021-05-23") ~ 2,
      date >= as.Date("2021-05-24") & date <= as.Date("2022-05-22") ~ 3
    ))


### below is the code used in validation if we only use 1 round in stead of round by round simulating.

### this is the footie dataset we will be using as validation (predicting 21/22 then comparing with the observed actual results)

#### if sim 1 round instead of doing round by round ?####
# Alllaliga2122_val = MultiSznFootieMaker(laligaallsched) %>%
#   mutate(
#     Round.Number = ifelse(date < as.Date("2022-02-18"), NA, Round.Number)) %>%
#   mutate(
#     Round.Number = case_when(date >= as.Date("2022-02-18") ~ 1)
#   )
# laliga2122dat = datprep(Alllaliga2122_val,1)
# mbaselinelaligaval = inla(formula = BaselineForm,
#                         data=laliga2122dat,
#                         family="poisson",
#                         control.predictor=list(compute=TRUE,link=1),
#                         control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
# summary(mbaselinelaligaval)
# blaliga2122footie23 = roundNinlampo(1,Alllaliga2122_val,frm = BaselineForm)
# 
# total_points_2122_val_baseline  = blaliga2122footie23$footieN %>%
#   filter(season_id == 3) %>%
#   group_by(Team) %>%
#   summarize(total_points = sum(points_won)) %>%
#   arrange(desc(total_points))
# 
# saveRDS(total_points_2122_val_baseline, "alllaligaupto2122valfinaltablebaseline.rds" )
# ### using optimal formul
# 
# mbaselinelaligaval = inla(formula = OpForm,
#                         data=laliga2122dat,
#                         family="poisson",
#                         control.predictor=list(compute=TRUE,link=1),
#                         control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
# summary(mbaselinelaligaval)
# 
# Alllaliga2122_val2 = Alllaliga2122_val
# laliga2122footie232 = roundNinlampo(1,Alllaliga2122_val2,frm=OpForm)
# 
# 
# total_points_2122_val_opfrom= laliga2122footie232$footieN %>%
#   filter(season_id == 3) %>%
#   group_by(Team) %>%
#   summarize(total_points = sum(points_won)) %>%
#   arrange(desc(total_points))
# 
# laliga2122finaltableOPFORM = t5tablerounrR(1,laliga2122r342$footieN)
# laliga2122finaltableOPFORM
# saveRDS(laliga2122finaltableOPFORM, "alllaligaupto2122valfinaltableopform.rds" )
# 
# ######  reading table observed output
# 
# bl2122tab = read_excel("laliga2122tab.xlsx") %>%
#   as.tibble() %>%
#   mutate(total_points = Pts,
#          Team = Squad) %>%
#   select(Team, total_points) %>%
#   arrange(Team)
# 
# bl2122OPFORM = total_points_2122_val_opfrom%>%
#   select(Team, total_points) %>%
#   arrange(Team)
# 
# bl2122baseline = total_points_2122_val_baseline %>%
#   select(Team, total_points) %>%
#   arrange(Team)
# 
# 
# # Calculate points difference for baseline model
# bl2122baseline$Points_Difference <- abs(bl2122baseline$total_points - bl2122tab$total_points)
# 
# # Calculate points difference for optimized model
# bl2122OPFORM$Points_Difference <- abs(bl2122OPFORM$total_points - bl2122tab$total_points)
# 
# # Combine the results
# comparison_table <- data.frame(
#   Team = bl2122tab$Team,
#   Observed_Points = bl2122tab$total_points,
#   Baseline_Points = bl2122baseline$total_points,
#   Baseline_Difference = bl2122baseline$Points_Difference,
#   Optimized_Points = bl2122OPFORM$total_points,
#   Optimized_Difference = bl2122OPFORM$Points_Difference
# ) 
# 
# # Print the comparison
# print(comparison_table)
# comptab=as.tibble(comparison_table)
# saveRDS(comptab, "comptabbl2122")
# 
# # Calculate the sum of squared differences for the baseline model
# sum_sq_diff_baseline <- sum(bl2122baseline$Points_Difference ^ 2)
# 
# # Calculate the sum of squared differences for the optimized model
# sum_sq_diff_optimized <- sum(bl2122OPFORM$Points_Difference ^ 2)
# 
# # Print the results
# cat("Mean squared error for the baseline model:", sum_sq_diff_baseline/20, "\n")
# cat("Mean squared error for the optimized model:", sum_sq_diff_optimized/20, "\n")



##### Alllaligaupto21/22 op vs baseline round by round ####

Alllaliga2122_val = MultiSznFootieMaker(laligaallsched)


blaliga2122r25 = roundNinlampo(25,Alllaliga2122_val, frm = BaselineForm) 
blaliga2122r26 = roundNinlampo(26,blaliga2122r25$footieN,frm = BaselineForm) 
blaliga2122r27 = roundNinlampo(27,blaliga2122r26$footieN,frm = BaselineForm) 
blaliga2122r28 = roundNinlampo(28,blaliga2122r27$footieN,frm = BaselineForm) 
blaliga2122r29 = roundNinlampo(29,blaliga2122r28$footieN,frm = BaselineForm) 
blaliga2122r30 = roundNinlampo(30,blaliga2122r29$footieN,frm = BaselineForm) 
blaliga2122r31 = roundNinlampo(31,blaliga2122r30$footieN,frm = BaselineForm) 
blaliga2122r32 = roundNinlampo(32,blaliga2122r31$footieN,frm = BaselineForm) 
blaliga2122r33 = roundNinlampo(33,blaliga2122r32$footieN,frm = BaselineForm) 
blaliga2122r34 = roundNinlampo(34,blaliga2122r33$footieN,frm = BaselineForm) 
blaliga2122r35 = roundNinlampo(35,blaliga2122r34$footieN,frm = BaselineForm) 
blaliga2122r36 = roundNinlampo(36,blaliga2122r35$footieN,frm = BaselineForm) 
blaliga2122r37 = roundNinlampo(37,blaliga2122r36$footieN,frm = BaselineForm) 
blaliga2122r38 = roundNinlampo(38,blaliga2122r37$footieN,frm = BaselineForm) 


laliga2122finaltablebaseline = blaliga2122r38$footieN %>%
  filter(season_id == 3) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points))

laliga2122finaltablebaseline

saveRDS(laliga2122finaltablebaseline, "alllaliga2122valfinaltablebaseline.rds" )


### using optimal formula


laliga2122footie2 = Alllaliga2122_val

laliga2122r252 = roundNinlampo(25,laliga2122footie2,frm=OpForm) 
laliga2122r262 = roundNinlampo(26,laliga2122r252$footieN,frm=OpForm) 
laliga2122r272 = roundNinlampo(27,laliga2122r262$footieN,frm=OpForm) 
laliga2122r282 = roundNinlampo(28,laliga2122r272$footieN,frm=OpForm) 
laliga2122r292 = roundNinlampo(29,laliga2122r282$footieN,frm=OpForm) 
laliga2122r302 = roundNinlampo(30,laliga2122r292$footieN,frm=OpForm) 
laliga2122r312 = roundNinlampo(31,laliga2122r302$footieN,frm=OpForm) 
laliga2122r322 = roundNinlampo(32,laliga2122r312$footieN,frm=OpForm) 
laliga2122r332 = roundNinlampo(33,laliga2122r322$footieN,frm=OpForm) 
laliga2122r342 = roundNinlampo(34,laliga2122r332$footieN,frm=OpForm) 
laliga2122r352 = roundNinlampo(35,laliga2122r342$footieN,frm=OpForm) 
laliga2122r362 = roundNinlampo(36,laliga2122r352$footieN,frm=OpForm) 
laliga2122r372 = roundNinlampo(37,laliga2122r362$footieN,frm=OpForm) 
laliga2122r382 = roundNinlampo(38,laliga2122r372$footieN,frm=OpForm) 

laliga2122finaltableOPFORM = laliga2122r382$footieN %>%
  filter(season_id == 3) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points))

laliga2122finaltableOPFORM

saveRDS(laliga2122finaltableOPFORM, "laliga2122AfinaltableOPFORM.rds" )

###### laliga Mod Val Results ####
######  reading table observed output

bl2122tab = read_excel("laliga2122tab.xlsx") %>%
  as.tibble() %>%
  mutate(total_points = Pts,
         Team = Squad) %>%
  select(Team, total_points) %>%
  arrange(Team)

bl2122OPFORM = laliga2122finaltableOPFORM %>%
  select(Team, total_points) %>%
  arrange(Team)

bl2122baseline = laliga2122finaltablebaseline %>%
  select(Team, total_points) %>%
  arrange(Team)





# Calculate points difference for baseline model
bl2122baseline$Points_Difference <- abs(bl2122baseline$total_points - bl2122tab$total_points)

# Calculate points difference for optimized model
bl2122OPFORM$Points_Difference <- abs(bl2122OPFORM$total_points - bl2122tab$total_points)

# Combine the results
comparison_table <- data.frame(
  Team = bl2122tab$Team,
  Observed_Points = bl2122tab$total_points,
  Baseline_Points = bl2122baseline$total_points,
  Baseline_Difference = bl2122baseline$Points_Difference,
  Optimized_Points = bl2122OPFORM$total_points,
  Optimized_Difference = bl2122OPFORM$Points_Difference
) 

# Print the comparison
print(comparison_table)
comptab=as.tibble(comparison_table)
saveRDS(comptab, "laligacomptabbl2122")

# Calculate the sum of squared differences for the baseline model
sum_sq_diff_baseline <- sum(bl2122baseline$Points_Difference ^ 2)

# Calculate the sum of squared differences for the optimized model
sum_sq_diff_optimized <- sum(bl2122OPFORM$Points_Difference ^ 2)

# Print the results
cat("Mean squared error for the baseline model:", sum_sq_diff_baseline/20, "\n")
cat("Mean squared error for the optimized model:", sum_sq_diff_optimized/20, "\n")


###### IT WORKSSSSSS WE GOT BETTER MEAN SQARE ERROR FOR OPTIMISED MODEL !!!!! ########
