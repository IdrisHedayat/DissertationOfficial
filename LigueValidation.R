source("FootieMaker.R")
source("UtilityFunctions.R")
#### combining all 4 seasons ####

ligue2223 = presched("ligue12223raw.xlsx") 
ligue2122 = presched("ligue2122.xlsx")
ligue2021 = presched("ligue2021.xlsx")
ligue1920 = presched("ligue1920.xlsx")

ligue_combined <- rbind(ligue1920, ligue2021, ligue2122, ligue2223)

lvhtable(ligue_combined)
nrow(lvhtable(ligue_combined))
VenueHome(ligue_combined)

ligueallsched<- as_tibble(ligue_combined) %>% 
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
      Location=="Decathlon Arena - Stade Pierre-Mauroy"~"Lille", 
      Location=="Groupama Stadium"~"Lyon",
      Location=="Matmut Stadium de Gerland"~"Lyon", 
      Location=="Orange Vélodrome"~"Marseille", 
      Location=="Parc des Princes"~"Paris S-G",
      Location=="Roazhon Park"~"Rennes",
      Location=="Stade Auguste-Delaune"~"Reims", 
      Location=="Stade Auguste-Delaune II"~"Reims",
      Location=="Stade Bollaert-Delelis"~"Lens", 
      Location=="Stade Crédit Agricole la Licorne"~"Amiens", 
      Location=="Stade de l'Abbé Deschamps"~"Auxerre", 
      Location=="Stade de l'Aube" & Home == "Nice"~"Nice",
      Location=="Stade de l'Aube" & Home == "Troyes"~"Troyes", 
      Location=="Stade de la Beaujoire - Louis Fonteneau"~"Nantes",
      Location=="Stade de la Meinau"~"Strasbourg",
      Location=="Stade de la Mosson"~"Montpellier", 
      Location=="Stade de la Mosson-Mondial 98"~"Montpellier", 
      Location=="Stade de Nice"~"Nice", 
      Location=="Stade des Costières"~"Nîmes", 
      Location=="Stade Francis-Le Blé"~"Brest", 
      Location=="Stade François Coty"~"Ajaccio", 
      Location=="Stade Gabriel Montpied"~"Clermont Foot", 
      Location=="Stade Gaston Gérard"~"Dijon",
      Location=="Stade Geoffroy-Guichard"~"Saint-Étienne", 
      Location=="Stade Louis II."~"Monaco", 
      Location=="Stade Matmut-Atlantique"~"Bordeaux", 
      Location=="Stade Municipal du Ray"~"Nice", 
      Location=="Stade Pierre-Mauroy"~"Lille",
      Location=="Stade Raoul-Barrière"~"Montpellier",
      Location=="Stade Raymond Kopa"~"Angers",
      Location=="Stade Saint-Symphorien"~"Metz", 
      Location=="Stade Yves Allainmat - Le Moustoir"~"Lorient", 
      Location=="Stadium Lille Métropole"~"Lille",
      Location=="Stadium Lille Métropole - Terrain Annexe"~"Lille",
      Location=="Stadium Municipal"~"Toulouse"
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
    # Round.Number = case_when(
    #         date <= as.Date("2023-02-23") ~ 1,
    #         date >= as.Date("2022-05-24") ~ 2
    # ),
    season_id = case_when(
      date <= as.Date("2020-03-08") ~ 1,
      date >= as.Date("2020-08-21") & date <= as.Date("2019-08-09") ~ 2,
      date >= as.Date("2021-08-06") & date <= as.Date("2022-05-21") ~ 3,
      date >= as.Date("2022-05-23") ~ 4
    ))

Allligue = MultiSznFootieMaker(ligueallsched)

#### ligue data starts at round 25
AllligueData = datprep(Allligue,25)

#### testing out allligue inla #####

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

Mallligue=inla(formula = eqB,
               data=AllligueData,
               family="poisson",
               control.predictor=list(compute=TRUE,link=1),
               control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
summary(Mallligue)
# Watanabe-Akaike information criterion (WAIC) ...: 6788.99

allliguedef = t5team_strength(Mallligue,Allligue,"defense") + ylab("Teams")

ggsave("allliguedef.png",allliguedef,width = 8, height = 4, dpi = 300)

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
      data = AllligueData,
      family = "poisson",
      control.predictor = list(compute = TRUE, link = 1),
      control.compute = list(config = TRUE, dic = TRUE, waic = TRUE)
    )
    waic_values[[paste(current_formula)]] <- current_model$waic$waic
  }
  
  # Save the results for the current chunk
  saveRDS(waic_values, paste0("liguewaic_values_chunk_", chunk, ".rds"))
  
  # Clear the waic_values list for the next chunk
  waic_values <- list()
}

# Combine the results from all chunks
waic_values_combined <- list()
for (chunk in 1:num_chunks) {
  waic_values_combined <- c(waic_values_combined, readRDS(paste0("liguewaic_values_chunk_", chunk, ".rds")))
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

saveRDS(top_10_models,"allligue_top_10_models.rds")







##### Set seed ######
set.seed(12345)

##### introducing baseline and optimal formula based on waic value list #####

BaselineForm =  Goal ~ Home + 
  f(factor(Team), model = "iid") +
  f(factor(Opponent), model = "iid")  

OpForm = Goal ~ Home
  Gpg +
  GCpg +
  GDdiff +
  f(factor(Team), model = "iid") +
  f(factor(Opponent), model = "iid")  




#### this time we are using 3 seasons up to halfway the 21/22 season point####
ligue2122val = presched("ligue2122trainingse.xlsx")
ligue_combined_val2122 <- rbind(ligue1920, ligue2021, ligue2122val)

lvhtable(ligue_combined)
nrow(lvhtable(ligue_combined))
VenueHome(ligue_combined)

ligueallsched <- as_tibble(ligue_combined_val2122) %>% 
  rename(
    Location = Venue
  ) %>% 
  mutate(
    Date=as.Date(Date, format = "%d/%m/%Y"),
    tournament="ligue 1",
    HG = as.numeric(HG),
    AG = as.numeric(AG),
    ID_game=row_number(),
    Location=case_when(
      Location=="Decathlon Arena - Stade Pierre-Mauroy"~"Lille", 
      Location=="Groupama Stadium"~"Lyon",
      Location=="Matmut Stadium de Gerland"~"Lyon", 
      Location=="Orange Vélodrome"~"Marseille", 
      Location=="Parc des Princes"~"Paris S-G",
      Location=="Roazhon Park"~"Rennes",
      Location=="Stade Auguste-Delaune"~"Reims", 
      Location=="Stade Auguste-Delaune II"~"Reims",
      Location=="Stade Bollaert-Delelis"~"Lens", 
      Location=="Stade Crédit Agricole la Licorne"~"Amiens", 
      Location=="Stade de l'Abbé Deschamps"~"Auxerre", 
      Location=="Stade de l'Aube" & Home == "Nice"~"Nice",
      Location=="Stade de l'Aube" & Home == "Troyes"~"Troyes", 
      Location=="Stade de la Beaujoire - Louis Fonteneau"~"Nantes",
      Location=="Stade de la Meinau"~"Strasbourg",
      Location=="Stade de la Mosson"~"Montpellier", 
      Location=="Stade de la Mosson-Mondial 98"~"Montpellier", 
      Location=="Stade de Nice"~"Nice", 
      Location=="Stade des Costières"~"Nîmes", 
      Location=="Stade Francis-Le Blé"~"Brest", 
      Location=="Stade François Coty"~"Ajaccio", 
      Location=="Stade Gabriel Montpied"~"Clermont Foot", 
      Location=="Stade Gaston Gérard"~"Dijon",
      Location=="Stade Geoffroy-Guichard"~"Saint-Étienne", 
      Location=="Stade Louis II."~"Monaco", 
      Location=="Stade Matmut-Atlantique"~"Bordeaux", 
      Location=="Stade Municipal du Ray"~"Nice", 
      Location=="Stade Pierre-Mauroy"~"Lille",
      Location=="Stade Raoul-Barrière"~"Montpellier",
      Location=="Stade Raymond Kopa"~"Angers",
      Location=="Stade Saint-Symphorien"~"Metz", 
      Location=="Stade Yves Allainmat - Le Moustoir"~"Lorient", 
      Location=="Stadium Lille Métropole"~"Lille",
      Location=="Stadium Lille Métropole - Terrain Annexe"~"Lille",
      Location=="Stadium Municipal"~"Toulouse"
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
      Round.Number = ifelse(date < as.Date("2022-02-11"), NA, Round.Number),
      season_id = case_when(
        date <= as.Date("2020-03-08") ~ 1,
        date >= as.Date("2020-08-21") & date <= as.Date("2019-08-09") ~ 2,
        date >= as.Date("2021-08-06") ~ 3
      ))


### vlidation data starts simulating at round 24 in 21/22 season
##### Allligueupto21/22 op vs baseline round by round ####

Allligue2122_val = MultiSznFootieMaker(ligueallsched)


bligue2122r24 = roundNinlampo(24,Allligue2122_val, frm = BaselineForm) 
bligue2122r25 = roundNinlampo(25,bligue2122r24$footieN,frm = BaselineForm) 
bligue2122r26 = roundNinlampo(26,bligue2122r25$footieN,frm = BaselineForm) 
bligue2122r27 = roundNinlampo(27,bligue2122r26$footieN,frm = BaselineForm) 
bligue2122r28 = roundNinlampo(28,bligue2122r27$footieN,frm = BaselineForm) 
bligue2122r29 = roundNinlampo(29,bligue2122r28$footieN,frm = BaselineForm) 
bligue2122r30 = roundNinlampo(30,bligue2122r29$footieN,frm = BaselineForm) 
bligue2122r31 = roundNinlampo(31,bligue2122r30$footieN,frm = BaselineForm) 
bligue2122r32 = roundNinlampo(32,bligue2122r31$footieN,frm = BaselineForm) 
bligue2122r33 = roundNinlampo(33,bligue2122r32$footieN,frm = BaselineForm) 
bligue2122r34 = roundNinlampo(34,bligue2122r33$footieN,frm = BaselineForm) 
bligue2122r35 = roundNinlampo(35,bligue2122r34$footieN,frm = BaselineForm) 
bligue2122r36 = roundNinlampo(36,bligue2122r35$footieN,frm = BaselineForm) 
bligue2122r37 = roundNinlampo(37,bligue2122r36$footieN,frm = BaselineForm) 
bligue2122r38 = roundNinlampo(38,bligue2122r37$footieN,frm = BaselineForm) 


ligue2122finaltablebaseline = bligue2122r38$footieN %>%
  filter(season_id == 3) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points))

ligue2122finaltablebaseline

saveRDS(ligue2122finaltablebaseline, "allligue2122valfinaltablebaseline.rds" )


### using optimal formula


ligue2122footie2 = Allligue2122_val

ligue2122r242 = roundNinlampo(24,ligue2122footie2, frm = OpForm) 
ligue2122r252 = roundNinlampo(25,ligue2122r242$footieN,frm = OpForm) 
ligue2122r262 = roundNinlampo(26,ligue2122r252$footieN,frm=OpForm) 
ligue2122r272 = roundNinlampo(27,ligue2122r262$footieN,frm=OpForm) 
ligue2122r282 = roundNinlampo(28,ligue2122r272$footieN,frm=OpForm) 
ligue2122r292 = roundNinlampo(29,ligue2122r282$footieN,frm=OpForm) 
ligue2122r302 = roundNinlampo(30,ligue2122r292$footieN,frm=OpForm) 
ligue2122r312 = roundNinlampo(31,ligue2122r302$footieN,frm=OpForm) 
ligue2122r322 = roundNinlampo(32,ligue2122r312$footieN,frm=OpForm) 
ligue2122r332 = roundNinlampo(33,ligue2122r322$footieN,frm=OpForm) 
ligue2122r342 = roundNinlampo(34,ligue2122r332$footieN,frm=OpForm) 
ligue2122r352 = roundNinlampo(35,ligue2122r342$footieN,frm=OpForm) 
ligue2122r362 = roundNinlampo(36,ligue2122r352$footieN,frm=OpForm) 
ligue2122r372 = roundNinlampo(37,ligue2122r362$footieN,frm=OpForm) 
ligue2122r382 = roundNinlampo(38,ligue2122r372$footieN,frm=OpForm) 

ligue2122finaltableOPFORM = ligue2122r382$footieN %>%
  filter(season_id == 3) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points))

ligue2122finaltableOPFORM

saveRDS(ligue2122finaltableOPFORM, "ligue2122AfinaltableOPFORM.rds" )

###### ligue Mod Val Results ####
######  reading table observed output

bl2122tab = read_excel("ligue2122tab.xlsx") %>%
  as.tibble() %>%
  mutate(total_points = Pts,
         Team = Squad) %>%
  select(Team, total_points) %>%
  arrange(Team)

bl2122OPFORM = ligue2122finaltableOPFORM %>%
  select(Team, total_points) %>%
  arrange(Team)

bl2122baseline = ligue2122finaltablebaseline %>%
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
saveRDS(comptab, "liguecomptabbl2122")

# Calculate the sum of squared differences for the baseline model
sum_sq_diff_baseline <- sum(bl2122baseline$Points_Difference ^ 2)

# Calculate the sum of squared differences for the optimized model
sum_sq_diff_optimized <- sum(bl2122OPFORM$Points_Difference ^ 2)

# Print the results
cat("Mean squared error for the baseline model:", sum_sq_diff_baseline/20, "\n")
cat("Mean squared error for the optimized model:", sum_sq_diff_optimized/20, "\n")


###### IT WORKSSSSSS WE GOT BETTER MEAN SQARE ERROR FOR OPTIMISED MODEL !!!!! ########


#### Final Predictions ####
# since for ligue 1 the optimal forula was the baseline model

OpForm =  Goal ~ Home + 
  f(factor(Team), model = "iid") +
  f(factor(Opponent), model = "iid")  

Allliguefinal = Allligue

Mallligue=inla(formula = OpForm,
                data=Allliguefinal,
                family="poisson",
                control.predictor=list(compute=TRUE,link=1),
                control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
Mallliguesum = summary(Mallligue)
saveRDS(Mallliguesum,"MallliguesumFinal.rds")



 
ligue2223r25 = roundNinlampo(25,Allliguefinal, frm = OpForm) 
ligue2223r26 = roundNinlampo(26,ligue2223r25$footieN,frm = OpForm) 
ligue2223r27 = roundNinlampo(27,ligue2223r26$footieN,frm = OpForm) 
ligue2223r28 = roundNinlampo(28,ligue2223r27$footieN,frm = OpForm) 
ligue2223r29 = roundNinlampo(29,ligue2223r28$footieN,frm = OpForm) 
ligue2223r30 = roundNinlampo(30,ligue2223r29$footieN,frm = OpForm) 
ligue2223r31 = roundNinlampo(31,ligue2223r30$footieN,frm = OpForm) 
ligue2223r32 = roundNinlampo(32,ligue2223r31$footieN,frm = OpForm) 
ligue2223r33 = roundNinlampo(33,ligue2223r32$footieN,frm = OpForm) 
ligue2223r34 = roundNinlampo(34,ligue2223r33$footieN,frm = OpForm) 
ligue2223r35 = roundNinlampo(35,ligue2223r34$footieN,frm = OpForm) 
ligue2223r36 = roundNinlampo(36,ligue2223r35$footieN,frm = OpForm) 
ligue2223r37 = roundNinlampo(37,ligue2223r36$footieN,frm = OpForm) 
ligue2223r38 = roundNinlampo(38,ligue2223r37$footieN,frm = OpForm) 


ligue2223finaltab = ligue2223r38$footieN %>%
  filter(season_id == 4) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points)) %>%
  ungroup()

ligue2223finaltab
saveRDS(ligue2223finaltab,"ligue2223TabWithSuperLeaguers")

#### Final Predictions for league WITHUOT teams joining super league #####

OpForm = Goal ~ Home + 
  f(factor(Team), model = "iid") +
  f(factor(Opponent), model = "iid")  

teams_to_remove_ligue <- c("Paris S-G")


Allliguefinal2 <- ligue2223r38$footieN  %>%
  filter(!(Team %in% teams_to_remove_ligue | Opponent %in% teams_to_remove_ligue))

ligue2223finaltab2 = Allliguefinal2 %>%
  filter(season_id == 4) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points))

ligue2223finaltab2
saveRDS(ligue2223finaltab2,"ligue2223TabWithoutSuperLeaguers")

# Mallligue2=inla(formula = OpForm,
#                data=Allliguefinal2,
#                family="poisson",
#                control.predictor=list(compute=TRUE,link=1),
#                control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
# Mallliguesum2 = summary(Mallligue2)
# saveRDS(Mallliguesum2,"MallliguesumFinal2.rds")
# 
# ligue2223r252 = roundNinlampo(25,Allliguefinal2, frm = OpForm) 
# ligue2223r262 = roundNinlampo(26,ligue2223r252$footieN,frm = OpForm) 
# ligue2223r272 = roundNinlampo(27,ligue2223r262$footieN,frm = OpForm) 
# ligue2223r282 = roundNinlampo(28,ligue2223r272$footieN,frm = OpForm) 
# ligue2223r292 = roundNinlampo(29,ligue2223r282$footieN,frm = OpForm) 
# ligue2223r302 = roundNinlampo(30,ligue2223r292$footieN,frm = OpForm) 
# ligue2223r312 = roundNinlampo(31,ligue2223r302$footieN,frm = OpForm) 
# ligue2223r322 = roundNinlampo(32,ligue2223r312$footieN,frm = OpForm) 
# ligue2223r332 = roundNinlampo(33,ligue2223r322$footieN,frm = OpForm) 
# ligue2223r342 = roundNinlampo(34,ligue2223r332$footieN,frm = OpForm) 
# ligue2223r352 = roundNinlampo(35,ligue2223r342$footieN,frm = OpForm) 
# ligue2223r362 = roundNinlampo(36,ligue2223r352$footieN,frm = OpForm) 
# ligue2223r372 = roundNinlampo(37,ligue2223r362$footieN,frm = OpForm) 
# ligue2223r382 = roundNinlampo(38,ligue2223r372$footieN,frm = OpForm) 


# ligue2223finaltab2 = ligue2223r382$footieN %>%
#   filter(season_id == 4) %>%
#   group_by(Team) %>%
#   summarize(total_points = sum(points_won)) %>%
#   arrange(desc(total_points))
# 
# ligue2223finaltab2
# saveRDS(ligue2223finaltab2,"ligue2223TabWithoutSuperLeaguers")
