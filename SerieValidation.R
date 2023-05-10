source("FootieMaker.R")
source("UtilityFunctions.R")
#### combining all 4 seasons ####

serie2223a = presched("seriea2223raw.xlsx") 
serie2223 = rename(serie2223a, "xG...6" = "xG...5", "xG...8" = "xG...7")
serie2122 = presched("serie2122.xlsx")
serie2021 = presched("serie2021.xlsx")
serie1920 = presched("serie1920.xlsx")

serie_combined <- rbind(serie1920, serie2021, serie2122, serie2223)

lvhtable(serie_combined)
nrow(lvhtable(serie_combined))
VenueHome(serie_combined)

serieallsched<- as_tibble(serie_combined) %>% 
  rename(
    Location = Venue
  ) %>% 
  mutate(
    Date=as.Date(Date, format = "%d/%m/%Y"),
    tournament="serieier League",
    HG = as.numeric(HG),
    AG = as.numeric(AG),
    ID_game=row_number(),
    Location=case_when(
      Location=="Allianz Stadium"~"Juventus",
      Location=="Dacia Arena"~"Udinese", 
      Location=="Gewiss Stadium"~"Atalanta",
      Location=="Mapei Stadium - Città del Tricolore"~"Sassuolo",
      Location=="Sardegna Arena"~"Cagliari", 
      Location=="Stadio Alberto Picco"~"Spezia", 
      Location=="Stadio Arechi"~"Salernitana", 
      Location=="Stadio Artemio Franchi"~"Fiorentina", 
      Location=="Stadio Atleti Azzurri d'Italia"~"Atalanta", 
      Location=="Stadio Carlo Castellani"~"Empoli", 
      Location=="Stadio Ciro Vigorito"~"Benevento",
      Location=="Stadio Comunale Luigi Ferraris" & Home == "Genoa"~"Genoa", 
      Location=="Stadio Comunale Luigi Ferraris" & Home == "Sampdoria"~"Sampdoria", 
      Location=="Stadio Comunale Via Del Mare"~"Lecce", 
      Location=="Stadio Diego Armando Maradona"~"Napoli", 
      Location=="Stadio Dino Manuzzi"~"Spezia", 
      Location=="Stadio Ennio Tardini" & Home == "Atalanta"~"Atalanta", 
      Location=="Stadio Ennio Tardini" & Home == "Parma"~"Parma", 
      Location=="Stadio Ezio Scida"~"Crotone",
      Location=="Stadio Giovanni Zini"~"Cremonese",
      Location=="Stadio Giuseppe Meazza" & Home == "Inter" ~"Inter",
      Location=="Stadio Giuseppe Meazza" & Home == "Milan"~"Milan", 
      Location=="Stadio Marc'Antonio Bentegodi"~"Hellas Verona", 
      Location=="Stadio Mario Rigamonti"~"Brescia",
      Location=="Stadio Olimpico" & Home == "Lazio"~"Lazio", 
      Location=="Stadio Olimpico" & Home == "Roma"~"Roma", 
      Location=="Stadio Olimpico di Torino"~"Torino", 
      Location=="Stadio Paolo Mazza"~"SPAL", 
      Location=="Stadio Pierluigi Penzo"~"Venezia", 
      Location=="Stadio Renato Dall'Ara"~"Bologna", 
      Location=="Stadio San Paolo"~"Napoli", 
      Location=="U-Power Stadium"~"Monza", 
      Location=="Unipol Domus"~"Cagliari"
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
    Round.Number = ifelse(date < as.Date("2023-02-21"), NA, Round.Number),
    # Round.Number = case_when(
    #         date <= as.Date("2023-02-23") ~ 1,
    #         date >= as.Date("2022-05-24") ~ 2
    # ),
    season_id = case_when(
      date <= as.Date("2020-08-02") ~ 1,
      date >= as.Date("2020-09-19") & date <= as.Date("2021-05-23") ~ 2,
      date >= as.Date("2021-08-21") & date <= as.Date("2022-05-22") ~ 3,
      date >= as.Date("2022-05-23") ~ 4
    ))

Allserie = MultiSznFootieMaker(serieallsched)

#### serie data starts at round 25
AllserieData = datprep(Allserie,25)

#### testing out allserie inla #####

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

Mallserie=inla(formula = eqB,
              data=AllserieData,
              family="poisson",
              control.predictor=list(compute=TRUE,link=1),
              control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
summary(Mallserie)
# Watanabe-Akaike information criterion (WAIC) ...: 6788.99

allseriedef = t5team_strength(Mallserie,Allserie,"defense") + ylab("Teams")

ggsave("allseriedef.png",allseriedef,width = 8, height = 4, dpi = 300)

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
      data = AllserieData,
      family = "poisson",
      control.predictor = list(compute = TRUE, link = 1),
      control.compute = list(config = TRUE, dic = TRUE, waic = TRUE)
    )
    waic_values[[paste(current_formula)]] <- current_model$waic$waic
  }
  
  # Save the results for the current chunk
  saveRDS(waic_values, paste0("seriewaic_values_chunk_", chunk, ".rds"))
  
  # Clear the waic_values list for the next chunk
  waic_values <- list()
}

# Combine the results from all chunks
waic_values_combined <- list()
for (chunk in 1:num_chunks) {
  waic_values_combined <- c(waic_values_combined, readRDS(paste0("seriewaic_values_chunk_", chunk, ".rds")))
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

saveRDS(top_10_models,"allserie_top_10_models.rds")







##### Set seed ######
set.seed(12345)

#####  baseline and optimal formula based on waic value list #####

BaselineForm =  Goal ~ Home + 
  f(factor(Team), model = "iid") +
  f(factor(Opponent), model = "iid")  

OpForm = Goal ~ Home + 
  form +
  Gpg +
  GCpg +
  GDdiff +
  f(factor(Team), model = "iid") +
  f(factor(Opponent), model = "iid")  




#### this time we are using 3 seasons up to halfway the 21/22 season point####
serie2122val = presched("serie2122trainingse.xlsx")
serie_combined_val2122 <- rbind(serie1920, serie2021, serie2122val)

lvhtable(serie_combined)
nrow(lvhtable(serie_combined))
VenueHome(serie_combined)

serieallsched2 <- as_tibble(serie_combined_val2122) %>% 
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
      Location=="Allianz Stadium"~"Juventus",
      Location=="Dacia Arena"~"Udinese", 
      Location=="Gewiss Stadium"~"Atalanta",
      Location=="Mapei Stadium - Città del Tricolore"~"Sassuolo",
      Location=="Sardegna Arena"~"Cagliari", 
      Location=="Stadio Alberto Picco"~"Spezia", 
      Location=="Stadio Arechi"~"Salernitana", 
      Location=="Stadio Artemio Franchi"~"Fiorentina", 
      Location=="Stadio Atleti Azzurri d'Italia"~"Atalanta", 
      Location=="Stadio Carlo Castellani"~"Empoli", 
      Location=="Stadio Ciro Vigorito"~"Benevento",
      Location=="Stadio Comunale Luigi Ferraris" & Home == "Genoa"~"Genoa", 
      Location=="Stadio Comunale Luigi Ferraris" & Home == "Sampdoria"~"Sampdoria", 
      Location=="Stadio Comunale Via Del Mare"~"Lecce", 
      Location=="Stadio Diego Armando Maradona"~"Napoli", 
      Location=="Stadio Dino Manuzzi"~"Spezia", 
      Location=="Stadio Ennio Tardini" & Home == "Atalanta"~"Atalanta", 
      Location=="Stadio Ennio Tardini" & Home == "Parma"~"Parma", 
      Location=="Stadio Ezio Scida"~"Crotone",
      Location=="Stadio Giovanni Zini"~"Cremonese",
      Location=="Stadio Giuseppe Meazza" & Home == "Inter" ~"Inter",
      Location=="Stadio Giuseppe Meazza" & Home == "Milan"~"Milan", 
      Location=="Stadio Marc'Antonio Bentegodi"~"Hellas Verona", 
      Location=="Stadio Mario Rigamonti"~"Brescia",
      Location=="Stadio Olimpico" & Home == "Lazio"~"Lazio", 
      Location=="Stadio Olimpico" & Home == "Roma"~"Roma", 
      Location=="Stadio Olimpico di Torino"~"Torino", 
      Location=="Stadio Paolo Mazza"~"SPAL", 
      Location=="Stadio Pierluigi Penzo"~"Venezia", 
      Location=="Stadio Renato Dall'Ara"~"Bologna", 
      Location=="Stadio San Paolo"~"Napoli", 
      Location=="U-Power Stadium"~"Monza", 
      Location=="Unipol Domus"~"Cagliari"
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
    Round.Number = ifelse(date < as.Date("2022-02-05"), NA, Round.Number), # this makes all the matches before the unplayed fixtures have round number = NA (since we arent predicting them)
    season_id = case_when(
      date <= as.Date("2020-08-02") ~ 1,
      date >= as.Date("2020-09-19") & date <= as.Date("2021-05-23") ~ 2,
      date >= as.Date("2021-08-21") ~ 3
    ))



### vlidation data starts simulating at round 24 in 21/22 season
##### Allserieupto21/22 op vs baseline round by round ####

Allserie2122_val = MultiSznFootieMaker(serieallsched2)


bserie2122r24 = roundNinlampo(24,Allserie2122_val, frm = BaselineForm) 
bserie2122r25 = roundNinlampo(25,bserie2122r24$footieN,frm = BaselineForm) 
bserie2122r26 = roundNinlampo(26,bserie2122r25$footieN,frm = BaselineForm) 
bserie2122r27 = roundNinlampo(27,bserie2122r26$footieN,frm = BaselineForm) 
bserie2122r28 = roundNinlampo(28,bserie2122r27$footieN,frm = BaselineForm) 
bserie2122r29 = roundNinlampo(29,bserie2122r28$footieN,frm = BaselineForm) 
bserie2122r30 = roundNinlampo(30,bserie2122r29$footieN,frm = BaselineForm) 
bserie2122r31 = roundNinlampo(31,bserie2122r30$footieN,frm = BaselineForm) 
bserie2122r32 = roundNinlampo(32,bserie2122r31$footieN,frm = BaselineForm) 
bserie2122r33 = roundNinlampo(33,bserie2122r32$footieN,frm = BaselineForm) 
bserie2122r34 = roundNinlampo(34,bserie2122r33$footieN,frm = BaselineForm) 
bserie2122r35 = roundNinlampo(35,bserie2122r34$footieN,frm = BaselineForm) 
bserie2122r36 = roundNinlampo(36,bserie2122r35$footieN,frm = BaselineForm) 
bserie2122r37 = roundNinlampo(37,bserie2122r36$footieN,frm = BaselineForm) 
bserie2122r38 = roundNinlampo(38,bserie2122r37$footieN,frm = BaselineForm) 


serie2122finaltablebaseline = bserie2122r38$footieN %>%
  filter(season_id == 3) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points))

serie2122finaltablebaseline

saveRDS(serie2122finaltablebaseline, "allserie2122valfinaltablebaseline.rds" )


### using optimal formula


serie2122footie2 = Allserie2122_val

serie2122r242 = roundNinlampo(24,serie2122footie2, frm = OpForm) 
serie2122r252 = roundNinlampo(25,serie2122r242$footieN,frm = OpForm) 
serie2122r262 = roundNinlampo(26,serie2122r252$footieN,frm=OpForm) 
serie2122r272 = roundNinlampo(27,serie2122r262$footieN,frm=OpForm) 
serie2122r282 = roundNinlampo(28,serie2122r272$footieN,frm=OpForm) 
serie2122r292 = roundNinlampo(29,serie2122r282$footieN,frm=OpForm) 
serie2122r302 = roundNinlampo(30,serie2122r292$footieN,frm=OpForm) 
serie2122r312 = roundNinlampo(31,serie2122r302$footieN,frm=OpForm) 
serie2122r322 = roundNinlampo(32,serie2122r312$footieN,frm=OpForm) 
serie2122r332 = roundNinlampo(33,serie2122r322$footieN,frm=OpForm) 
serie2122r342 = roundNinlampo(34,serie2122r332$footieN,frm=OpForm) 
serie2122r352 = roundNinlampo(35,serie2122r342$footieN,frm=OpForm) 
serie2122r362 = roundNinlampo(36,serie2122r352$footieN,frm=OpForm) 
serie2122r372 = roundNinlampo(37,serie2122r362$footieN,frm=OpForm) 
serie2122r382 = roundNinlampo(38,serie2122r372$footieN,frm=OpForm) 

serie2122finaltableOPFORM = serie2122r382$footieN %>%
  filter(season_id == 3) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points))

serie2122finaltableOPFORM

saveRDS(serie2122finaltableOPFORM, "serie2122AfinaltableOPFORM.rds" )

###### serie Mod Val Results ####
######  reading table observed output

bl2122tab = read_excel("serie2122tab.xlsx") %>%
  as.tibble() %>%
  mutate(total_points = Pts,
         Team = Squad) %>%
  select(Team, total_points) %>%
  arrange(Team)

bl2122OPFORM = serie2122finaltableOPFORM %>%
  select(Team, total_points) %>%
  arrange(Team)

bl2122baseline = serie2122finaltablebaseline %>%
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
saveRDS(comptab, "seriecomptabbl2122")

# Calculate the sum of squared differences for the baseline model
sum_sq_diff_baseline <- sum(bl2122baseline$Points_Difference ^ 2)

# Calculate the sum of squared differences for the optimized model
sum_sq_diff_optimized <- sum(bl2122OPFORM$Points_Difference ^ 2)

# Print the results
cat("Mean squared error for the baseline model:", sum_sq_diff_baseline/20, "\n")
cat("Mean squared error for the optimized model:", sum_sq_diff_optimized/20, "\n")


#### Final Predictions ####



Allseriefinal = Allserie

Mallserie=inla(formula = OpForm,
              data=Allseriefinal,
              family="poisson",
              control.predictor=list(compute=TRUE,link=1),
              control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
Mallseriesum = summary(Mallserie)
saveRDS(Mallseriesum,"MallseriesumFinal.rds")



serie2223r24 = roundNinlampo(24,Allseriefinal, frm = OpForm) 
serie2223r25 = roundNinlampo(25,serie2223r24$footieN, frm = OpForm) 
serie2223r26 = roundNinlampo(26,serie2223r25$footieN,frm = OpForm) 
serie2223r27 = roundNinlampo(27,serie2223r26$footieN,frm = OpForm) 
serie2223r28 = roundNinlampo(28,serie2223r27$footieN,frm = OpForm) 
serie2223r29 = roundNinlampo(29,serie2223r28$footieN,frm = OpForm) 
serie2223r30 = roundNinlampo(30,serie2223r29$footieN,frm = OpForm) 
serie2223r31 = roundNinlampo(31,serie2223r30$footieN,frm = OpForm) 
serie2223r32 = roundNinlampo(32,serie2223r31$footieN,frm = OpForm) 
serie2223r33 = roundNinlampo(33,serie2223r32$footieN,frm = OpForm) 
serie2223r34 = roundNinlampo(34,serie2223r33$footieN,frm = OpForm) 
serie2223r35 = roundNinlampo(35,serie2223r34$footieN,frm = OpForm) 
serie2223r36 = roundNinlampo(36,serie2223r35$footieN,frm = OpForm) 
serie2223r37 = roundNinlampo(37,serie2223r36$footieN,frm = OpForm) 
serie2223r38 = roundNinlampo(38,serie2223r37$footieN,frm = OpForm) 


serie2223finaltab = serie2223r38$footieN %>%
  filter(season_id == 4) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points)) %>%
  ungroup()

serie2223finaltab
saveRDS(serie2223finaltab,"serie2223TabWithSuperLeaguers")

#### Final Predictions for league WITHUOT teams joining super league #####

OpForm = Goal ~ Home + 
  form +
  Gpg +
  GCpg +
  GDdiff +
  f(factor(Team), model = "iid") +
  f(factor(Opponent), model = "iid")  

teams_to_remove_serie <- c("Milan", "Inter", "Juventus")

Allseriefinal2 <- Allserie %>%
  filter(!(Team %in% teams_to_remove_serie | Opponent %in% teams_to_remove_serie))


Mallserie2=inla(formula = OpForm,
               data=Allseriefinal2,
               family="poisson",
               control.predictor=list(compute=TRUE,link=1),
               control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
Mallseriesum2 = summary(Mallserie2)
saveRDS(Mallseriesum2,"MallseriesumFinal2.rds")

serie2223r242 = roundNinlampo(24,Allseriefinal2, frm = OpForm) 
serie2223r252 = roundNinlampo(25,serie2223r242$footieN, frm = OpForm) 
serie2223r262 = roundNinlampo(26,serie2223r252$footieN,frm = OpForm) 
serie2223r272 = roundNinlampo(27,serie2223r262$footieN,frm = OpForm) 
serie2223r282 = roundNinlampo(28,serie2223r272$footieN,frm = OpForm) 
serie2223r292 = roundNinlampo(29,serie2223r282$footieN,frm = OpForm) 
serie2223r302 = roundNinlampo(30,serie2223r292$footieN,frm = OpForm) 
serie2223r312 = roundNinlampo(31,serie2223r302$footieN,frm = OpForm) 
serie2223r322 = roundNinlampo(32,serie2223r312$footieN,frm = OpForm) 
serie2223r332 = roundNinlampo(33,serie2223r322$footieN,frm = OpForm) 
serie2223r342 = roundNinlampo(34,serie2223r332$footieN,frm = OpForm) 
serie2223r352 = roundNinlampo(35,serie2223r342$footieN,frm = OpForm) 
serie2223r362 = roundNinlampo(36,serie2223r352$footieN,frm = OpForm) 
serie2223r372 = roundNinlampo(37,serie2223r362$footieN,frm = OpForm) 
serie2223r382 = roundNinlampo(38,serie2223r372$footieN,frm = OpForm) 


serie2223finaltab2 = serie2223r382$footieN %>%
  filter(season_id == 4) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points))

serie2223finaltab2
saveRDS(serie2223finaltab2,"serie2223TabWithoutSuperLeaguers")

