source("FootieMaker.R")
source("UtilityFunctions.R")
#### combining all 4 seasons ####

prem2223 = presched("Prem2223.xlsx")
prem2122 = presched("Prem2122.xlsx")
prem2021 = presched("Prem2021.xlsx")
prem1920 = presched("Prem1920.xlsx")

prem_combined <- rbind(prem1920, prem2021, prem2122, prem2223)

lvhtable(prem_combined)
nrow(lvhtable(prem_combined))
VenueHome(prem_combined)

premallsched<- as_tibble(prem_combined) %>% 
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
      Location=="Anfield"~"Liverpool", 
      Location=="Bramall Lane"~"Sheffield Utd",
      Location=="Brentford Community Stadium"~"Brentford", 
      Location=="Carrow Road"~"Norwich City",
      Location=="Craven Cottage"~"Fulham", 
      Location=="Elland Road"~"Leeds United",
      Location=="Emirates Stadium"~"Arsenal", 
      Location=="Etihad Stadium"~"Manchester City", 
      Location=="Goodison Park"~"Everton",
      Location=="King Power Stadium"~"Leicester City",
      Location=="London Stadium"~"West Ham", 
      Location=="Molineux Stadium"~"Wolves",
      Location=="Old Trafford"~"Manchester Utd",
      Location=="Selhurst Park"~"Crystal Palace", 
      Location=="St James' Park"~"Newcastle Utd", 
      Location=="St. James' Park"~"Newcastle Utd", 
      Location=="St. Mary's Stadium"~"Southampton",
      Location=="Stamford Bridge"~"Chelsea", 
      Location=="The American Express Community Stadium"~"Brighton",
      Location=="The City Ground"~"Nott'ham Forest", 
      Location=="The Hawthorns"~"West Brom", 
      Location=="Tottenham Hotspur Stadium"~"Tottenham", 
      Location=="Turf Moor"~"Burnley", 
      Location=="Vicarage Road Stadium"~"Watford", 
      Location=="Villa Park"~"Aston Villa", 
      Location=="Vitality Stadium"~"Bournemouth"
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
      date <= as.Date("2020-07-26") ~ 1,
      date >= as.Date("2020-07-27") & date <= as.Date("2021-05-23") ~ 2,
      date >= as.Date("2021-05-24") & date <= as.Date("2022-05-22") ~ 3,
      date >= as.Date("2022-05-23") ~ 4
    ))

Allprem = MultiSznFootieMaker(premallsched)

#### prem data starts at round 25
AllpremData = datprep(Allprem,25)

#### testing out allprem inla #####

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

Mallprem=inla(formula = eqB,
                data=AllpremData,
                family="poisson",
                control.predictor=list(compute=TRUE,link=1),
                control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
Mallpremsum = summary(Mallprem)
saveRDS(Mallpremsum,"Mallpremsum.rds")
# Watanabe-Akaike information criterion (WAIC) ...: 6788.99


# defence effect for all teams for all seasons
allpremdef = t5team_strength(Mallprem,Allprem,"defense") + ylab("Teams")
# defence effect if we are only using single prem season.
premdef2223onyl = t5team_strength(mprem,plfootie,"defense")   + ylab("Teams")
allvssingledefprem = grid.arrange(premdef2223onyl,allpremdef,ncol=2) 
ggsave("allvssingledefprem.png",allvssingledefprem,width = 8, height = 4, dpi = 300)



ggsave("allpremdef.png",allpremdef,width = 8, height = 4, dpi = 300)

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
      data = AllpremData,
      family = "poisson",
      control.predictor = list(compute = TRUE, link = 1),
      control.compute = list(config = TRUE, dic = TRUE, waic = TRUE)
    )
    waic_values[[paste(current_formula)]] <- current_model$waic$waic
  }
  
  # Save the results for the current chunk
  saveRDS(waic_values, paste0("premwaic_values_chunk_", chunk, ".rds"))
  
  # Clear the waic_values list for the next chunk
  waic_values <- list()
}

# Combine the results from all chunks
waic_values_combined <- list()
for (chunk in 1:num_chunks) {
  waic_values_combined <- c(waic_values_combined, readRDS(paste0("premwaic_values_chunk_", chunk, ".rds")))
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

saveRDS(top_10_models,"allprem_top_10_models.rds")







##### Set seed ######
set.seed(12345)

##### introducing baseline and optimal formula based on waic value list #####

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
prem2122val = presched("prem2122trainingse.xlsx")
prem_combined_val2122 <- rbind(prem1920, prem2021, prem2122val)

lvhtable(prem_combined)
nrow(lvhtable(prem_combined))
VenueHome(prem_combined)

premallsched <- as_tibble(prem_combined_val2122) %>% 
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
      Location=="Anfield"~"Liverpool", 
      Location=="Bramall Lane"~"Sheffield Utd",
      Location=="Brentford Community Stadium"~"Brentford", 
      Location=="Carrow Road"~"Norwich City",
      Location=="Craven Cottage"~"Fulham", 
      Location=="Elland Road"~"Leeds United",
      Location=="Emirates Stadium"~"Arsenal", 
      Location=="Etihad Stadium"~"Manchester City", 
      Location=="Goodison Park"~"Everton",
      Location=="King Power Stadium"~"Leicester City",
      Location=="London Stadium"~"West Ham", 
      Location=="Molineux Stadium"~"Wolves",
      Location=="Old Trafford"~"Manchester Utd",
      Location=="Selhurst Park"~"Crystal Palace", 
      Location=="St James' Park"~"Newcastle Utd", 
      Location=="St. James' Park"~"Newcastle Utd", 
      Location=="St. Mary's Stadium"~"Southampton",
      Location=="Stamford Bridge"~"Chelsea", 
      Location=="The American Express Community Stadium"~"Brighton",
      Location=="The City Ground"~"Nott'ham Forest", 
      Location=="The Hawthorns"~"West Brom", 
      Location=="Tottenham Hotspur Stadium"~"Tottenham", 
      Location=="Turf Moor"~"Burnley", 
      Location=="Vicarage Road Stadium"~"Watford", 
      Location=="Villa Park"~"Aston Villa", 
      Location=="Vitality Stadium"~"Bournemouth"
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
    Round.Number = ifelse(date <= as.Date("2022-02-10"), NA, Round.Number),
    season_id = case_when(
      date <= as.Date("2020-07-19") ~ 1,
      date >= as.Date("2020-07-20") & date <= as.Date("2021-05-23") ~ 2,
      date >= as.Date("2021-05-24") & date <= as.Date("2022-05-22") ~ 3
    ))





##### Allpremupto21/22 op vs baseline round by round ####

Allprem2122_val = MultiSznFootieMaker(premallsched)


bprem2122r25 = roundNinlampo(25,Allprem2122_val, frm = BaselineForm) 
bprem2122r26 = roundNinlampo(26,bprem2122r25$footieN,frm = BaselineForm) 
bprem2122r27 = roundNinlampo(27,bprem2122r26$footieN,frm = BaselineForm) 
bprem2122r28 = roundNinlampo(28,bprem2122r27$footieN,frm = BaselineForm) 
bprem2122r29 = roundNinlampo(29,bprem2122r28$footieN,frm = BaselineForm) 
bprem2122r30 = roundNinlampo(30,bprem2122r29$footieN,frm = BaselineForm) 
bprem2122r31 = roundNinlampo(31,bprem2122r30$footieN,frm = BaselineForm) 
bprem2122r32 = roundNinlampo(32,bprem2122r31$footieN,frm = BaselineForm) 
bprem2122r33 = roundNinlampo(33,bprem2122r32$footieN,frm = BaselineForm) 
bprem2122r34 = roundNinlampo(34,bprem2122r33$footieN,frm = BaselineForm) 
bprem2122r35 = roundNinlampo(35,bprem2122r34$footieN,frm = BaselineForm) 
bprem2122r36 = roundNinlampo(36,bprem2122r35$footieN,frm = BaselineForm) 
bprem2122r37 = roundNinlampo(37,bprem2122r36$footieN,frm = BaselineForm) 
bprem2122r38 = roundNinlampo(38,bprem2122r37$footieN,frm = BaselineForm) 


prem2122finaltablebaseline = bprem2122r38$footieN %>%
  filter(season_id == 3) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points))

prem2122finaltablebaseline

saveRDS(prem2122finaltablebaseline, "allprem2122valfinaltablebaseline.rds" )


### using optimal formula


prem2122footie2 = Allprem2122_val

prem2122r252 = roundNinlampo(25,prem2122footie2,frm=OpForm) 
prem2122r262 = roundNinlampo(26,prem2122r252$footieN,frm=OpForm) 
prem2122r272 = roundNinlampo(27,prem2122r262$footieN,frm=OpForm) 
prem2122r282 = roundNinlampo(28,prem2122r272$footieN,frm=OpForm) 
prem2122r292 = roundNinlampo(29,prem2122r282$footieN,frm=OpForm) 
prem2122r302 = roundNinlampo(30,prem2122r292$footieN,frm=OpForm) 
prem2122r312 = roundNinlampo(31,prem2122r302$footieN,frm=OpForm) 
prem2122r322 = roundNinlampo(32,prem2122r312$footieN,frm=OpForm) 
prem2122r332 = roundNinlampo(33,prem2122r322$footieN,frm=OpForm) 
prem2122r342 = roundNinlampo(34,prem2122r332$footieN,frm=OpForm) 
prem2122r352 = roundNinlampo(35,prem2122r342$footieN,frm=OpForm) 
prem2122r362 = roundNinlampo(36,prem2122r352$footieN,frm=OpForm) 
prem2122r372 = roundNinlampo(37,prem2122r362$footieN,frm=OpForm) 
prem2122r382 = roundNinlampo(38,prem2122r372$footieN,frm=OpForm) 

prem2122finaltableOPFORM = prem2122r382$footieN %>%
  filter(season_id == 3) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points))

prem2122finaltableOPFORM

saveRDS(prem2122finaltableOPFORM, "prem2122AfinaltableOPFORM.rds" )

###### prem Mod Val Results ####
######  reading table observed output

bl2122tab = read_excel("prem2122tab.xlsx") %>%
  as.tibble() %>%
  mutate(total_points = Pts,
         Team = Squad) %>%
  select(Team, total_points) %>%
  arrange(Team)

bl2122OPFORM = prem2122finaltableOPFORM %>%
  select(Team, total_points) %>%
  arrange(Team)

bl2122baseline = prem2122finaltablebaseline %>%
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
saveRDS(comptab, "premcomptabbl2122")

# Calculate the sum of squared differences for the baseline model
sum_sq_diff_baseline <- sum(bl2122baseline$Points_Difference ^ 2)

# Calculate the sum of squared differences for the optimized model
sum_sq_diff_optimized <- sum(bl2122OPFORM$Points_Difference ^ 2)

# Print the results
cat("Mean squared error for the baseline model:", sum_sq_diff_baseline/20, "\n")
cat("Mean squared error for the optimized model:", sum_sq_diff_optimized/20, "\n")

#### Final Predictions for league with teams joining super league #####

OpForm = Goal ~ Home + 
  form +
  Gpg +
  GCpg +
  GDdiff +
  f(factor(Team), model = "iid") +
  f(factor(Opponent), model = "iid")  

Allpremfinal = Allprem

Mallprem=inla(formula = OpForm,
              data=Allpremfinal,
              family="poisson",
              control.predictor=list(compute=TRUE,link=1),
              control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
Mallpremsum = summary(Mallprem)
saveRDS(Mallpremsum,"MallpremsumFinal.rds")

prem2223r25 = roundNinlampo(25,Allpremfinal, frm = OpForm) 
prem2223r26 = roundNinlampo(26,prem2223r25$footieN,frm = OpForm) 
prem2223r27 = roundNinlampo(27,prem2223r26$footieN,frm = OpForm) 
prem2223r28 = roundNinlampo(28,prem2223r27$footieN,frm = OpForm) 
prem2223r29 = roundNinlampo(29,prem2223r28$footieN,frm = OpForm) 
prem2223r30 = roundNinlampo(30,prem2223r29$footieN,frm = OpForm) 
prem2223r31 = roundNinlampo(31,prem2223r30$footieN,frm = OpForm) 
prem2223r32 = roundNinlampo(32,prem2223r31$footieN,frm = OpForm) 
prem2223r33 = roundNinlampo(33,prem2223r32$footieN,frm = OpForm) 
prem2223r34 = roundNinlampo(34,prem2223r33$footieN,frm = OpForm) 
prem2223r35 = roundNinlampo(35,prem2223r34$footieN,frm = OpForm) 
prem2223r36 = roundNinlampo(36,prem2223r35$footieN,frm = OpForm) 
prem2223r37 = roundNinlampo(37,prem2223r36$footieN,frm = OpForm) 
prem2223r38 = roundNinlampo(38,prem2223r37$footieN,frm = OpForm) 


prem2223finaltab = prem2223r38$footieN %>%
  filter(season_id == 4) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points))

prem2223finaltab
saveRDS(prem2223finaltab,"Prem2223TabWithSuperLeaguers")



#### Final Predictions for league WITHUOT teams joining super league #####

OpForm = Goal ~ Home + 
  form +
  Gpg +
  GCpg +
  GDdiff +
  f(factor(Team), model = "iid") +
  f(factor(Opponent), model = "iid")  

teams_to_remove_prem <- c("Arsenal", "Chelsea", "Liverpool", "Manchester City", "Manchester Utd", "Tottenham")

Allpremfinal2 <- prem2223r38$footieN  %>%
  filter(!(Team %in% teams_to_remove_prem | Opponent %in% teams_to_remove_prem))

prem2223finaltab2 = Allpremfinal2 %>%
  filter(season_id == 4) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points))

prem2223finaltab2
saveRDS(prem2223finaltab2,"Prem2223TabWithoutSuperLeaguers")

# Mallprem2=inla(formula = OpForm,
#               data=Allpremfinal2,
#               family="poisson",
#               control.predictor=list(compute=TRUE,link=1),
#               control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
# Mallpremsum2 = summary(Mallprem2)
# saveRDS(Mallpremsum2,"MallpremsumFinal2.rds")
# prem2223r252$inlam
# prem2223r252 = roundNinlampo(25,Allpremfinal2, frm = OpForm) 
# prem2223r262 = roundNinlampo(26,prem2223r252$footieN,frm = OpForm) 
# prem2223r272 = roundNinlampo(27,prem2223r262$footieN,frm = OpForm) 
# prem2223r282 = roundNinlampo(28,prem2223r272$footieN,frm = OpForm) 
# prem2223r292 = roundNinlampo(29,prem2223r282$footieN,frm = OpForm) 
# prem2223r302 = roundNinlampo(30,prem2223r292$footieN,frm = OpForm) 
# prem2223r312 = roundNinlampo(31,prem2223r302$footieN,frm = OpForm) 
# prem2223r322 = roundNinlampo(32,prem2223r312$footieN,frm = OpForm) 
# prem2223r332 = roundNinlampo(33,prem2223r322$footieN,frm = OpForm) 
# prem2223r342 = roundNinlampo(34,prem2223r332$footieN,frm = OpForm) 
# prem2223r352 = roundNinlampo(35,prem2223r342$footieN,frm = OpForm) 
# prem2223r362 = roundNinlampo(36,prem2223r352$footieN,frm = OpForm) 
# prem2223r372 = roundNinlampo(37,prem2223r362$footieN,frm = OpForm) 
# prem2223r382 = roundNinlampo(38,prem2223r372$footieN,frm = OpForm) 


prem2223finaltab2 = prem2223r382$footieN %>%
  filter(season_id == 4) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points))

prem2223finaltab2
saveRDS(prem2223finaltab2,"Prem2223TabWithoutSuperLeaguers")


# "Arsenal",
# "Chelsea",
# "Liverpool",
# "Manchester City",
# "Manchester Utd",
# "Tottenham", 
# "Milan", 
# "Inter" ,
# "Juventus", 
# "Atletico Madrid", 
# "Barcelona", 
# "Real Madrid",
# "Bayern Munich",
# "Paris S-G",