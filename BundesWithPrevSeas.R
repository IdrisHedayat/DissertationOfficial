source("FootieMaker.R")
source("UtilityFunctions.R")
 #### combining all 4 seasons ####

buli1920 = presched("buli1920.xlsx") 
buli2021= presched("buli2021.xlsx")
buli2122 = presched("buli2122.xlsx")
buli2223 = presched("bundes2223raw.xlsx")

buli_combined <- rbind(buli1920, buli2021, buli2122, buli2223)

lvhtable(buli_combined)
nrow(lvhtable(buli_combined))
VenueHome(buli_combined)

buliallsched<- as_tibble(buli_combined) %>% 
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
      Location=="Allianz Arena"~"Bayern Munich",
      Location=="BayArena"~"Leverkusen", 
      Location=="Benteler-Arena"~"Paderborn 07", 
      Location=="Deutsche Bank Park"~"Eint Frankfurt", 
      Location=="Europa-Park Stadion"~"Freiburg", 
      Location=="Mercedes-Benz Arena"~"Stuttgart",
      Location=="Merkur Spielarena"~"Düsseldorf", 
      Location=="Mewa Arena"~"Mainz 05",
      Location=="Olympiastadion Berlin"~"Hertha BSC",
      Location=="Opel Arena"~"Mainz 05", 
      Location=="PreZero Arena"~"Hoffenheim", 
      Location=="Red Bull Arena"~"RB Leipzig",
      Location=="RheinEnergieSTADION"~"Köln", 
      Location=="SchücoArena" & Home == "Arminia"~"Arminia",
      Location=="SchücoArena" & Home == "Mainz 05"~"Mainz 05", 
      Location=="Schwarzwald-Stadion"~"Freiburg",
      Location=="Signal Iduna Park"~"Dortmund", 
      Location=="Sportpark Ronhof Thomas Sommer"~"Greuther Fürth",
      Location=="Stadion An der Alten Försterei"~"Union Berlin", 
      Location=="Stadion im Borussia-Park"~"M'Gladbach", 
      Location=="Veltins-Arena"~"Schalke 04", 
      Location=="Volkswagen Arena"~"Wolfsburg", 
      Location=="Vonovia Ruhrstadion"~"Bochum", 
      Location=="Weserstadion"~"Werder Bremen", 
      Location=="Wohninvest-Weserstadion"~"Werder Bremen", 
      Location=="WWK Arena"~"Augsburg"
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
      date <= as.Date("2020-06-27") ~ 1,
      date >= as.Date("2020-06-28") & date <= as.Date("2021-05-22") ~ 2,
      date >= as.Date("2021-05-23") & date <= as.Date("2022-05-14") ~ 3,
      date >= as.Date("2022-05-15") ~ 4
    ))

AllBuli = MultiSznFootieMaker(buliallsched)

#### data starts at round 22
AllBuliData = datprep(AllBuli,22)

#### testing out allbuli inla #####

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

MallBuli=inla(formula = eqB,
                data=AllBuliData,
                family="poisson",
                control.predictor=list(compute=TRUE,link=1),
                control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
summary(MallBuli)
# Watanabe-Akaike information criterion (WAIC) ...: 6788.99

allbulidef = t5team_strength(MallBuli,AllBuli) +ylab("Teams")
ggsave("allbulidef.png", allbulidef, width = 8, height = 4, dpi = 300)

##### comparing MPO vs mean outcomes:


### Model Validation ####

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
      data = AllBuliData,
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
# Renaming the formula column
top_10_models = top_10_models %>%
  mutate(Formula= gsub("f\\(factor\\(Team\\), model = 'iid'\\)", "Att", Formula)) %>%
  mutate(Formula = gsub("f\\(factor\\(Opponent\\), model = 'iid'\\)", "Def", Formula))

saveRDS(top_10_models,"allbuli_top_10_models.rds")
  




##### Model Validation With BULI 21-22 HALFWAY SEASON ONLY TRAINING DATA #######
#### this is only if we are using 21/22 seasons which doesnt make sense because of overfitting
# 
# buli2122val = presched("buli2122trainingse.xlsx")
# 
# buli2122valsched <- as_tibble(buli2122val) %>% 
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
#       Location=="Allianz Arena"~"Bayern Munich", 
#       Location=="BayArena"~"Leverkusen", 
#       Location=="Deutsche Bank Park"~"Eint Frankfurt", 
#       Location=="Europa-Park Stadion"~"Freiburg", 
#       Location=="Mercedes-Benz Arena"~"Stuttgart", 
#       Location=="Mewa Arena"~"Mainz 05", 
#       Location=="Olympiastadion Berlin"~"Hertha BSC",
#       Location=="PreZero Arena"~"Hoffenheim", 
#       Location=="Red Bull Arena"~"RB Leipzig",
#       Location=="RheinEnergieSTADION"~"Köln", 
#       Location=="SchücoArena"~"Arminia",
#       Location=="Schwarzwald-Stadion"~"Freiburg", 
#       Location=="Signal Iduna Park"~"Dortmund", 
#       Location=="Sportpark Ronhof Thomas Sommer"~"Greuther Fürth", 
#       Location=="Stadion An der Alten Försterei"~"Union Berlin",
#       Location=="Stadion im Borussia-Park"~"M'Gladbach",
#       Location=="Volkswagen Arena"~"Wolfsburg", 
#       Location=="Vonovia Ruhrstadion"~"Bochum",
#       Location=="WWK Arena"~"Augsburg")
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

#####  baseline and optimal formula based on waic value list #####

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


# buli2122footie = FootieMaker(buli2122valsched)
# buli2122dat = datprep(buli2122footie,23)
# 
# 
# mbaselinebulival = inla(formula = BaselineForm,
#                       data=buli2122dat,
#                       family="poisson",
#                       control.predictor=list(compute=TRUE,link=1),
#                       control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
# summary(mbaselinebulival)
# 
# 
# bbuli2122footie23 = roundNinlampo(23,buli2122footie,frm = BaselineForm)
# bbuli2122r24 = roundNinlampo(24,bbuli2122footie23$footieN,frm = BaselineForm)
# bbuli2122r25 = roundNinlampo(25,bbuli2122r24$footieN,frm = BaselineForm) 
# bbuli2122r26 = roundNinlampo(26,bbuli2122r25$footieN,frm = BaselineForm) 
# bbuli2122r27 = roundNinlampo(27,bbuli2122r26$footieN,frm = BaselineForm) 
# bbuli2122r28 = roundNinlampo(28,bbuli2122r27$footieN,frm = BaselineForm) 
# bbuli2122r29 = roundNinlampo(29,bbuli2122r28$footieN,frm = BaselineForm) 
# bbuli2122r30 = roundNinlampo(30,bbuli2122r29$footieN,frm = BaselineForm) 
# bbuli2122r31 = roundNinlampo(31,bbuli2122r30$footieN,frm = BaselineForm) 
# bbuli2122r32 = roundNinlampo(32,bbuli2122r31$footieN,frm = BaselineForm) 
# bbuli2122r33 = roundNinlampo(33,bbuli2122r32$footieN,frm = BaselineForm) 
# bbuli2122r34 = roundNinlampo(34,bbuli2122r33$footieN,frm = BaselineForm) 
# 
# buli2122finaltablebaseline = t5tablerounrR(34,bbuli2122r34$footieN)
# buli2122finaltablebaseline
# saveRDS(buli2122finaltablebaseline, "allbuli2122valfinaltablebaseline.rds" )
# 
# 
# ### using optimal formula
# 
# 
# mopformbulival = inla(formula = OpForm,
#                       data=buli2122dat,
#                       family="poisson",
#                       control.predictor=list(compute=TRUE,link=1),
#                       control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
# 
# summary(mopformbulival)
# 
# 
# buli2122footie2 = buli2122footie
# buli2122footie232 = roundNinlampo(23,buli2122footie2,frm=OpForm)
# buli2122r242 = roundNinlampo(24,buli2122footie232$footieN,frm=OpForm)
# buli2122r252 = roundNinlampo(25,buli2122r242$footieN,frm=OpForm) 
# buli2122r262 = roundNinlampo(26,buli2122r252$footieN,frm=OpForm) 
# buli2122r272 = roundNinlampo(27,buli2122r262$footieN,frm=OpForm) 
# buli2122r282 = roundNinlampo(28,buli2122r272$footieN,frm=OpForm) 
# buli2122r292 = roundNinlampo(29,buli2122r282$footieN,frm=OpForm) 
# buli2122r302 = roundNinlampo(30,buli2122r292$footieN,frm=OpForm) 
# buli2122r312 = roundNinlampo(31,buli2122r302$footieN,frm=OpForm) 
# buli2122r322 = roundNinlampo(32,buli2122r312$footieN,frm=OpForm) 
# buli2122r332 = roundNinlampo(33,buli2122r322$footieN,frm=OpForm) 
# buli2122r342 = roundNinlampo(34,buli2122r332$footieN,frm=OpForm) 
# 
# buli2122finaltableOPFORM = t5tablerounrR(34,buli2122r342$footieN)
# buli2122finaltableOPFORM
# saveRDS(buli2122finaltableOPFORM, "buli2122AfinaltableOPFORM.rds" )
# 
#     ### Buli Mod Val Results ###
# ######  reading table observed output
# 
# bl2122tab = read_excel("buli2122tab.xlsx") %>%
#   as.tibble() %>%
#   mutate(total_points = Pts,
#          Team = Squad) %>%
#   select(Team, total_points) %>%
#   arrange(Team)
# 
# bl2122OPFORM = buli2122finaltableOPFORM %>%
#   select(Team, total_points) %>%
#   arrange(Team)
# 
# bl2122baseline = buli2122finaltablebaseline %>%
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
buli2122val = presched("buli2122trainingse.xlsx")
buli_combined_val2122 <- rbind(buli1920, buli2021, buli2122val)

lvhtable(buli_combined)
nrow(lvhtable(buli_combined))
VenueHome(buli_combined)

buliallsched <- as_tibble(buli_combined_val2122) %>% 
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
      Location=="Allianz Arena"~"Bayern Munich",
      Location=="BayArena"~"Leverkusen", 
      Location=="Benteler-Arena"~"Paderborn 07", 
      Location=="Deutsche Bank Park"~"Eint Frankfurt", 
      Location=="Europa-Park Stadion"~"Freiburg", 
      Location=="Mercedes-Benz Arena"~"Stuttgart",
      Location=="Merkur Spielarena"~"Düsseldorf", 
      Location=="Mewa Arena"~"Mainz 05",
      Location=="Olympiastadion Berlin"~"Hertha BSC",
      Location=="Opel Arena"~"Mainz 05", 
      Location=="PreZero Arena"~"Hoffenheim", 
      Location=="Red Bull Arena"~"RB Leipzig",
      Location=="RheinEnergieSTADION"~"Köln", 
      Location=="SchücoArena" & Home == "Arminia"~"Arminia",
      Location=="SchücoArena" & Home == "Mainz 05"~"Mainz 05", 
      Location=="Schwarzwald-Stadion"~"Freiburg",
      Location=="Signal Iduna Park"~"Dortmund", 
      Location=="Sportpark Ronhof Thomas Sommer"~"Greuther Fürth",
      Location=="Stadion An der Alten Försterei"~"Union Berlin", 
      Location=="Stadion im Borussia-Park"~"M'Gladbach", 
      Location=="Veltins-Arena"~"Schalke 04", 
      Location=="Volkswagen Arena"~"Wolfsburg", 
      Location=="Vonovia Ruhrstadion"~"Bochum", 
      Location=="Weserstadion"~"Werder Bremen", 
      Location=="Wohninvest-Weserstadion"~"Werder Bremen", 
      Location=="WWK Arena"~"Augsburg"
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
    Round.Number = ifelse(date < as.Date("2022-02-18"), NA, Round.Number),
    # Round.Number = case_when(
    #         date <= as.Date("2023-02-23") ~ 1,
    #         date >= as.Date("2022-05-24") ~ 2
    # ),
    season_id = case_when(
      date <= as.Date("2020-06-27") ~ 1,
      date >= as.Date("2020-06-28") & date <= as.Date("2021-05-22") ~ 2,
      date >= as.Date("2021-05-23") & date <= as.Date("2022-05-14") ~ 3
    ))


### below is the code used in validation if we only use 1 round in stead of round by round simulating.

### this is the footie dataset we will be using as validation (predicting 21/22 then comparing with the observed actual results)

#### if sim 1 round instead of doing round by round ?####
# AllBuli2122_val = MultiSznFootieMaker(buliallsched) %>%
#   mutate(
#     Round.Number = ifelse(date < as.Date("2022-02-18"), NA, Round.Number)) %>%
#   mutate(
#     Round.Number = case_when(date >= as.Date("2022-02-18") ~ 1)
#   )
# buli2122dat = datprep(AllBuli2122_val,1)
# mbaselinebulival = inla(formula = BaselineForm,
#                         data=buli2122dat,
#                         family="poisson",
#                         control.predictor=list(compute=TRUE,link=1),
#                         control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
# summary(mbaselinebulival)
# bbuli2122footie23 = roundNinlampo(1,AllBuli2122_val,frm = BaselineForm)
# 
# total_points_2122_val_baseline  = bbuli2122footie23$footieN %>%
#   filter(season_id == 3) %>%
#   group_by(Team) %>%
#   summarize(total_points = sum(points_won)) %>%
#   arrange(desc(total_points))
# 
# saveRDS(total_points_2122_val_baseline, "allbuliupto2122valfinaltablebaseline.rds" )
# ### using optimal formul
# 
# mbaselinebulival = inla(formula = OpForm,
#                         data=buli2122dat,
#                         family="poisson",
#                         control.predictor=list(compute=TRUE,link=1),
#                         control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
# summary(mbaselinebulival)
# 
# AllBuli2122_val2 = AllBuli2122_val
# buli2122footie232 = roundNinlampo(1,AllBuli2122_val2,frm=OpForm)
# 
# 
# total_points_2122_val_opfrom= buli2122footie232$footieN %>%
#   filter(season_id == 3) %>%
#   group_by(Team) %>%
#   summarize(total_points = sum(points_won)) %>%
#   arrange(desc(total_points))
# 
# buli2122finaltableOPFORM = t5tablerounrR(1,buli2122r342$footieN)
# buli2122finaltableOPFORM
# saveRDS(buli2122finaltableOPFORM, "allbuliupto2122valfinaltableopform.rds" )
# 
# ######  reading table observed output
# 
# bl2122tab = read_excel("buli2122tab.xlsx") %>%
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



##### AllBuliupto21/22 op vs baseline round by round ####

AllBuli2122_val = MultiSznFootieMaker(buliallsched)

bbuli2122footie23 = roundNinlampo(23,AllBuli2122_val,frm = BaselineForm)
bbuli2122r24 = roundNinlampo(24,bbuli2122footie23$footieN,frm = BaselineForm)
bbuli2122r25 = roundNinlampo(25,bbuli2122r24$footieN,frm = BaselineForm) 
bbuli2122r26 = roundNinlampo(26,bbuli2122r25$footieN,frm = BaselineForm) 
bbuli2122r27 = roundNinlampo(27,bbuli2122r26$footieN,frm = BaselineForm) 
bbuli2122r28 = roundNinlampo(28,bbuli2122r27$footieN,frm = BaselineForm) 
bbuli2122r29 = roundNinlampo(29,bbuli2122r28$footieN,frm = BaselineForm) 
bbuli2122r30 = roundNinlampo(30,bbuli2122r29$footieN,frm = BaselineForm) 
bbuli2122r31 = roundNinlampo(31,bbuli2122r30$footieN,frm = BaselineForm) 
bbuli2122r32 = roundNinlampo(32,bbuli2122r31$footieN,frm = BaselineForm) 
bbuli2122r33 = roundNinlampo(33,bbuli2122r32$footieN,frm = BaselineForm) 
bbuli2122r34 = roundNinlampo(34,bbuli2122r33$footieN,frm = BaselineForm) 

buli2122finaltablebaseline = bbuli2122r34$footieN %>%
  filter(season_id == 3) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points))

buli2122finaltablebaseline

saveRDS(buli2122finaltablebaseline, "allbuli2122valfinaltablebaseline.rds" )


### using optimal formula


buli2122footie2 = AllBuli2122_val

buli2122footie232 = roundNinlampo(23,buli2122footie2,frm=OpForm)
buli2122r242 = roundNinlampo(24,buli2122footie232$footieN,frm=OpForm)
buli2122r252 = roundNinlampo(25,buli2122r242$footieN,frm=OpForm) 
buli2122r262 = roundNinlampo(26,buli2122r252$footieN,frm=OpForm) 
buli2122r272 = roundNinlampo(27,buli2122r262$footieN,frm=OpForm) 
buli2122r282 = roundNinlampo(28,buli2122r272$footieN,frm=OpForm) 
buli2122r292 = roundNinlampo(29,buli2122r282$footieN,frm=OpForm) 
buli2122r302 = roundNinlampo(30,buli2122r292$footieN,frm=OpForm) 
buli2122r312 = roundNinlampo(31,buli2122r302$footieN,frm=OpForm) 
buli2122r322 = roundNinlampo(32,buli2122r312$footieN,frm=OpForm) 
buli2122r332 = roundNinlampo(33,buli2122r322$footieN,frm=OpForm) 
buli2122r342 = roundNinlampo(34,buli2122r332$footieN,frm=OpForm) 

buli2122finaltableOPFORM = buli2122r342$footieN %>%
    filter(season_id == 3) %>%
    group_by(Team) %>%
    summarize(total_points = sum(points_won)) %>%
    arrange(desc(total_points))

buli2122finaltableOPFORM

saveRDS(buli2122finaltableOPFORM, "buli2122AfinaltableOPFORM.rds" )

###### Buli Mod Val Results ####
######  reading table observed output

bl2122tab = read_excel("buli2122tab.xlsx") %>%
  as.tibble() %>%
  mutate(total_points = Pts,
         Team = Squad) %>%
  select(Team, total_points) %>%
  arrange(Team)

bl2122OPFORM = buli2122finaltableOPFORM %>%
  select(Team, total_points) %>%
  arrange(Team)

bl2122baseline = buli2122finaltablebaseline %>%
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
saveRDS(comptab, "bullicomptabbl2122")

# Calculate the sum of squared differences for the baseline model
sum_sq_diff_baseline <- sum(bl2122baseline$Points_Difference ^ 2)

# Calculate the sum of squared differences for the optimized model
sum_sq_diff_optimized <- sum(bl2122OPFORM$Points_Difference ^ 2)

# Print the results
cat("Mean squared error for the baseline model:", sum_sq_diff_baseline/20, "\n")
cat("Mean squared error for the optimized model:", sum_sq_diff_optimized/20, "\n")


###### IT WORKSSSSSS WE GOT BETTER MEAN SQARE ERROR FOR OPTIMISED MODEL !!!!! ########


#### Final Predictions ####

OpForm = Goal ~ Home + 
  Gpg +
  GCpg +
  GDdiff +
  f(factor(Team), model = "iid") +
  f(factor(Opponent), model = "iid")  

AllBulifinal = AllBuli

MallBuli=inla(formula = OpForm,
               data=AllBulifinal,
               family="poisson",
               control.predictor=list(compute=TRUE,link=1),
               control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
MallBulisum = summary(MallBuli)
saveRDS(MallBulisum,"MallBulisumFinal.rds")

Buli2223r22 = roundNinlampo(22,AllBulifinal, frm = OpForm) 
Buli2223r23 = roundNinlampo(23,Buli2223r22$footieN, frm = OpForm) 
Buli2223r24 = roundNinlampo(24,Buli2223r23$footieN, frm = OpForm) 
Buli2223r25 = roundNinlampo(25,Buli2223r24$footieN, frm = OpForm) 
Buli2223r26 = roundNinlampo(26,Buli2223r25$footieN,frm = OpForm) 
Buli2223r27 = roundNinlampo(27,Buli2223r26$footieN,frm = OpForm) 
Buli2223r28 = roundNinlampo(28,Buli2223r27$footieN,frm = OpForm) 
Buli2223r29 = roundNinlampo(29,Buli2223r28$footieN,frm = OpForm) 
Buli2223r30 = roundNinlampo(30,Buli2223r29$footieN,frm = OpForm) 
Buli2223r31 = roundNinlampo(31,Buli2223r30$footieN,frm = OpForm) 
Buli2223r32 = roundNinlampo(32,Buli2223r31$footieN,frm = OpForm) 
Buli2223r33 = roundNinlampo(33,Buli2223r32$footieN,frm = OpForm) 
Buli2223r34 = roundNinlampo(34,Buli2223r33$footieN,frm = OpForm) 


Buli2223finaltab = Buli2223r34$footieN %>%
  filter(season_id == 4) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points)) %>%
  ungroup()

Buli2223finaltab
saveRDS(Buli2223finaltab,"Buli2223TabWithSuperLeaguers")

#### Final Predictions for league WITHUOT teams joining super league #####

OpForm = Goal ~ Home + 
  Gpg +
  GCpg +
  GDdiff +
  f(factor(Team), model = "iid") +
  f(factor(Opponent), model = "iid")    

teams_to_remove_buli <- c("Bayern Munich","Dortmund")

AllBulifinal2 = Buli2223r34$footieN %>%
  filter(!(Team %in% teams_to_remove_buli | Opponent %in% teams_to_remove_buli))

buli2223finaltab2 = AllBulifinal2 %>%
  filter(season_id == 4) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points))

buli2223finaltab2
saveRDS(buli2223finaltab2,"buli2223TabWithoutSuperLeaguers")

# Mallbuli2=inla(formula = OpForm,
#                data=AllBulifinal2,
#                family="poisson",
#                control.predictor=list(compute=TRUE,link=1),
#                control.compute=list(config=TRUE,dic=TRUE,waic=TRUE))
# Mallbulisum2 = summary(Mallbuli2)
# saveRDS(Mallbulisum2,"MallbulisumFinal2.rds")
# 
# Buli2223r222 = roundNinlampo(22,AllBulifinal2, frm = OpForm) 
# Buli2223r232 = roundNinlampo(23,Buli2223r222$footieN, frm = OpForm) 
# Buli2223r242 = roundNinlampo(24,Buli2223r232$footieN, frm = OpForm) 
# buli2223r252 = roundNinlampo(25,Buli2223r242$footieN, frm = OpForm) 
# buli2223r262 = roundNinlampo(26,buli2223r252$footieN,frm = OpForm) 
# buli2223r272 = roundNinlampo(27,buli2223r262$footieN,frm = OpForm) 
# buli2223r282 = roundNinlampo(28,buli2223r272$footieN,frm = OpForm) 
# buli2223r292 = roundNinlampo(29,buli2223r282$footieN,frm = OpForm) 
# buli2223r302 = roundNinlampo(30,buli2223r292$footieN,frm = OpForm) 
# buli2223r312 = roundNinlampo(31,buli2223r302$footieN,frm = OpForm) 
# buli2223r322 = roundNinlampo(32,buli2223r312$footieN,frm = OpForm) 
# buli2223r332 = roundNinlampo(33,buli2223r322$footieN,frm = OpForm) 
# buli2223r342 = roundNinlampo(34,buli2223r332$footieN,frm = OpForm) 
#  


buli2223finaltab2 = Buli2223r342$footieN %>%
  filter(season_id == 4) %>%
  group_by(Team) %>%
  summarize(total_points = sum(points_won)) %>%
  arrange(desc(total_points))

buli2223finaltab2
saveRDS(buli2223finaltab2,"buli2223TabWithoutSuperLeaguers")

