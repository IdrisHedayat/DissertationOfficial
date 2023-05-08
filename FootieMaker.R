presched = function(excelfile){
  leaguesheet = read_excel(excelfile)
  leaguesheet[c("HG", "AG")] = as.data.frame(do.call(rbind, strsplit(as.character(leaguesheet$Score), "–", fixed = TRUE)))  
  leaguesheet = leaguesheet[, !(names(leaguesheet) %in% c("Time", "Attendance", "Referee", "Match Report","Notes"))]
  leaguesheet
}


lvhtable <- function(df) {
  lvh <- data.frame(table(df$Venue, df$Home))
  lvh = subset(lvh, Freq != 0) %>%
    arrange(Var1)
  lvh
}

VenueHome <- function(df) {
  lvh = lvhtable(df)
  locations <- paste0("Location==", lvh[, 1], "~" , lvh[, 2])
  locationlist <- paste(locations, collapse = ", ")
  return(locationlist)
}


#### if we are only doing single seasons: 

FootieMaker = function(sched){
  
  # Long format
  
  sched =(
    sched %>% select(ID_game,date,home_team,home_score,tournament,Location,Round.Number) %>% 
      rename(Team=home_team,Goal=home_score) 
  ) %>% 
    bind_rows(
      sched %>% select(ID_game,date,away_team,away_score,tournament,Location,Round.Number) %>% 
        rename(Team=away_team,Goal=away_score)
    ) %>% arrange(ID_game) %>% mutate(Home=case_when(
      (Team==Location)~1,
      TRUE~0
    ))
  
  
  
  
  
  pf=sched
  
  # creates opponent variable 

  
  
  pf=pf %>% 
    group_by(ID_game) %>% mutate(
      Opponent=c(Team[2],Team[1])
    ) %>% ungroup() %>% 
    select(ID_game,date,Team,Goal,Opponent,tournament,Location,Home,everything()) 
  
  
  ### Trying to create points won , ' total_points ' to track the points tally, for form etc. as well as the days_since_last variable
  
  pf=pf %>% group_by(ID_game) %>% 
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
  pf=pf %>% group_by(ID_game) %>% 
    mutate(
      diff_point=c(total_points[1]-total_points[2],total_points[2]-total_points[1]),
      # diff_rank=c(rank[2]-rank[1],rank[1]-rank[2]),
      rel_strength=c(total_points[1]/(total_points[1]+total_points[2]),total_points[2]/(total_points[1]+total_points[2]))
    ) %>% ungroup()
  
  
  
  # Creates the "form" variable --- based on the proportion of points won in the last 3 games (weighted by the relative strength)
  pf=pf %>% 
    group_by(Team) %>% 
    mutate(game_number = row_number(),
           form= ifelse( game_number >= 5, lag(zoo::rollsumr(points_won/15,5,fill=NA)), NA_real_)) %>% 
    ungroup()
  
  
  #perhaps create a total goal scored column and conceded column, and maybe a goal difference column? I could even make a column that would use Goal difference- difference between each team ?
  # could experiment more with these, i.e. total GS diff, or making variables that similar to form track goalscoring form over past few games.
  pf <- pf %>%
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
  
 
  
  pf = pf %>%
    group_by(game_number) %>%
    mutate(rank = min_rank(desc(total_points)) +
             row_number(desc(total_GD)) / 10000) %>%
    mutate(rank = dense_rank(rank)) %>%
    ungroup() %>% 
    group_by(ID_game) %>%
    mutate(diff_rank = c(rank[1] - rank[2], rank[2] - rank[1])) %>%
    ungroup()
  

  #### finally adding a unique identifier for each team in each given game (i.e row number)
  pf = pf %>%
    mutate(num=row_number())
}



##### if we are doing multiple seasons #######


MultiSznFootieMaker = function(sched){
  
  # Long format
  
  sched =(
    sched %>% select(ID_game,season_id,date,home_team,home_score,tournament,Location,Round.Number) %>% 
      rename(Team=home_team,Goal=home_score) 
  ) %>% 
    bind_rows(
      sched %>% select(ID_game,season_id,date,away_team,away_score,tournament,Location,Round.Number) %>% 
        rename(Team=away_team,Goal=away_score)
    ) %>% arrange(ID_game) %>% mutate(Home=case_when(
      (Team==Location)~1,
      TRUE~0
    ))
  
  
  
  
  
  pf=sched
  
  # creates opponent variable 
  
  
  
  pf=pf %>% 
    group_by(ID_game) %>% mutate(
      Opponent=c(Team[2],Team[1])
    ) %>% ungroup() %>% 
    select(ID_game,date,Team,Goal,Opponent,tournament,Location,Home,everything()) 
  
  
  ### Trying to create points won , ' total_points ' to track the points tally, for form etc. as well as the days_since_last variable
  
  
  ### create points won , ' total_points ' days_since_last variable
  
  pf=pf %>% group_by(ID_game) %>% 
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
  pf=pf %>% group_by(ID_game) %>% 
    mutate(
      diff_point=c(total_points[1]-total_points[2],total_points[2]-total_points[1]),
      # diff_rank=c(rank[2]-rank[1],rank[1]-rank[2]),
      rel_strength=c(total_points[1]/(total_points[1]+total_points[2]),total_points[2]/(total_points[1]+total_points[2]))
    ) %>% ungroup()
  
  
  
  # Creates the "form" variable --- based on the proportion of points won in the last 3 games (weighted by the relative strength)
  pf=pf %>% 
    group_by(Team) %>% 
    mutate(game_number = row_number(),
           form= ifelse( game_number >= 5, lag(zoo::rollsumr(points_won/15,5,fill=NA)), NA_real_)) %>% 
    ungroup()
  
  
  #perhaps create a total goal scored column and conceded column, and maybe a goal difference column? I could even make a column that would use Goal difference- difference between each team ?
  # could experiment more with these, i.e. total GS diff, or making variables that similar to form track goalscoring form over past few games.
  pf <- pf %>%
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
  
  
  
  pf = pf %>%
    group_by(game_number) %>%
    mutate(rank = min_rank(desc(total_points)) +
             row_number(desc(total_GD)) / 10000) %>%
    mutate(rank = dense_rank(rank)) %>%
    ungroup() %>% 
    group_by(ID_game) %>%
    mutate(diff_rank = c(rank[1] - rank[2], rank[2] - rank[1])) %>%
    ungroup()
  
  
  #### finally adding a unique identifier for each team in each given game (i.e row number)
  pf = pf %>%
    mutate(num=row_number())
}