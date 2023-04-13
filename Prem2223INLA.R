library(INLA)
library(tidyverse)
library(readxl)

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

# this is what was added in the euro file
#   tournament="UEFA Euro",
#   home_score=NA,
#   away_score=NA,
#   neutral=FALSE
# ) %>% rename(
#   country=Location,
#   home_team=Home.Team,
#   away_team=Away.Team,
#   ID_game=Match.Number,
#   date=Date
# )


# Long format
premsched=(
  premsched %>% select(ID_game,date,home_team,home_score,tournament,Location,Round.Number) %>% 
    rename(Team=home_team,Goal=home_score) 
) %>% 
  bind_rows(
    premsched %>% select(ID_game,date,away_team,away_score,tournament,Location,Round.Number) %>% 
      rename(Team=away_team,Goal=away_score)
  ) %>% arrange(ID_game) %>% mutate(Home=case_when(
    (Team==Location)~1,
    TRUE~0
  ))





premfootie=premsched

# creates opponent variable 

# premfootie = premfootie %>% 
#   mutate(
#     Goal = Goal %>% as.numeric()
#   )


premfootie=premfootie %>% 
  group_by(ID_game) %>% mutate(
    Opponent=c(Team[2],Team[1])
  ) %>% ungroup() %>% 
  select(ID_game,date,Team,Goal,Opponent,tournament,Location,Home,everything()) 


### Trying to create points won , ' total_points ' to track the points tally, for form etc. as well as the days_since_last variable

premfootie=premfootie %>% group_by(ID_game) %>% 
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
premfootie=premfootie %>% group_by(ID_game) %>% 
  mutate(
    diff_point=c(total_points[1]-total_points[2],total_points[2]-total_points[1]),
    # diff_rank=c(rank[2]-rank[1],rank[1]-rank[2]),
    rel_strength=c(total_points[1]/(total_points[1]+total_points[2]),total_points[2]/(total_points[1]+total_points[2]))
  ) %>% ungroup()



# Creates the "form" variable --- based on the proportion of points won in the last 3 games (weighted by the relative strength)
premfootie=premfootie %>% 
  group_by(Team) %>% 
    mutate(game_number = row_number(),
           form= ifelse( game_number >= 5, lag(zoo::rollsumr(points_won/15,5,fill=NA)), NA_real_)) %>% 
  ungroup()


#perhaps create a total goal scored column and conceded column, and maybe a goal difference column? I could even make a column that would use Goal difference- difference between each team ?
# could experiment more with these, i.e. total GS diff, or making variables that similar to form track goalscoring form over past few games.
premfootie <- premfootie %>%
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

###### There is a problem with calculating rank here, it doesnt accurately monitor league position at the time, perhaps consider an ELO ranking instead ############
# Calculate rank for each given round.  

premfootie = premfootie %>%
  group_by(Round.Number) %>%
  mutate(rank = dense_rank(desc(total_points))) %>%
  ungroup() %>% group_by(ID_game) %>% 
  mutate(diff_rank=c(rank[1]-rank[2],rank[2]-rank[1])) 


### fine here , idris




saveRDS(premfootie,"IdrisPremfootie2223.rds")




####### Notes:
# I think the main issue might just be the fact that results such as "total points" should be shifted down 1 round later, i.e the total points theyre on at the time of that game





########## Formula  #########




# ANOVA-like model

formula = Goal ~ Home + 
  # diff_point +
  # diff_rank +
  # form +
  rel_strength +
  days_since_last +
  Gpg +
  # GCpg +
  GDdiff + 
  f(factor(Team), model = "iid") +     # f() is used to define General Gasuain Model in INLA formula
  f(factor(Opponent), model = "iid") # +
# Time component wek by team to account for difference in time performance
  # f(id_date,model="rw2",replicate=as.numeric(factor(Team))) +
# Overdispersion to account for extra goals
  # f(num,model="iid") +
# seeing if   f(factor(Location), model = "iid") + impacts
# f(factor(Location), model = "iid") 




##########  Prepping Premfootie -> Data #########

# Model for Round r


# given that the data use is up to round r, we use 

r=28
data=
  # Here "fixes" the data
  premfootie %>% 
  arrange(date) %>% mutate(
    form=case_when((is.nan(form)|is.infinite(form))~0,TRUE~form),
    # And scales all the continuous covariates for ease of fitting
    # diff_point=scale(diff_point,scale=TRUE),
    # diff_rank=scale(diff_rank,scale=TRUE),
    # days_since_last=scale(days_since_last,scale=TRUE),
    num=row_number(),
    id_date=date %>% as.factor() %>% as.numeric()
  ) %>% 
  # Then filters only the games in a given round (for prediction)
  filter(Round.Number%in%c(NA,1:r)) # Here I have changed the code to 1:r to ensure it keeps the updated data too



########## INLA Model #########
m=inla(formula,
       data=data,
       family="poisson",
       control.predictor=list(compute=TRUE,link=1),
       control.compute=list(config=TRUE,dic=TRUE))
summary(m)


runINLA <- function(dat){
  inmod=inla(formula,
         data=dat,
         family="poisson",
         control.predictor=list(compute=TRUE,link=1),
         control.compute=list(config=TRUE,dic=TRUE))
  return(inmod)
}


########## Source Utility Functions ########

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


# For the RW2 by team model
time_trend=function(m,date_start="2022-01-01",date_end="2023-06-11") {
  ii=premfootie %>% mutate(Team2=Team,Team=factor(Team)) %>% mutate(Team=as.numeric(Team)) %>% 
    filter(date>=date_end) %>% select(ID,date,Team2,Team,everything()) %>% with(unique(Team))
  labs=(premfootie %>% with(levels(as.factor(Team))))[sort(ii)]
  m$summary.random$id_date %>% as_tibble() %>% 
    mutate(TeamID=rep(1:(premfootie %>% with(length(unique(Team)))),each=length(unique(m$summary.random$id_date$ID))),
           Team=rep(levels(as.factor(premfootie$Team)),each=length(unique(m$summary.random$id_date$ID)))) %>% 
    filter(TeamID %in% ii) %>% ggplot(aes(ID,mean,group=Team))+geom_line()+geom_ribbon(aes(ymin=`0.025quant`,ymax=`0.975quant`),alpha=.2) +
    facet_wrap(~Team) +
    scale_x_continuous(
      name="Time",
      breaks=premfootie %>% filter(date>=date_start) %>% arrange(ID) %>% mutate(tt=lubridate::year(date),id_date=date %>% as.factor %>% as.numeric()) %>% 
        select(ID,date,tt,id_date,everything()) %>% group_by(tt) %>% slice(1) %>% pull(id_date),
      labels=lubridate::year(date_start):lubridate::year(date_end)
    ) + theme(axis.text.x=element_text(angle=60, hjust=1)) + ylab("Historical performance")
}

attack_defense=function(m) {
  options(dplyr.show_progress = FALSE)
  ii=premfootie %>% mutate(Team2=Team,Team=factor(Team)) %>% mutate(Team=as.numeric(Team)) %>% 
    filter(date>="2021-06-11") %>% select(ID,date,Team2,Team,everything()) %>% with(unique(Team))
  labs=(premfootie %>% with(levels(as.factor(Team))))[sort(ii)]
  (m$summary.random$`factor(Team)` %>% as_tibble() %>% filter(ID%in%labs) %>% mutate(attack=mean)) %>% 
    bind_cols((m$summary.random$`factor(Opponent)` %>% as_tibble() %>% filter(ID%in%labs) %>% mutate(defense=mean))) %>% 
    select(Team=`ID...1`,attack,defense) %>% ggplot(aes(defense,attack,label=Team))+geom_text() + 
    geom_vline(xintercept=0,linetype="dashed",size=.6) + geom_hline(yintercept=0,linetype="dashed",size=.6)
}

team_strength=function(m,effect="attack") {
  options(dplyr.show_progress = FALSE)
  ii=premfootie %>% mutate(Team2=Team,Team=factor(Team)) %>% mutate(Team=as.numeric(Team)) %>% 
    filter(date>="2021-06-11") %>% select(ID,date,Team2,Team,everything()) %>% with(unique(Team))
  labs=(premfootie %>% with(levels(as.factor(Team))))[sort(ii)]
  
  if(effect=="attack") {
    pl=(m$summary.random$`factor(Team)` %>% as_tibble() %>% filter(ID%in%labs) %>% 
          mutate(attack=mean,attack025=`0.025quant`,attack975=`0.975quant`)) %>% 
      select(Team=`ID`,attack,attack025,attack975) %>% ggplot(aes(x=attack,y=fct_reorder(Team,attack)))+
      geom_linerange(aes(xmin=attack025,xmax=attack975),position=position_dodge(.3))+
      geom_point(aes(attack,Team),position = position_dodge(0.3),size=2.3) +
      xlab("Attacking ability")+geom_vline(xintercept=0,size=1.1,linetype="dashed")
  }
  if(effect=="defense") {
    pl=(m$summary.random$`factor(Opponent)` %>% as_tibble() %>% filter(ID%in%labs) %>% 
          mutate(attack=mean,attack025=`0.025quant`,attack975=`0.975quant`)) %>% 
      select(Team=`ID`,attack,attack025,attack975) %>% ggplot(aes(x=attack,y=fct_reorder(Team,attack)))+
      geom_linerange(aes(xmin=attack025,xmax=attack975),position=position_dodge(.3))+
      geom_point(aes(attack,Team),position = position_dodge(0.3),size=2.3) +
      xlab("Defending ability")+geom_vline(xintercept=0,size=1.1,linetype="dashed")
  }
  pl
}

#IDris, tried to remove mutate(form=case_when....)

# Post-processing Used to predict the number of goals scored in.a new game
make_scored=function(round,model,nsims=1000) {
  r=round
  m=model
  # Then selects the relevant indices
  idx=(data %>%  mutate(form=case_when((is.nan(form)|is.infinite(form))~0,TRUE~form)) %>% 
         filter(Round.Number%in%c(NA,r))) %>% mutate(num=row_number()) %>% filter(Round.Number==r) %>% pull(num)
  jpost=inla.posterior.sample(n=nsims,m)
  topredict=tail(grep("Predictor",rownames(jpost[[1]]$latent)),length(idx))
  theta.pred=matrix(exp(unlist(lapply(jpost,function(x) x$latent[idx,]))),ncol=length(idx),byrow=T) 
  colnames(theta.pred)= (data %>%  mutate(form=case_when((is.nan(form)|is.infinite(form))~0,TRUE~form)) %>%
                           filter(Round.Number%in%c(NA,r))) %>% mutate(num=row_number()) %>% filter(Round.Number==r) %>% pull(Team)
  theta.pred=theta.pred %>% as_tibble()
  # Predictions from the posterior distribution for the number of goals scored
  scored=theta.pred %>% mutate(across(everything(),~rpois(nrow(theta.pred),.)))
}






####
# Utility functions for plots & summaries of the output
# Plots the joint posterior distributions of all the possible scores
plot_joint=function(x,y,scored,result=NULL,annotate=TRUE,...) {
  exArgs=list(...)
  if(exists("annotate_size",exArgs)) {annotate_size=exArgs$annotate_size} else {annotate_size=6}
  if(exists("title",exArgs)) {title=exArgs$title} else {title=paste0("Joint posterior probability for the number of goal scored: ",x," vs ",y)}
  
  data=scored %>% with(table(.[[x]],.[[y]])) %>% prop.table() %>% 
    as_tibble(.name_repair = ~vctrs::vec_as_names(c(x,y,"n"),quiet=TRUE)) %>% 
    mutate(across(where(is.character),as.numeric))
  if(!is.null(result)){
    data=data %>% mutate(obs=case_when((.[[x]]==result[1] & .[[y]]==result[2])~1,TRUE~0)) 
  }
  pl = data %>% ggplot(aes(as.factor(!!sym(x)),as.factor(!!sym(y)),fill=100*n))+geom_tile()
  if(!is.null(result)) {
    pl=pl+geom_text(aes(label=paste0(format(100*n,digits=2,nsmall=2),"%"),color=obs>0),size=annotate_size,fontface="bold")+
      scale_fill_gradient(low="white", high="black")+#theme_gb()+
      xlab(x) + ylab(y) +
      labs(fill="",title=title)+
      # Removes the legend (don't really need it if the probability values are displayed in the tiles...)
      theme(legend.position = "none") +
      geom_abline(slope=1,intercept=0,size=.1)+scale_color_manual(guide=FALSE,values=c("white","red"))
  } else {
    pl=pl+geom_text(aes(label=paste0(format(100*n,digits=2,nsmall=2),"%")),color="white",size=annotate_size,fontface="bold") +
      scale_fill_gradient(low="white", high="black")+#theme_gb()+
      xlab(x) + ylab(y) +
      labs(fill="",title=title)+
      # Removes the legend (don't really need it if the probability values are displayed in the tiles...)
      theme(legend.position = "none") +
      geom_abline(slope=1,intercept=0,size=.1)
  }
  if(annotate) {
    pl=pl + 
      annotate(
        "text",x=Inf,y=Inf,
        label=paste0("Pr(",x," win)=",format(
          100*scored %>% select(!!sym(x),!!sym(y)) %>% 
            mutate(Homewin=!!sym(x)>!!sym(y),Draw=!!sym(x)==!!sym(y),Awaywin=!!sym(y)>!!sym(x)) %>% 
            select(-c(!!sym(x),!!sym(y))) %>% summarise(across(everything(),~mean(.))) %>% pull(Homewin),
          digits=2,nsmall=2
        ),"%"),
        hjust=1,vjust=10
      ) +
      annotate(
        "text",x=Inf,y=Inf,
        label=paste0("Pr(",y," win)=",format(
          100*scored %>% select(!!sym(x),!!sym(y)) %>% 
            mutate(Homewin=!!sym(x)>!!sym(y),Draw=!!sym(x)==!!sym(y),Awaywin=!!sym(y)>!!sym(x)) %>% 
            select(-c(!!sym(x),!!sym(y))) %>% summarise(across(everything(),~mean(.))) %>% pull(Awaywin),
          digits=2,nsmall=2
        ),"%"),
        hjust=1,vjust=12
      ) +
      annotate(
        "text",x=Inf,y=Inf,
        label=paste0("Pr(Draw)=",format(
          100*scored %>% select(!!sym(x),!!sym(y)) %>% 
            mutate(Homewin=!!sym(x)>!!sym(y),Draw=!!sym(x)==!!sym(y),Awaywin=!!sym(y)>!!sym(x)) %>% 
            select(-c(!!sym(x),!!sym(y))) %>% summarise(across(everything(),~mean(.))) %>% pull(Draw),
          digits=2,nsmall=2
        ),"%"),
        hjust=1,vjust=14
      )
  }
  if(exists("max_goal",exArgs)){
    max_goal=exArgs$max_goal
    pl=pl+xlim(levels(as.factor(data[[x]]))[1:(max_goal+1)])+ylim(levels(as.factor(data[[x]]))[1:(max_goal+1)])
  }
  pl
}

outcome_predict=function(x,y,scored) {
  scored %>% select(!!sym(x),!!sym(y)) %>% 
    mutate(Homewin=!!sym(x)>!!sym(y),Draw=!!sym(x)==!!sym(y),Awaywin=!!sym(y)>!!sym(x)) %>% 
    select(-c(!!sym(x),!!sym(y))) %>% summarise(across(everything(),~mean(.))) %>% 
    setNames(
      c(paste0(x,"_win"),"Draw",paste0(y,"_win"))
    )
}


joint_marginal=function(x,y,scored,result=NULL,...) {
  exArgs=list(...)
  if(exists("annotate_size",exArgs)) {annotate_size=exArgs$annotate_size} else {annotate_size=6}
  if(exists("title",exArgs)) {title=exArgs$title} else {title=paste0("Joint posterior probability for the number of goal scored: ",x," vs ",y)}
  
  empty=ggplot()+geom_point(aes(1,1), colour="white")+
    theme(axis.ticks=element_blank(), 
          panel.background=element_blank(), 
          axis.text.x=element_blank(), axis.text.y=element_blank(),           
          axis.title.x=element_blank(), axis.title.y=element_blank())
  hist_top=scored %>% ggplot(aes(as.factor(!!sym(x))))+geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+ylab("")+xlab("")
  if(exists("max_goal",exArgs)){
    max_goal=exArgs$max_goal
    hist_top=hist_top+xlim(levels(as.factor(scored[[x]]))[1:(max_goal+1)])
  }
  hist_side=scored %>% ggplot(aes(as.factor(!!sym(y))))+geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))+ylab("")+xlab("")+coord_flip()
  if(exists("max_goal",exArgs)){
    max_goal=exArgs$max_goal
    hist_side=hist_side+xlim(levels(as.factor(scored[[x]]))[1:(max_goal+1)])
  }
  center=plot_joint(x,y,scored,result=result,...)
  
  gridExtra::grid.arrange(hist_top, empty, center, hist_side, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
}

#### running some utility functions

attack_defense(m) #plot of the attack and defense effects


#plotting the respective attack and defending effects seperately i.e ability to defend (concede less), or attack more (more goals)
team_strength(m, "attack")
team_strength(m, "defense")









# Test Prem data

set.seed(2223)

# premtest <- make_scored(28,m,nsims=10000)
# joint_marginal("Liverpool", "Fulham", premtest)  # Draw 1-1
# joint_marginal("Manchester Utd", "Brighton", premtest)  #draw 1-1
# joint_marginal("Manchester City", "West Ham", premtest) # draw 1-1





# #testing out if extraction of joint marginal results is correct
# e2clubs <- function(scored,team,opp) {
#   extracteddata = scored %>% with(table(.[[team]],.[[opp]])) %>% prop.table() %>% 
#     as_tibble(.name_repair = ~vctrs::vec_as_names(c(team,opp,"n"),quiet=TRUE)) %>% 
#     mutate(across(where(is.character),as.numeric))
#   
#   return(extracteddata)
#   
# }
# 
# 
# livful <- e2clubs(premtest,"Liverpool","Fulham")
# briman <- e2clubs(premtest,"Brighton","Manchester Utd")
# wesman <- e2clubs(premtest,"West Ham","Manchester City")






########## Post Processing  #########

# # Replace N with the desired round number

# Loop through each row in gwNu


#for r=28
# r28 = make_scored(28,m,nsims=10000)
# gw28u = premfootie %>% filter(is.na(Goal & Round.Number == 28))

########## updated_unplayed if its using the most probable joint outcome #########

# updated_unplayed = function(N, scored) {
#   
#   gwNu <- premfootie %>% filter(is.na(Goal) & Round.Number == N)  
#   
#   for(i in 1:nrow(gwNu)) {
#     # Extract the home and away teams for the i-th row
#     team <- gwNu$Team[i]
#     opp <- gwNu$Opponent[i]
#     
#     
#     extractdata = scored %>% with(table(.[[team]],.[[opp]])) %>% prop.table() %>% 
#       as_tibble(.name_repair = ~vctrs::vec_as_names(c(team,opp,"n"),quiet=TRUE)) %>% 
#       mutate(across(where(is.character),as.numeric))
#     
#     max_row_index <- which.max(extractdata$n)
#     max_row <- extractdata[max_row_index, ]
#     
#     # Extract the predicted goals for the home and away teams from the most likely result "max_row"
#     team_goals <- max_row[1]
#     # away_goals <- max_row[2] turns out this not needed since we are doing specific team rows in order
#     
#     gwNu$Goal[i] <- team_goals
#     
#   }
#   
#   return(gwNu)
# }


updated_unplayed = function(N, scored) {
  
  gwNu = premfootie %>% filter(is.na(Goal) & (Round.Number == N))
  

  scored_long = scored %>%
    pivot_longer(cols = everything(),
                 names_to = "Team",
                 values_to = "Goals")

  
  for(i in 1:nrow(gwNu)) {
    # Extract the home and away teams for the i-th row
    team = gwNu$Team[i]
    
    # Calculate the mean of the scored tibble for the given team
    team_goals = scored_long %>%
      filter(Team == team) %>%
      summarize(mean_goals = mean(Goals, na.rm = TRUE)) %>%
      pull(mean_goals)
    
    gwNu$Goal[i] = round(team_goals)
  }
  
  return(gwNu %>% select(ID_game, Team, Goal, Opponent))
}


##### but if 




# Define the update_footie function that just updates the prem footie goal column with the new updated unplayed fixtures in the given rund
update_footie = function(gwNu, footie) {
  footie_updated = footie %>%
   rows_update(gwNu, by = c("ID_game", "Team"))
  return(footie_updated)
}

# ufr28test = update_footie(gw28U,premfootie)

# var updater just updates the other variables such as form points won etc, based on the new goal inputs
#uses the same code as before nothing new
var_updater = function(pf){
  pf=pf %>% 
    group_by(ID_game) %>% mutate(
      Opponent=c(Team[2],Team[1])
    ) %>% ungroup() %>% 
    select(ID_game,date,Team,Goal,Opponent,tournament,Location,Home,everything()) 
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
  

  pf=pf %>% group_by(ID_game) %>% 
    mutate(
      diff_point=c(total_points[1]-total_points[2],total_points[2]-total_points[1]),
      # diff_rank=c(rank[2]-rank[1],rank[1]-rank[2]),
      rel_strength=c(total_points[1]/(total_points[1]+total_points[2]),total_points[2]/(total_points[1]+total_points[2]))
    ) %>% ungroup()
  
   pf=pf %>% 
    group_by(Team) %>% 
    mutate(game_number = row_number(),
           form= ifelse( game_number >= 5, lag(zoo::rollsumr(points_won/15,5,fill=NA)), NA_real_)) %>% 
    ungroup()
  
  
  #perhaps create a total goal scored column and conceded column, and maybe a goal difference column? I could even make a column that would use Goal difference- difference between each team ?
  # could experiment more with these, i.e. total GS diff, or making variables that similar to form track goalscoring form over past few games.
  pf = pf %>%
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
  
  
  # Calculate rank for each given round
  pf = pf %>%
    group_by(Round.Number) %>%
    mutate(rank = dense_rank(desc(total_points))) %>%
    ungroup() %>% group_by(ID_game) %>% 
    mutate(diff_rank=c(rank[1]-rank[2],rank[2]-rank[1]))  %>%
    ungroup()
    
  return(pf)
}






datprep = function(pf,r){
  data=
    # Here "fixes" the data
    pf %>% 
    arrange(date) %>% mutate(
      form=case_when((is.nan(form)|is.infinite(form))~0,TRUE~form),
      # And scales all the continuous covariates for ease of fitting
      # diff_point=scale(diff_point,scale=TRUE),
      # diff_rank=scale(diff_rank,scale=TRUE),
      # days_since_last=scale(days_since_last,scale=TRUE),
      num=row_number(),
      id_date=date %>% as.factor() %>% as.numeric()
    ) %>% 
    # Then filters only the games in a given round (for prediction)
    filter(Round.Number%in%c(NA,1:r)) # Here I have changed the code to 1:r to ensure it keeps the updated data too  
  return(data)
}



#rN  does all above for a given round N including the initial make_scored() 

roundN = function(N, rN, footie){
  gwNU = updated_unplayed(N,rN)
  footie = update_footie(gwNU, footie)
  footie = var_updater(footie)
  return(footie)
}

#running predictions from INLA
r28 = make_scored(28,m,nsims=10000)
# then running roundN which updates premfootie df with the new goals then updates the rest of the variables based on these new results
premfootie28 = roundN(28,r28, premfootie)



####### Round 29 #######
r=29
data = datprep(premfootie28,r)
#after this go back to the INLA Model section and run before using round N again
#after going back to line 237 and running INLA, we run roundN for round 29, using the last used premfootie dataset which in this case is premfootie28

#make sure to rune make_scored for this given round first
r29 = make_scored(29, m, nsims=10000)
#now we can run roundN using the arguments N, rN, premfootie`N-1`
premfootie29 = roundN(29,r29, premfootie28)

###
###
##### I have tried creating functions that will streamline the process
##### instead of iteratively doing it, but iterative works for now
##### when using functions I get errors particularly when running roundN because of updated_unplayed 's use of pivot_lonsger()
###
###


####### Round 30 #######
r=30
data = datprep(premfootie29,r)
m=runINLA(data)
r30=make_scored(30,m,nsims=10000)
premfootie30 = roundN(30,r30,premfootie29)

####### Round 31 #######

r=31
data = datprep(premfootie30,r)
m=runINLA(data)
r31=make_scored(31,m,nsims=10000)
premfootie31 = roundN(31,r31,premfootie30)

####### Round 32 #######
# trying to make a function that runs it all at once doesnt work because of :
# Error in `pivot_longer()`:
#   ! `cols` must select at least one column.
# runtest = function(pf, n){
#   x=n
#   data = datprep(pf,x)
#   m=runINLA(data)
#   rN=make_scored(x,m,nsims=10000)
#   premfootie32 = roundN(x,rN,pf)  
# 
# }
# 
# premfootie32test = runtest(premfootie31, n=32)



r=32
data = datprep(premfootie31,r)
m=runINLA(data)
r32=make_scored(32,m,nsims=10000)
premfootie32 = roundN(32,r32,premfootie31)


####### Round 33 #######

r=33
data = datprep(premfootie32,r)
m=runINLA(data)
r33=make_scored(33,m,nsims=10000)
premfootie33 = roundN(33,r33,premfootie32)


####### Round 34 #######

r=34
data = datprep(premfootie33,r)
m=runINLA(data)
r34=make_scored(34,m,nsims=10000)
premfootie34 = roundN(34,r34,premfootie33)


####### Round 35 #######

r=35
data = datprep(premfootie34,r)
m=runINLA(data)
r35=make_scored(35,m,nsims=10000)
premfootie35 = roundN(35,r35,premfootie34)


####### Round 36 #######

r=36
data = datprep(premfootie35,r)
m=runINLA(data)
r36=make_scored(36,m,nsims=10000)
premfootie36 = roundN(36,r36,premfootie35)


####### Round 37 #######

r=37
data = datprep(premfootie36,r)
m=runINLA(data)
r37=make_scored(37,m,nsims=10000)
premfootie37 = roundN(37,r37,premfootie36)


####### Round 38 #######

r=38
data = datprep(premfootie37,r)
m=runINLA(data)
r38=make_scored(38,m,nsims=10000)
premfootie38 = roundN(38,r38,premfootie37)


####### Round 39 #######

r=39
data = datprep(premfootie38,r)
m=runINLA(data)
r39=make_scored(39,m,nsims=10000)
premfootie39 = roundN(39,r39,premfootie38)


####### Round 40 #######

r=40
data = datprep(premfootie39,r)
m=runINLA(data)
r40=make_scored(40,m,nsims=10000)
premfootie40 = roundN(40,r40,premfootie39)


####### Round 41 #######

r=41
data = datprep(premfootie40,r)
m=runINLA(data)
r41=make_scored(41,m,nsims=10000)
premfootie41 = roundN(41,r41,premfootie40)


####### Round 42 #######

r=42
data = datprep(premfootie41,r)
m=runINLA(data)
r42=make_scored(42,m,nsims=10000)
premfootie42 = roundN(42,r42,premfootie41)


####### Round 43 #######

r=43
data = datprep(premfootie42,r)
m=runINLA(data)
r43=make_scored(43,m,nsims=10000)
premfootie43 = roundN(43,r43,premfootie42)


####### Round 44 #######

r=44
data = datprep(premfootie43,r)
m=runINLA(data)
r44=make_scored(44,m,nsims=10000)
premfootie44 = roundN(44,r44,premfootie43)

####### Compiling Premier League Table #######
saveRDS(premfootie44,"r44FinalfootieFrame.rds")

Finalround = premfootie44 %>% filter( Round.Number == 44)



premier_league_table <- Finalround %>%
  arrange(desc(total_points), desc(total_GD), desc(total_G)) %>%
  ungroup() %>%        # had to add ungroup() here because it automatically groups by ID_game
  mutate(Position = row_number()) %>%
  select(Position,Team,total_points, total_G, total_GC, total_GD) 



premier_league_table 


saveRDS(premier_league_table, "EndOfSeasonTable2223.rds")



###### Ninla - trying making it quicker #######
Ninla = function(N,pf){
  r=N
  df = datprep(pf,r)

  
  inlam = runINLA(df)
  # return(summary(inmod)) ?
  set.seed(2223)
  rN = make_scored(N,inlam,nsims=10000)
  
  # return(head(df))
  # return(head(rN))
  premfootieN = roundN(r,rN,pf)
  
  return(premfootieN)
}
# ##### doesnt work because of some error 
# > premfootie30 = Ninla(30,premfootie29)
# Error in `pivot_longer()`:
#   ! `cols` must select at least one column.
# Run `rlang::last_error()` to see where the error occurred.

# premfootie30 = Ninla(30,premfootie29)
# premfootie31 = Ninla(31,premfootie30)
# premfootie32 = Ninla(32,premfootie31)
# premfootie33 = Ninla(33,premfootie32)
# premfootie34 = Ninla(34,premfootie33)
# premfootie35 = Ninla(35,premfootie34)
# premfootie36 = Ninla(36,premfootie35)
# premfootie37 = Ninla(37,premfootie36)
# premfootie38 = Ninla(38,premfootie37)
# premfootie39 = Ninla(39,premfootie38)
# premfootie40 = Ninla(40,premfootie39)
# premfootie41 = Ninla(41,premfootie40)
# premfootie42 = Ninla(42,premfootie41)
# premfootie43 = Ninla(43,premfootie42)
# premfootie44 = Ninla(44,premfootie43)
