library(INLA)
library(tidyverse)
library(readxl)

# This file is used if we are using the fixture list updated for double game weeks (i.e postponed games)
prem2223 <- read_excel("fbref2223.xlsx")

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


# Calculate rank for each given round
premfootie = premfootie %>%
  group_by(Round.Number) %>%
  mutate(rank = dense_rank(desc(total_points))) %>%
  ungroup() %>% group_by(ID_game) %>% 
  mutate(diff_rank=c(rank[1]-rank[2],rank[2]-rank[1])) 


### fine here , idris




saveRDS(premfootie,"IdrisPrem2223.rds")




####### Notes:
# I think the main issue might just be the fact that results such as "total points" should be shifted down 1 round later, i.e the total points theyre on at the time of that game


######



# ANOVA-like model
formula = as.numeric(Goal) ~ Home + diff_point + diff_rank + form + #removing the following variables from the formuka to make it easier to run initially (idris) + form + diff_point + diff_rank + days_since_last + as.factor(tournament) +
  # days_since_last +
  Gpg +
  GCpg +
  rel_strength +
  f(factor(Team), model = "iid") +     # f() is used to define General Gasuain Model in INLA formula
  f(factor(Opponent), model = "iid") +
# Time component wek by team to account for difference in time performance
  # f(id_date,model="rw2",replicate=as.numeric(factor(Team))) +
# Overdispersion to account for extra goals
  f(num,model="iid")
# Model for Round r


# given that the data use is up to round r, we use 

r=19
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
m=inla(formula,
       data=data,
       family="poisson",
       control.predictor=list(compute=TRUE,link=1),
       control.compute=list(config=TRUE,dic=TRUE))
summary(m)


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





# # All plots in a single grob
# library(gridExtra)
# h=seq(1,ncol(scored),by=2)
# a=seq(2,ncol(scored),by=2)
# allplot=lapply(
#   1:length(h),function(x) 
#     joint_marginal(names(scored)[h[x]],names(scored)[a[x]],scored,max_goal=6,annotate_size=3.2,title="")
# )
# 
# 





# Test Prem data

set.seed(2223)

# premtest <- make_scored(28,m,nsims=10000)
# joint_marginal("Liverpool", "Fulham", premtest)  # Draw 1-1
# joint_marginal("Manchester Utd", "Brighton", premtest)  #draw 1-1
# joint_marginal("Manchester City", "West Ham", premtest) # draw 1-1




# now i need to make a way to update the table efficiently ?
# perhaps make a function that will extract the most likely outcome

# #round 28 unplayed games:
# 
# gw28u <- premfootie %>% filter(is.na(Goal) & Round.Number == 28)




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







# # Replace N with the desired round number
# gw28u <- premfootie %>% filter(is.na(Goal) & Round.Number == 28)

# Loop through each row in gwNu


updated_unplayed = function(N, scored) {

  gwNu <- premfootie %>% filter(is.na(Goal) & Round.Number == N)  
    
  for(i in 1:nrow(gwNu)) {
    # Extract the home and away teams for the i-th row
    team <- gwNu$Team[i]
    opp <- gwNu$Opponent[i]
  
    
    extractdata = scored %>% with(table(.[[team]],.[[opp]])) %>% prop.table() %>% 
      as_tibble(.name_repair = ~vctrs::vec_as_names(c(team,opp,"n"),quiet=TRUE)) %>% 
      mutate(across(where(is.character),as.numeric))
    
    max_row_index <- which.max(extractdata$n)
    max_row <- extractdata[max_row_index, ]
    
    # Extract the predicted goals for the home and away teams from the most likely result "max_row"
    team_goals <- max_row[1]
    # away_goals <- max_row[2] turns out this not needed since we are doing specific team rows in order
    
    gwNu$Goal[i] <- team_goals
  
  }

  return(gwNu)
}





# # Updating the Goal column in premfootie dataframe
# premfootie_updated <- premfootie %>%
#   mutate(Goal = ifelse(ID_game %in% gw28U$ID_game, gw28U$Goal[match(ID_game, gw28U$ID_game)], Goal))


# Define the update_footie function
update_footie <- function(gwU, footie) {
  footie_updated <- footie %>%
    mutate(Goal = case_when(
      ID_game %in% gwU$ID_game ~ as.character(gwU$Goal[match(ID_game, gwU$ID_game)]),
      TRUE ~ as.character(Goal)
    ))
  return(footie_updated)
}

# Update the premfootie dataframe
# premfootie <- update_footie(gw28U, premfootie)



var_updater = function(pf){
  ### create points won , ' total_points ' days_since_last variable
  
  pf = pf %>% group_by(ID_game) %>% 
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
    ) %>% ungroup()
  
  
  
  # Compute differences in ranks & points, and relative strength
  pf=pf %>% group_by(ID_game) %>% 
    mutate(
      diff_point=c(total_points[1]-total_points[2],total_points[2]-total_points[1]),
      rel_strength=c(total_points[1]/(total_points[1]+total_points[2]),total_points[2]/(total_points[1]+total_points[2]))
    ) %>% ungroup()
  
  
  
# create form column
  pf=pf %>% 
    group_by(Team) %>% 
    mutate(form=lag(zoo::rollsumr(points_won/rel_strength,3,fill=NA))) %>% ungroup()
  
  
# create GC,GD,GDdiff, ID columns
    pf <- pf %>%
    group_by(ID_game) %>%
    mutate(GC = c(Goal[2],Goal[1]),
           GD = c(as.numeric(Goal[1])-as.numeric(Goal[2]),as.numeric(Goal[2])-as.numeric(Goal[1])),
           GDdiff = c(GD[1]-GD[2],GD[2]-GD[1])) %>%
    ungroup() %>% group_by(Team) %>% 
    mutate(total_GC = cumsum(GC),
           total_G = cumsum(Goal),
           total_GD = cumsum(GD)) %>% ungroup()  %>% 
    group_by(ID_game) %>% mutate(ID=cur_group_id()) %>% ungroup() 
  
  
  # Calculate rank for each given round
  pf = pf %>%
    group_by(Round.Number) %>%
    mutate(rank = dense_rank(desc(total_points))) %>%
    ungroup() %>% group_by(ID_game) %>% 
    mutate(diff_rank=c(rank[1]-rank[2],rank[2]-rank[1])) 

  
  return(pf)
}


#rN 

roundN <- function(N, footie){
  rN <- make_scored(N,m,nsims=10000)
  gwNU <- updated_unplayed(N,rN)
  footie <- update_footie(gwNU, footie)
  footie <- var_updater(footie)
}






datprep = function(pf){
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

inlamark <- function(){}

m=inla(formula,
       data=data,
       family="poisson",
       control.predictor=list(compute=TRUE,link=1),
       control.compute=list(config=TRUE,dic=TRUE))
summary(m)


#for r=25
r25 = make_scored(25,m,nsims=10000)
gw25U <- updated_unplayed(25,r25)
premfootie <- update_footie(gw25U, premfootie)
premfootie <- var_updater(premfootie)

#for r=26

data = datprep(premfootie)

r25 = make_scored(25,m,nsims=10000)
gw25U <- updated_unplayed(25,r25)
premfootie <- update_footie(gw25U, premfootie)
premfootie <- var_updater(premfootie)







# i now need to fix the case for double gameweeks, sometimes there are cases where form = NA for example


