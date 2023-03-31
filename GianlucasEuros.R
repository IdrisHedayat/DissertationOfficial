library(INLA)
library(tidyverse)


# when running I was getting an error frmo the as_date function since the csv file included time in the Date column
#so, changed the lubridate function to this:
#schedule$Date <- as.Date(schedule$Date, format = "%d/%m/%Y")





# Schedule for Euro2020
schedule = read.csv("euro2020_schedule.csv") %>% as_tibble() %>% 
  mutate(
    Date=as.Date(Date, format = "%d/%m/%Y"),
    Location=case_when(
      Location=="Olimpico in Rome"~"Italy",
      Location=="Baki Olimpiya Stadionu"~"Azerbaijan",
      Location=="Parken"~"Denmark",
      Location=="Saint Petersburg Stadium"~"Russia",
      Location=="Wembley Stadium"~"England",
      Location=="Arena Nationala"~"Romania",
      Location=="Johan Cruijff ArenA"~"Netherlands",
      Location=="Hampden Park"~"Scotland",
      Location=="Estadio La Cartuja"~"Spain",
      Location=="Puskás Aréna"~"Hungary",
      Location=="Fußball Arena München"~"Germany"
    ),
    city=case_when(
      Location=="Italy"~"Rome",
      Location=="Azerbaijan"~"Baku",
      Location=="Denmark"~"Copenhagen",
      Location=="Russia"~"Saint Petersburg",
      Location=="England"~"London",
      Location=="Romania"~"Bucharest",
      Location=="Netherlands"~"Amsterdam",
      Location=="Scotland"~"Glasgow",
      Location=="Spain"~"Sevilla",
      Location=="Hungary"~"Budapest",
      Location=="Germany"~"Munich"
    ),
    tournament="UEFA Euro",
    home_score=NA,
    away_score=NA,
    neutral=FALSE
  ) %>% rename(
    country=Location,
    home_team=Home.Team,
    away_team=Away.Team,
    ID_game=Match.Number,
    date=Date
  )


# Long format
schedule=(
  schedule %>% select(ID_game,date,home_team,home_score,tournament,city,country,neutral,Round.Number) %>% 
    rename(Team=home_team,Goal=home_score) 
) %>% 
  bind_rows(
    schedule %>% select(ID_game,date,away_team,away_score,tournament,city,country,neutral,Round.Number) %>% 
      rename(Team=away_team,Goal=away_score)
  ) %>% arrange(ID_game) %>% mutate(Home=case_when(
    (Team==country)~1,
    TRUE~0
  ))

schedule=schedule %>% mutate(
  neutral=case_when(
    (Home==1 & country==Team)~TRUE,
    TRUE~neutral
  )
)

# Data source: https://www.kaggle.com/martj42/international-football-results-from-1872-to-2017?select=results.csv
footie=read.csv("International_history.csv") %>% as_tibble() %>% 
  mutate(date=lubridate::as_date(date)) %>% filter(date>="2009-01-01") %>% 
  mutate(ID_game=row_number())

# Long format
footie=(
  footie %>% select(ID_game,date,home_team,home_score,tournament,city,country,neutral) %>% 
    rename(Team=home_team,Goal=home_score) 
) %>% 
  bind_rows(
    footie %>% select(ID_game,date,away_team,away_score,tournament,city,country,neutral) %>% 
      rename(Team=away_team,Goal=away_score)
  ) %>% arrange(ID_game) %>% mutate(Home=case_when(
    (Team==country)~1,
    TRUE~0
  ))

# Adds the scheduled games to the historical data
footie=footie %>% bind_rows(
  schedule %>% mutate(ID_game=ID_game+max(footie$ID_game))
)

footie1=footie #added this myself to work around

# FIFA ranking dataset
# Source: https://www.kaggle.com/cashncarry/fifaworldranking
ranking=read.csv("FIFA_ranking_2021.csv") %>% as_tibble() %>% 
  mutate(
    rank_date=lubridate::as_date(rank_date),
    country_full=case_when(
      country_full=="USA"~"United States",
      country_full=="Korea DPR"~"South Korea",
      country_full=="IR Iran"~"Iran",
      TRUE~country_full
    )
  ) %>% 
  filter(rank_date>="2009-01-01")        ### fine here , idris

# Merges the ranking giving the value that is closer to the game
footie=footie %>% left_join(ranking,by=c("Team"="country_full")) %>% 
  select(-c(country_abrv,rank_change)) %>% group_by(ID_game,Team) %>% 
   # This only selects UEFA nations
#  filter(date>=rank_date,confederation=="UEFA") %>%
   # While this would keep every team
   filter(date>=rank_date) %>% 
   slice(n()) %>% ungroup() %>% 
  # Selects only the games where both teams are in UEFA
  group_by(ID_game) %>% filter(n()>1) %>% ungroup()




### fine here , idris 

## This keeps **all** the games in which at least one team are in UEFA
# footie=footie |> left_join(ranking,by=c("Team"="country_full")) |> select(-c(country_abrv,rank_change)) |> 
#   group_by(ID_game,Team) |> filter(date>=rank_date) |> slice(n()) |> ungroup() |> group_by(ID_game) |> 
#   filter(n()>1) |> ungroup() |> select(ID_game,date,Team,tournament,confederation,everything()) |> 
#   group_by(ID_game) |> filter((confederation[1]=="UEFA" | confederation[2]=="UEFA")) |> ungroup()

# Compute differences in ranks & points
footie=footie %>% group_by(ID_game) %>% 
  mutate(
    diff_point=c(total_points[1]-total_points[2],total_points[2]-total_points[1]),
    diff_rank=c(rank[2]-rank[1],rank[1]-rank[2]),
    points_won=case_when(
      (Goal[1]>Goal[2])~c(3,0),
      (Goal[1]==Goal[2])~c(1,1),
      (Goal[1]<Goal[2])~c(0,3),
      # If the game has not been played yet, nobody wins any points...]
      (is.na(Goal[1]) & is.na(Goal[2]))~c(NA_real_,NA_real_)
    ),
    rel_strength=c(total_points[1]/(total_points[1]+total_points[2]),total_points[2]/(total_points[1]+total_points[2]))
  ) %>% ungroup()



### fine here , idris




# Creates the "form" variable --- based on the proportion of points won in the last 3 games (weighted by the relative strength)
footie=footie %>% 
  group_by(Team) %>% 
  mutate(form=lag(zoo::rollsumr(points_won/rel_strength,3,fill=NA))) %>% 
  mutate(days_since_last=as.numeric(date-lag(date))) %>% ungroup() %>% 
  group_by(ID_game) %>% mutate(
    Opponent=c(Team[2],Team[1])
  ) %>% ungroup() %>% 
  select(ID_game,date,Team,Goal,Opponent,tournament,country,Home,form,diff_point,diff_rank,days_since_last,everything()) 




### fine here , idris


# # Creates the "form" variable --- based on the proportion of points won in the last 3 games
# make.form = function(x) {
#   if(length(x)>=4) {
#     f=numeric()
#     f[1]=runif(1)                 # starts with random form
#     f[2]=x[1]                     # at game 2 only depends on the first game (so max points=3)
#     f[3]=sum(x[1:2])              # at game 3 only depends on games 1,2 (so max points=6)
#     for (i in 4:length(x)) {      # then depends on the past 3 games
#       f[i]=sum(x[(i-3):(i-1)])
#     }
#   }
#   else {
#     f=0
#   }
#   return(f)
# }
# footie=footie %>% 
#   group_by(Team) %>% 
#   mutate(form=make.form(points_won/rel_strength),form2=make.form(points_won)) %>% 
#   # Create days since last game + filter only from 2010 onward
#   mutate(
#     days_since_last=as.numeric(date-lag(date))
#   ) %>% ungroup() %>% 
#   group_by(ID_game) %>% mutate(
#     Opponent=c(Team[2],Team[1])
#   ) %>% 
#   select(ID_game,date,Team,Goal,Opponent,tournament,country,Home,form,diff_point,diff_rank,days_since_last,everything())

## Only keeps data (games) with data on form 

footie=footie %>% filter(!is.na(form)) %>% group_by(ID_game) %>% filter(n()>1) %>% ungroup() 

#### the line above is where the last 48 rows (rounds 2 and 3 of the footie df are)

# Filters off only data from 2010
### Should only the data from UEFA-specific competitions??
# footie=footie %>% filter(date>="2010-01-01") %>% 
#   group_by(ID_game) %>% mutate(ID=cur_group_id()) %>% ungroup() %>% 
#   select(ID,date,Team,Goal,Opponent,tournament,country,Home,form,diff_point,diff_rank,days_since_last,everything()) %>% 
#   mutate(
#     tournament=case_when(
#       tournament%in%c("Baltic Cup","Cyprus International Tournament","Friendly","King's Cup","Kirin Cup","Nations Cup")~"Friendly",
#       tournament%in%c("Confederations Cup","UEFA Nations League")~"Semi-official",
#       tournament%in%c("FIFA World Cup qualification","UEFA Euro qualification")~"Qualifier",
#       tournament%in%c("FIFA World Cup","UEFA Euro","African Cup of Nations")~"Official",
#       TRUE~tournament
#     )
#   )

footie=footie %>% filter(date>="2010-01-01") %>% 
   group_by(ID_game) %>% mutate(ID=cur_group_id()) %>% ungroup() %>% 
   select(ID,date,Team,Goal,Opponent,tournament,country,Home,form,diff_point,diff_rank,days_since_last,everything()) %>% 
   mutate(
      tournament=case_when(
         grepl("qualification",tournament)~"Qualifiers",
         tournament %in% c(
            "FIFA World Cup","UEFA Euro","African Cup of Nations","Copa América",
            "AFF Championship","AFC Asian Cup"
         )~"Official",
         (grepl("Nations League",tournament)|tournament=="Confederations Cup")~"Semi-official",
         TRUE~"Friendy"
      )
   )


saveRDS(footie,"IdrisEuro20.rds")

# ANOVA-like model
# formula = Goal ~ Home + form + diff_point + diff_rank + days_since_last + as.factor(tournament) +
#  as.factor(confederation) +
  f(factor(Team), model = "iid") + 
  f(factor(Opponent), model = "iid") +
  # Time component wek by team to account for difference in time performance
  f(id_date,model="rw2",replicate=as.numeric(factor(Team)))
  #+
  # Overdispersion to account for extra goals
  #f(num,model="iid")
# Model for Round r
r=1
data=
  # Here "fixes" the data
  footie %>% 
    ####
    # Makes sure there aren't too many teams/data to process
    filter(date>="2018-01-01") %>% group_by(ID_game) %>% filter(n()>1) %>% 
    mutate(ID=cur_group_id()) %>% ungroup() %>% 
    ####
    arrange(date) %>% mutate(
      # Recode form for San Marino, who are *always* on 0 points...
      form=case_when((is.nan(form)|is.infinite(form))~0,TRUE~form),
      # And scales all the continuous covariates for ease of fitting
      diff_point=scale(diff_point,scale=TRUE),
      diff_rank=scale(diff_rank,scale=TRUE),
      days_since_last=scale(days_since_last,scale=TRUE),
      num=row_number(),
      id_date=date %>% as.factor() %>% as.numeric()
    ) %>% 
    # Then filters only the games in a given round (for prediction)
    filter(Round.Number%in%c(NA,r))
m=inla(formula,
          data=data,
          family="poisson",
          control.predictor=list(compute=TRUE,link=1),
          control.compute=list(config=TRUE,dic=TRUE))
summary(m)


# For the RW2 by team model
time_trend=function(m,date_start="2010-01-01",date_end="2021-06-11") {
  ii=footie %>% mutate(Team2=Team,Team=factor(Team)) %>% mutate(Team=as.numeric(Team)) %>% 
    filter(date>=date_end) %>% select(ID,date,Team2,Team,everything()) %>% with(unique(Team))
  labs=(footie %>% with(levels(as.factor(Team))))[sort(ii)]
  m$summary.random$id_date %>% as_tibble() %>% 
    mutate(TeamID=rep(1:(footie %>% with(length(unique(Team)))),each=length(unique(m$summary.random$id_date$ID))),
           Team=rep(levels(as.factor(footie$Team)),each=length(unique(m$summary.random$id_date$ID)))) %>% 
    filter(TeamID %in% ii) %>% ggplot(aes(ID,mean,group=Team))+geom_line()+geom_ribbon(aes(ymin=`0.025quant`,ymax=`0.975quant`),alpha=.2) +
    facet_wrap(~Team) +
    scale_x_continuous(
      name="Time",
      breaks=footie %>% filter(date>=date_start) %>% arrange(ID) %>% mutate(tt=lubridate::year(date),id_date=date %>% as.factor %>% as.numeric()) %>% 
        select(ID,date,tt,id_date,everything()) %>% group_by(tt) %>% slice(1) %>% pull(id_date),
      labels=lubridate::year(date_start):lubridate::year(date_end)
    ) + theme(axis.text.x=element_text(angle=60, hjust=1)) + ylab("Historical performance")
}

attack_defense=function(m) {
  options(dplyr.show_progress = FALSE)
  ii=footie %>% mutate(Team2=Team,Team=factor(Team)) %>% mutate(Team=as.numeric(Team)) %>% 
    filter(date>="2021-06-11") %>% select(ID,date,Team2,Team,everything()) %>% with(unique(Team))
  labs=(footie %>% with(levels(as.factor(Team))))[sort(ii)]
  (m$summary.random$`factor(Team)` %>% as_tibble() %>% filter(ID%in%labs) %>% mutate(attack=mean)) %>% 
    bind_cols((m$summary.random$`factor(Opponent)` %>% as_tibble() %>% filter(ID%in%labs) %>% mutate(defense=mean))) %>% 
    select(Team=`ID...1`,attack,defense) %>% ggplot(aes(defense,attack,label=Team))+geom_text() + 
    geom_vline(xintercept=0,linetype="dashed",size=.6) + geom_hline(yintercept=0,linetype="dashed",size=.6)
}

team_strength=function(m,effect="attack") {
  options(dplyr.show_progress = FALSE)
  ii=footie %>% mutate(Team2=Team,Team=factor(Team)) %>% mutate(Team=as.numeric(Team)) %>% 
    filter(date>="2021-06-11") %>% select(ID,date,Team2,Team,everything()) %>% with(unique(Team))
  labs=(footie %>% with(levels(as.factor(Team))))[sort(ii)]
  
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


# Post-processing
make_scored=function(round,model,nsims=1000) {
  r=round
  m=model
  # Then selects the relevant indices
  idx=(data %>% mutate(form=case_when((is.nan(form)|is.infinite(form))~0,TRUE~form)) %>% 
         filter(Round.Number%in%c(NA,r))) %>% mutate(num=row_number()) %>% filter(Round.Number==r) %>% pull(num)
  jpost=inla.posterior.sample(n=nsims,m)
  topredict=tail(grep("Predictor",rownames(jpost[[1]]$latent)),length(idx))
  theta.pred=matrix(exp(unlist(lapply(jpost,function(x) x$latent[idx,]))),ncol=length(idx),byrow=T) 
  colnames(theta.pred)= (data %>% mutate(form=case_when((is.nan(form)|is.infinite(form))~0,TRUE~form)) %>%
                           filter(Round.Number%in%c(NA,r))) %>% mutate(num=row_number()) %>% filter(Round.Number==r) %>% pull(Team)
  theta.pred=theta.pred %>% as_tibble()
  # Predictions from the posterior distribution for the number of goals scored
  scored=theta.pred %>% mutate(across(everything(),~rpois(nrow(theta.pred),.)))
}


# Update observed results
results=tibble(
  Goal=c(
    # Round 1 of the Group stage
    c(3,0,1,1,0,1,3,0,0,1,3,1,3,2,2,0,1,2,0,0,0,3,1,0),
    # Round 2 of the Group stage
    c(0,1,0,2,3,0,1,2,2,1,0,2,0,1,1,1,0,0,1,1,4,2,1,1),
    # Round 3 of the Group stage
    c(1,0,3,1,1,0,3,0,2,0,4,1,0,1,3,1,2,3,0,5,2,2,2,2)
  )
)
# Now merges the new results into the main dataset
footie[footie$date>="2021-06-11","Goal"]=results

# Then updates the form variable and the points won that is used to compute that!
footie=footie %>% group_by(ID) %>% mutate(
  points_won=case_when(
    (Goal[1]>Goal[2])~c(3,0),
    (Goal[1]==Goal[2])~c(1,1),
    (Goal[1]<Goal[2])~c(0,3),
    # If the game has not been played yet, nobody wins any points...]
    (is.na(Goal[1]) & is.na(Goal[2]))~c(NA_real_,NA_real_)
  )
) %>% ungroup() %>% group_by(Team) %>% mutate(
  form=case_when(
#    is.na(form)~make.form(points_won/rel_strength),
    is.na(form)~lag(zoo::rollsumr(points_won/rel_strength,3,fill=NA)),
    TRUE~form
  )
) %>% ungroup()
#%>% filter(date>="2021-06-11") %>% select(ID,date,Team,Goal,form,points_won)

## Add knockout stage
knockout=tibble(
  ID=rep(max(footie$ID):(max(footie$ID)+14),each=2),
  date=c(
    # Round of 16
    rep("2021-06-26",4),rep("2021-06-27",4),rep("2021-06-28",4),rep("2021-06-29",4),
    # Quarter-finals
    rep("2021-07-03",8),
    # Semi-finals
    rep("2021-07-06",2),rep("2021-07-07",2),
    # Final
    rep("2021-07-11",2)
  ),
  Team=c(
    # Round of 16
    "Wales","Denmark",
    "Italy","Austria",
    "Netherlands","Czech Republic",
    "Belgium","Portugal",
    "Croatia","Spain",
    "France","Switzerland",
    "England","Germany",
    "Sweden","Ukraine",
    # Quarter-finals
    "Switzerland","Spain",
    "Belgium","Italy",
    "Czech Republic","Denmark",
    "Ukraine","England",
    # Semi-finals
    "Italy","Spain","England","Denmark",
    # Final
    "Italy","England"
  ),
  Goal=c(
    # Round of 16
    c(0,4,2,1,0,2,1,0,3,5,3,3,2,0,1,2),
    # Quarter-finals
    c(1,1,1,2,1,2,0,4),
    # Semi-finals
    1,1,2,1,
    # Final
    rep(NA,2)
  ),
  Opponent=c(
    # Round of 16
    "Denmark","Wales",
    "Austria","Italy",
    "Czech Republic","Netherlands",
    "Portugal","Belgium",
    "Spain","Croatia",
    "Switzerland","France",
    "Germany","England",
    "Ukraine","Sweden",
    # Quarter-finals
    "Spain","Switzerland",
    "Italy","Belgium",
    "Denmark","Czech Republic",
    "England","Ukraine",
    # Semi-finals
    "Spain","Italy","Denmark","England",
    # Final
    "England","Italy"
  ),
  tournament=rep("Official",30),
  country=c(
    # Round of 16
    rep("Netherlands",2),rep("England",2),rep("Hungary",2),rep("Spain",2),rep("Denmark",2),rep("Romania",2),rep("England",2),rep("Scotland",2),
    # Quarter-finals
    rep("Russia",2),rep("Germany",2),rep("Azerbaijan",2),rep("Italy",2),
    # Semi-finals
    rep("England",4),
    # Final
    rep("England",2)
  ),
  Home=if_else(Team==country,1,0),
  form=rep(NA,30),
  diff_point=rep(NA,30),
  diff_rank=rep(NA,30),days_since_last=rep(NA,30),
  ID_game=rep(max(footie$ID_game):(max(footie$ID_game)+14),each=2),
  city=c(
    rep("Amsterdam",2),rep("London",2),rep("Budapest",2),rep("Sevilla",2),rep("Copenhagen",2),rep("Bucharest",2),rep("London",2),rep("Glasgow",2),
    rep("St Petersburg",2),rep("Munich",2),rep("Baku",2),rep("Rome",2),
    rep("London",4),
    rep("London",2)
  ),
  neutral=if_else(Team==country,TRUE,FALSE),
  Round.Number=c(
    rep("4",16),rep("5",8),rep("6",4),rep("7",2)
  ),
  points_won=rep(NA,30),
  rel_strength=rep(NA,30),
  form2=rep(NA,30)
)
knockout$date=lubridate::as_date(knockout$date)
# Now need to match with ranking + create other variables (form, days_since_last, etc)
knockout=knockout %>% left_join(ranking,by=c("Team"="country_full")) %>% 
  select(-c(id,country_abrv,rank_change)) %>% group_by(ID_game,Team) %>% 
  filter(date>=rank_date,confederation=="UEFA") %>% slice(n()) %>% ungroup() 
knockout=knockout %>% group_by(ID_game) %>% 
  mutate(
    diff_point=c(total_points[1]-total_points[2],total_points[2]-total_points[1]),
    diff_rank=c(rank[2]-rank[1],rank[1]-rank[2]),
    points_won=case_when(
      (Goal[1]>Goal[2])~c(3,0),
      (Goal[1]==Goal[2])~c(1,1),
      (Goal[1]<Goal[2])~c(0,3),
      # If the game has not been played yet, nobody wins any points...]
      (is.na(Goal[1]) & is.na(Goal[2]))~c(NA_real_,NA_real_)
    ),
    rel_strength=c(total_points[1]/(total_points[1]+total_points[2]),total_points[2]/(total_points[1]+total_points[2]))
  ) %>% ungroup()

# Now adds the new games to the main dataset
footie=footie %>% bind_rows(knockout)
# Creates form and days since last game to use as covariates in the updated analysis
footie=footie %>% group_by(Team) %>% mutate(
  form=case_when(
    #is.na(form)~make.form(points_won/rel_strength),
    is.na(form)~lag(zoo::rollsumr(points_won/rel_strength,3,fill=NA)),
    TRUE~form
  ),
  days_since_last=case_when(
    is.na(days_since_last)~as.numeric(date-lag(date)),
    TRUE~days_since_last
  )
) %>% ungroup()



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
      scale_fill_gradient(low="white", high="black")+theme_gb()+
      xlab(x) + ylab(y) +
      labs(fill="",title=title)+
      # Removes the legend (don't really need it if the probability values are displayed in the tiles...)
      theme(legend.position = "none") +
      geom_abline(slope=1,intercept=0,size=.1)+scale_color_manual(guide=FALSE,values=c("white","red"))
  } else {
    pl=pl+geom_text(aes(label=paste0(format(100*n,digits=2,nsmall=2),"%")),color="white",size=annotate_size,fontface="bold") +
      scale_fill_gradient(low="white", high="black")+theme_gb()+
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


predict_pass=function(data,nsim=1000) {
  euro2021=data %>% filter(date>="2021-06-11") %>% select(ID,date,Team,Goal,points_won)
  preds=t(scored) %>% as_tibble() %>% mutate(Team=names(scored)) %>% select(Team,everything())
  round3=euro2021 %>% filter(date>="2021-06-20") %>% left_join(preds,by=c("Team"="Team"))
  
  make_table=function(x) {
    options(dplyr.summarise.inform = FALSE)
    
    col=paste0("V",x)
    euro2021 %>% filter(date<"2021-06-20") %>% bind_rows(round3) %>% mutate(
      Group=case_when(
        Team%in%c("Italy","Switzerland","Turkey","Wales")~"A",
        Team%in%c("Denmark","Finland","Belgium","Russia")~"B",
        Team%in%c("Netherlands","Austria","Ukraine","North Macedonia")~"C",
        Team%in%c("England","Croatia","Scotland","Czech Republic")~"D",
        Team%in%c("Spain","Slovakia","Poland","Sweden")~"E",
        Team%in%c("France","Germany","Portugal","Hungary")~"F"
      )
    ) %>% mutate(Goal=case_when(is.na(Goal)~!!sym(col),TRUE~Goal)) %>% 
      group_by(ID) %>% mutate(
        GF=col,GA=c(Goal[2],Goal[1]),
        points_won=case_when(
          (Goal[1]>Goal[2])~c(3,0),
          (Goal[1]==Goal[2])~c(1,1),
          (Goal[1]<Goal[2])~c(0,3),
        )
      ) %>% ungroup() %>% 
      select(ID,date,Group,Team,points_won,Goal,GA,everything()) %>% 
      group_by(Group,Team) %>% summarise(
        pts=sum(points_won,na.rm=T),
        GF=sum(Goal,na.rm=T),
        GA=sum(GA,na.rm=T)
      ) %>% 
      ungroup() %>% arrange(Group,desc(pts),GF,desc(GA)) %>% mutate(sim=x)
  }
  
  groups=lapply(1:nsim,function(x) make_table(x))
  return(groups)
}

groups %>% bind_rows() %>% filter(Group=="D") %>% group_by(sim) %>% 
  arrange(desc(pts),GF,desc(GA)) %>% mutate(rank=row_number()) %>% ungroup() %>% 
  arrange(sim,desc(pts),GF,desc(GA)) %>% group_by(Team) %>% summarise(pos=table(rank) %>% prop.table) %>%
  ungroup() %>% mutate(n=names(pos)) %>% arrange(Team,n) %>% 
  ggplot(aes(Team,pos))+geom_bar(aes(fill=as.factor(n)),position="dodge",stat="identity") +
  labs(x="Team",y="Probability",fill="Position")


# All plots in a single grob
library(gridExtra)

h=seq(1,ncol(scored),by=2)
a=seq(2,ncol(scored),by=2)
allplot=lapply(
  1:length(h),function(x) 
    joint_marginal(names(scored)[h[x]],names(scored)[a[x]],scored,max_goal=6,annotate_size=3.2,title="")
)


# Plot of form variable over time
footie %>% filter(date>="2021-06-11") %>% 
  ggplot(aes(Round.Number,form,group=as.factor(Team),color=as.factor(Team)))+
  geom_line()+ggrepel::geom_label_repel(aes(label=as.factor(Team)))+
  theme(legend.position="none")+xlab("Game")+ylab("Form")

