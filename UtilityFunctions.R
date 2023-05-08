
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
make_scored=function(round,dt,model,nsims=1000){
  r=round
  m=model
  # Then selects the relevant indices
  idx=(dt %>%  mutate(form=case_when((is.nan(form)|is.infinite(form))~0,TRUE~form)) %>% 
         filter(Round.Number%in%c(NA,r))) %>% mutate(num=row_number()) %>% filter(Round.Number==r) %>% pull(num)
  jpost=inla.posterior.sample(n=nsims,m)
  topredict=tail(grep("Predictor",rownames(jpost[[1]]$latent)),length(idx))
  
  theta.pred=matrix(exp(unlist(lapply(jpost,function(x) x$latent[idx,]))),ncol=length(idx),byrow=T) 
  
  colnames(theta.pred)= (dt %>%  mutate(form=case_when((is.nan(form)|is.infinite(form))~0,TRUE~form)) %>%
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


##### joint marginal is fine when working with all top 5 leagues

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


########## mpo_updated_unplayed if its using the most probable joint outcome #########

mpo_updated_unplayed = function(N,roundata, scored) {

  gwNu = roundata %>% filter(is.na(Goal) & (Round.Number == N))
  
  for(i in 1:nrow(gwNu)) {
    # Extract the home and away teams for the i-th row
    gwNteam = as.character(gwNu[i, "Team"])
    gwNopponent = as.character(gwNu[i, "Opponent"])
    
    extractdata = scored %>% with(table(.[[gwNteam]],.[[gwNopponent]])) %>% prop.table() %>%
          as_tibble(.name_repair = ~vctrs::vec_as_names(c(gwNteam,gwNopponent,"n"),quiet=TRUE)) %>%
          mutate(across(where(is.character),as.numeric))

    max_row_index = which.max(extractdata$n)
    max_row = extractdata[max_row_index, ]
    
    # Extract the predicted goals for the home and away teams from the most likely result "max_row"
    team_goals = max_row[1]
    # away_goals = max_row[2] turns out this not needed since we are doing specific team rows in order
         
    gwNu$Goal[i] = as.numeric(team_goals)
    
    
  }
  
  return(gwNu %>% select(ID_game, Team, Goal, Opponent,num))
}

########## updated_unplayed if its using sample mean #########

updated_unplayed = function(N,roundata, scored) {
  
  gwNu = roundata %>% filter(is.na(Goal) & (Round.Number == N))
  # create a vector of team names from the scored dataframe
  scored_names = colnames(scored)
  
  for(i in 1:nrow(gwNu)) {
    # Extract the home and away teams for the i-th row
    gwNteam = gwNu[i, "Team"]
    
    # Calculate the mean of the scored tibble for the given team
    team_scored_col = scored[[as.character(gwNteam)]]
    team_goals = mean(team_scored_col)
    gwNu$Goal[i] = round(team_goals)
  }
  
  return(gwNu %>% select(ID_game, Team, Goal, Opponent,num))
}


# # Updating the Goal column in premfootie dataframe
# premfootie_updated = premfootie %>%
#   mutate(Goal = ifelse(ID_game %in% gw28U$ID_game, gw28U$Goal[match(ID_game, gw28U$ID_game)], Goal))


# Define the update_footie function
update_footie = function(gwU, footie) {
  footie_updated = footie %>%
    mutate(Goal = case_when(
      num %in% gwU$num ~ gwU$Goal[match(num, gwU$num)],
      TRUE ~ Goal
    ))
  return(footie_updated)
}

# Update the premfootie dataframe
# premfootie = update_footie(gw28U, premfootie)



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
  pf = pf %>%
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
    group_by(game_number) %>%
    mutate(rank = min_rank(desc(total_points)) +
             row_number(desc(total_GD)) / 10000) %>%
    mutate(rank = dense_rank(rank)) %>%
    ungroup() %>% 
    group_by(ID_game) %>%
    mutate(diff_rank = c(rank[1] - rank[2], rank[2] - rank[1])) %>%
    ungroup()
  
  
  return(pf)
}


#rN 

roundN = function(N,scored,roundata,prevfootie){
  gwNU = updated_unplayed(N,roundata,scored)
  footiep1 = update_footie(gwNU, prevfootie)
  footiep2 = var_updater(footiep1)
  footiep2
}


#roundNmpo
roundNmpo = function(N,scored,roundata,prevfootie){
  gwNU = mpo_updated_unplayed(N,roundata,scored)
  footiep1 = update_footie(gwNU, prevfootie)
  footiep2 = var_updater(footiep1)
  footiep2
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



runINLA = function(formu,dat){
  inmod=inla(formula=formu,
             data=dat,
             family="poisson",
             control.predictor=list(compute=TRUE,link=1),
             control.compute=list(config=TRUE,dic=TRUE))
  inmod
}



######## Top 5 League Functions ######

t5datprep = function(pf,r){
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






t5attack_defense=function(m,pf) {
  options(dplyr.show_progress = FALSE)
  ii=pf %>% mutate(Team2=Team,Team=factor(Team)) %>% mutate(Team=as.numeric(Team)) %>% 
    filter(date>="2021-06-11") %>% select(ID,date,Team2,Team,everything()) %>% with(unique(Team))
  labs=(pf %>% with(levels(as.factor(Team))))[sort(ii)]
  (m$summary.random$`factor(Team)` %>% as_tibble() %>% filter(ID%in%labs) %>% mutate(attack=mean)) %>% 
    bind_cols((m$summary.random$`factor(Opponent)` %>% as_tibble() %>% filter(ID%in%labs) %>% mutate(defense=mean))) %>% 
    select(Team=`ID...1`,attack,defense) %>% ggplot(aes(defense,attack,label=Team))+geom_text() + 
    geom_vline(xintercept=0,linetype="dashed",size=.6) + geom_hline(yintercept=0,linetype="dashed",size=.6)
}

t5team_strength=function(m,pf,effect="attack") {
  options(dplyr.show_progress = FALSE)
  ii= pf %>% mutate(Team2=Team,Team=factor(Team)) %>% mutate(Team=as.numeric(Team)) %>% 
    filter(date>="2021-06-11") %>% select(ID,date,Team2,Team,everything()) %>% with(unique(Team))
  labs=(pf %>% with(levels(as.factor(Team))))[sort(ii)]
  
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


t5make_scored=function(round,dt,model,nsims=1000) {
  r=round
  m=model
  # Then selects the relevant indices
  idx=(dt %>%  mutate(form=case_when((is.nan(form)|is.infinite(form))~0,TRUE~form)) %>% 
         filter(Round.Number%in%c(NA,r))) %>% mutate(num=row_number()) %>% filter(Round.Number==r) %>% pull(num)
  jpost=inla.posterior.sample(n=nsims,m)
  topredict=tail(grep("Predictor",rownames(jpost[[1]]$latent)),length(idx))
  theta.pred=matrix(exp(unlist(lapply(jpost,function(x) x$latent[idx,]))),ncol=length(idx),byrow=T) 
  colnames(theta.pred)= (dt %>%  mutate(form=case_when((is.nan(form)|is.infinite(form))~0,TRUE~form)) %>%
                           filter(Round.Number%in%c(NA,r))) %>% mutate(num=row_number()) %>% filter(Round.Number==r) %>% pull(Team)
  theta.pred=theta.pred %>% as_tibble()
  # Predictions from the posterior distribution for the number of goals scored
  scored=theta.pred %>% mutate(across(everything(),~rpois(nrow(theta.pred),.)))
}
  
  ##### Ninla #####
  
roundNinla = function(n,prevfootie,frm){
    df = datprep(prevfootie,n)
    
    
    inlam = runINLA(formu  =  frm, dat=df)
    
    set.seed(12345)
    
    rN = make_scored(round=n,dt=df, model = inlam,nsims=100)
    
    footieN = roundN(N=n,scored=rN,roundata = df , prevfootie=prevfootie)
    
    list(footieN = footieN, rN = rN, inlam = inlam)
  }


roundNinlampo = function(n,prevfootie,frm){
  df = datprep(prevfootie,n)
  
  
  inlam = runINLA(formu = frm , dat=df)
  
  set.seed(12345)
  
  rN = make_scored(round=n,dt=df, model = inlam,nsims=100)
  
  footieN = roundNmpo(N=n,scored=rN,roundata = df , prevfootie=prevfootie)
  
  list(footieN = footieN, rN = rN, inlam = inlam)
}


###### trying to do t5time_trend ######

# For the RW2 by team model
t5time_trend=function(m,pf,date_start="2022-08-01",date_end="2023-06-11") {
  date_start = as_date(date_start)
  date_end = as_date(date_end)
  ii=pf %>% mutate(Team2=Team,Team=factor(Team)) %>% mutate(Team=as.numeric(Team)) %>% 
    filter(date <= date_end) %>% select(ID,date,Team2,Team,everything()) %>% with(unique(Team))
  labs=(pf %>% with(levels(as.factor(Team))))[sort(ii)]
  m$summary.random$id_date %>% as_tibble() %>% 
    mutate(TeamID=rep(1:(pf %>% with(length(unique(Team)))),each=length(unique(m$summary.random$id_date$ID))),
           Team=rep(levels(as.factor(pf$Team)),each=length(unique(m$summary.random$id_date$ID)))) %>% 
    filter(TeamID %in% ii) %>% ggplot(aes(ID,mean,group=Team))+geom_line()+geom_ribbon(aes(ymin=`0.025quant`,ymax=`0.975quant`),alpha=.2) +
    facet_wrap(~Team) +
    scale_x_continuous(
      name="Time",
      breaks=pf %>% filter(date>=date_start) %>% arrange(ID) %>% mutate(tt=lubridate::year(date),id_date=date %>% as.factor %>% as.numeric()) %>% 
        select(ID,date,tt,id_date,everything()) %>% group_by(tt) %>% slice(1) %>% pull(id_date),
      labels=lubridate::year(date_start):lubridate::year(date_end)
    ) + theme(axis.text.x=element_text(angle=60, hjust=1)) + ylab("Historical performance")
}



###### table maker #####

t5tablerounrR = function(R,pf){
  round = pf %>% filter( Round.Number == R)
  
  tab <- round %>%
    arrange(desc(total_points), desc(total_GD), desc(total_G)) %>%
    ungroup() %>%        # had to add ungroup() here because it automatically groups by ID_game
    mutate(Position = row_number()) %>%
    select(Position,Team,total_points, total_G, total_GC, total_GD)
  
  tab
}





######## if we are doing multple seasons ######

