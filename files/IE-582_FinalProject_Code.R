library(pracma)
library (data.table)
library(caret)
library(methods)
library(caTools)
require(data.table)
require(TunePareto)
require(glmnet)
library(base)


firstdayofweek<-as.Date("2018-12-29") # it is important to create fixture and season before predicted matches
lastdayofweek<-as.Date("2018-12-30")


#data import

dt_odds<- readRDS("~/Documents/Dersler/Fall-2018/IE-582/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds")
dt_matches <- readRDS("~/Documents/Dersler/Fall-2018/IE-582/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")


## Odd Preprocessing

dt_odds<-dt_odds[betType=="1x2"& date>1406913631]
dt_odds<-dt_odds[,.(matchId,bookmaker,oddtype,date,odd)]
dt_odds[,rnk:=rank(-date,ties.method= "min"),by = list(matchId,oddtype,bookmaker)]
dt_oddsl<-dt_odds[rnk==1] ## Final odds by bookmakers
dt_odds_pvt <- dcast(dt_oddsl[,.(matchId,bookmaker,oddtype,odd)],matchId+bookmaker ~oddtype) # pivot table gives match,odd and bookmaker 
dt_odds_pvt$homeprob <-as.numeric((1/dt_odds_pvt$odd1)*(1/((1/dt_odds_pvt$odd1)+(1/dt_odds_pvt$odd2)+(1/dt_odds_pvt$oddX))))
dt_odds_pvt$awayprob <-as.numeric((1/dt_odds_pvt$odd2)*(1/((1/dt_odds_pvt$odd1)+(1/dt_odds_pvt$odd2)+(1/dt_odds_pvt$oddX))))
dt_odds_pvt$drawprob <-as.numeric((1/dt_odds_pvt$oddX)*(1/((1/dt_odds_pvt$odd1)+(1/dt_odds_pvt$odd2)+(1/dt_odds_pvt$oddX))))


## Match Dataprocessing 


dt_matches<-dt_matches[date>1406913631] # 1 A??utos 2017
dt_matches <- dt_matches[,date:=as.Date(as.POSIXct(date,origin="1970-01-01",tz = "UTC",tryformats="%Y/%m/%d"))]
dt_matches<-dt_matches[date<=lastdayofweek]
dt_matches[date>=firstdayofweek,c("score")]=NA ## remove result tı simulate t 
dt_matches[,c("HomeGoal","AwayGoal"):= tstrsplit(score,":",fixed=TRUE)]
dt_matches[,`:=`(MatchResult = ifelse(HomeGoal == AwayGoal, "Tie" , ifelse(HomeGoal > AwayGoal, 'Home' , 'Away')))]
dt_matches[,`:=`(ResultHome = ifelse(MatchResult=="Home",1,0)
                 ,ResultTie = ifelse(MatchResult=="Tie",1,0)
                 ,ResultAway = ifelse(MatchResult=="Away",1,0))]


dt_matches<-unique(dt_matches)

# merge odd and matches


setkey(dt_matches,matchId)
setkey(dt_odds_pvt,matchId)
dt_merged <- merge(dt_odds_pvt,unique(dt_matches),all.y =TRUE)
dt_dataset<-dt_merged
dt_dataset<-unique(dt_dataset)


#split seasons, team names are preprocessed to uniqueness
# season1 is current season
dt_season1<-dt_dataset[date>="2018-08-01"& date<=lastdayofweek][bookmaker=="Pinnacle"] 
dt_season2<-dt_dataset[date>="2017-08-01" & date<="2018-06-07"][bookmaker=="Pinnacle"]
dt_season3<-dt_dataset[date>="2016-08-01" & date<="2017-06-07"][bookmaker=="Pinnacle"]
dt_season4<-dt_dataset[date>="2015-08-01" & date<="2016-06-07"][bookmaker=="Pinnacle"]
dt_season5<-dt_dataset[date>="2014-08-01" & date<="2015-06-07"][bookmaker=="Pinnacle"]

dt_season2$home[dt_season2$home %in% "stoke"] <-"stoke city"
dt_season2$away[dt_season2$away %in% "stoke"] <-"stoke city"

dt_season2$home[dt_season2$home %in% "newcastle utd"] <-"newcastle"
dt_season2$away[dt_season2$away %in% "newcastle utd"] <-"newcastle"


dt_season1$home[dt_season1$home %in% "manchester-utd"] <-"manchester-united"
dt_season1$away[dt_season1$away %in% "manchester-utd"] <-"manchester-united"



##functions

#rps
RPS_matrix <- function(probs,outcomes){
  probs=as.matrix(probs)
  outcomes=as.matrix(outcomes)
  probs=t(apply(t(probs), 2, cumsum))
  outcomes=t(apply(t(outcomes), 2, cumsum))
  RPS = apply((probs-outcomes)^2,1,sum) / (ncol(probs)-1)
  return(RPS) 
  
}


performance_analysis<-function(data){
  
  data_rps<-RPS_matrix(data[,.(homeprob,drawprob,awayprob)],data[,.(ResultHome,ResultTie,ResultAway)])
  
  data$rps<-data.frame(data_rps)
  #data$difference<-data$homeprob-data$awayprob
  #dt_performancesummary<-data[,mean(rps),by = bookmaker]
  return(data)#dt_performancesummary)
}


# fixture creation

fixture_create = function(data)
{
  
  data[,`:=`(HomeGrade = ifelse(MatchResult == "Home", 1 , ifelse(MatchResult == "Tie", 0.5 , 0)))]
  data[,`:=`(AwayGrade = ifelse(MatchResult == "Away", 1 , ifelse(MatchResult == "Tie", 0.5 , 0)))]
  
  
  dt_Awaymatches<-data[,.(matchId,date,away,AwayGrade,AwayGoal,HomeGoal,MatchResult,homeprob,drawprob,awayprob,ResultHome,ResultTie,ResultAway)]
  dt_Awaymatches[,HAWeek:=rank(date,ties.method= "min"),by = list(away)]
  dt_Awaymatches$Status=1
  setnames(dt_Awaymatches,old = c("away","AwayGrade","AwayGoal","HomeGoal"),new = c("Team","Grade","SGoal","CGoal"))
  
  #dt_Homematches<-data[,.(matchId,date,home,HomeGrade,HomeGoal,AwayGoal,MatchResult)]
  
  dt_Homematches<-data[,.(matchId,date,home,HomeGrade,HomeGoal,AwayGoal,MatchResult,homeprob,drawprob,awayprob,ResultHome,ResultTie,ResultAway)]
  dt_Homematches[,HAWeek:=rank(date,ties.method= "min"),by = list(home)]
  setnames(dt_Homematches,old = c("home","HomeGrade","HomeGoal","AwayGoal"),new = c("Team","Grade","SGoal","CGoal"))
  
  dt_Homematches$Status=0
  dt_fixture<-rbind(dt_Homematches,dt_Awaymatches)
  dt_fixture<-dt_fixture[order(Team,date)]
  dt_fixture<-dt_fixture[,TotalGrade:=cumsum(Grade), by = list(Team)]
  dt_fixture[,week:=rank(date,ties.method= "min"),by = list(Team)]
  
  oldrps<-RPS_matrix(dt_fixture[,.(homeprob,drawprob,awayprob)],dt_fixture[,.(ResultHome,ResultTie,ResultAway)])
  
  dt_fixture$rps<-data.frame(oldrps)
  
  dt_fixture<-dt_fixture[,`:=`(WinProbratio = ifelse(Status == 0, homeprob/awayprob,awayprob/homeprob))]
  
  
  return(dt_fixture)
}


# feature extract function

feature_extract = function(data){
  
  data[,HomeTotalGrade:=0]
  data[,AwayTotalGrade:=0]
  data[,homeL3SGoal:=0]
  data[,homeL3CGoal:=0]
  data[,awayL3SGoal:=0]
  data[,awayL3CGoal:=0]
  data[,homeL3wincount:=0]
  data[,homeL3defeatcount:=0]
  data[,awayL3wincount:=0]
  data[,awayL3defeatcount:=0]
  data[,homeOldRps:=0]
  data[,awayOldRps:=0]
  data[,week:=0]
  data[,homeWinRatioInHome:=0]
  data[,awayWinRatioInAway:=0]
  data[,homeL5rps:=0]
  data[,awayL5rps:=0]
  data[,homeL5avggrade:=0]
  data[,awayL5avggrade:=0]
  data[,homeinhomeOldrps:=0]
  data[,awayinawayOldrps:=0]
  data[,homeL5WinRatio:=0]
  data[,awayL5WinRatio:=0]
  data[,GradeDifference:=0]
  
  
  for(i in 1:nrow(data))
  {
    matchid=data[i,c("matchId")]
    homeTeam=data[i,c("home")]
    awayTeam=data[i,c("away")]
    Iscurrentwk=is.na(data[i,c("MatchResult")])
    
    
    hwk=as.numeric(fixture[matchId==matchid &Status==0][,c("HAWeek")]) # number of match is played in home
    hwk=ifelse(is.na(Iscurrentwk),hwk-1,hwk) 
    
    homeGrade=fixture[matchId==matchid & Team==homeTeam&Status==0][,c("TotalGrade")]
    
    # two week ago for Last 3 week
    
    hwkp=as.numeric(ifelse(hwk>2,hwk-2,1))
    #hwkp=ifelse(is.na(Iscurrentwk)&hwkp>2,hwk-2,1)
    homeGrade=ifelse(Iscurrentwk,max(na.omit(fixture[Team==homeTeam][,c("TotalGrade")])),homeGrade)
    
    
    awk=as.numeric(fixture[matchId==matchid &Status==1][,c("HAWeek")])
    awayGrade=fixture[matchId==matchid & Team==awayTeam&Status==1][,c("TotalGrade")]
    awkp=as.numeric(ifelse(awk>2,awk-2,awk))
    awkp=ifelse(Iscurrentwk & awkp>2,awkp-3,awkp)
    awayGrade=ifelse(Iscurrentwk,max(na.omit(fixture[Team==awayTeam][,c("TotalGrade")])),awayGrade)
    
    
    #Grades
    data[i,c("HomeTotalGrade")]=homeGrade
    data[i,c("AwayTotalGrade")]=awayGrade
    data[i,c("GradeDifference")]=abs(data[i,c("HomeTotalGrade")]-data[i,c("AwayTotalGrade")])
    
    
    #L3 home away win count
    
    dt_home3week<-fixture[ Team==homeTeam&Status==0][hwkp:hwk,]
    dt_away3week<-fixture[ Team==awayTeam&Status==1][hwkp:hwk,]
    
    homeL3wincount=sum(dt_home3week$Grade==1,na.rm = TRUE)
    homeL3defeatcount=sum(dt_home3week$Grade==0,na.rm = TRUE)
    
    awayL3wincount=sum(dt_away3week$Grade==1,na.rm = TRUE)
    awayL3defeatcount=sum(dt_away3week$Grade==0,na.rm = TRUE)
    
    
    data[i,c("homeL3wincount")]=homeL3wincount
    data[i,c("homeL3defeatcount")]=homeL3defeatcount
    
    data[i,c("awayL3wincount")]=awayL3wincount
    data[i,c("awayL3defeatcount")]=awayL3defeatcount
    
    
    dt_homeL3SGoal=fixture[ Team==homeTeam&Status==0][hwkp:hwk,c("SGoal")]
    homeL3SGoal=mean(as.numeric(dt_homeL3SGoal$SGoal),na.rm = TRUE)
    
    dt_homeL3CGoal=fixture[ Team==homeTeam&Status==0][hwkp:hwk,c("CGoal")]
    homeL3CGoal=mean(as.numeric(dt_homeL3CGoal$CGoal),na.rm = TRUE)
    
    
    data[i,c("homeL3SGoal")]=homeL3SGoal
    data[i,c("homeL3CGoal")]=homeL3CGoal
    
    
    dt_awayL3SGoal=fixture[ Team==awayTeam&Status==1][awkp:awk,c("SGoal")]
    awayL3SGoal=mean(as.numeric(dt_awayL3SGoal$SGoal),na.rm = TRUE)
    
    
    dt_awayL3CGoal=fixture[ Team==awayTeam&Status==1][awkp:awk,c("CGoal")]
    awayL3CGoal=mean(as.numeric(dt_awayL3CGoal$CGoal),na.rm = TRUE)
    
    data[i,c("awayL3SGoal")]=awayL3SGoal
    data[i,c("awayL3CGoal")]=awayL3CGoal
    
    
    #wk
    wk=as.numeric(max(fixture[matchId==matchid &Status==0][,c("week")]))
    wk=ifelse(Iscurrentwk,wk-1,wk)
    wkp5=as.numeric(ifelse(wk>5,wk-5,1))
    
    
    #l5grade
    
    
    homeL5avggrade=as.numeric(mean(as.matrix(fixture[Team==homeTeam][wkp5:wk,c("Grade")])))
    
    data[i,c("homeL5avggrade")]=homeL5avggrade
    
    awayL5avggrade=as.numeric(mean(as.matrix(fixture[Team==awayTeam][wkp5:wk,c("Grade")])))
    
    data[i,c("awayL5avggrade")]=awayL5avggrade
    
    
    
    #week
    week=as.numeric(max(fixture[matchId==matchid][,c("week")]))
    data[i,c("week")]=week
    
    
    #WinRatio, bookmaker's old probability for team
    homeWinRatioInHome<-median(fixture[Team==homeTeam&Status==0]$WinProbratio,na.rm = TRUE)
    awayWinRatioInAway<-median(fixture[Team==awayTeam&Status==1]$WinProbratio,na.rm = TRUE)
    
    homeL5WinRatio=as.numeric(mean(fixture[Team==homeTeam][wkp5:wk,c("WinProbratio")]$WinProbratio))
    awayL5WinRatio=as.numeric(mean(fixture[Team==awayTeam][wkp5:wk,c("WinProbratio")]$WinProbratio))
    
    data[i,c("homeWinRatioInHome")]<-homeWinRatioInHome
    data[i,c("awayWinRatioInAway")]<-awayWinRatioInAway  
    
    
    data[i,c("homeL5WinRatio")]=homeL5WinRatio
    data[i,c("awayL5WinRatio")]=awayL5WinRatio
    
    
    
    #rps
    
    
    dt_homeOldRps=fixture[ Team==homeTeam][1:hwk,c("rps")]
    #dt_homeOldRps=fixture[ Team=="bournemouth"][1:5,c("rps")]
    homeOldRps=median(as.numeric(dt_homeOldRps$rps))
    
    dt_home5OldRps=fixture[ Team==homeTeam][wkp5:wk,c("rps")]
    #dt_homeOldRps=fixture[ Team=="bournemouth"][1:5,c("rps")]
    home5OldRps=median(as.numeric(dt_home5OldRps$rps))
    
    homeinhomeOldrps=median(fixture[Team==homeTeam&Status==0][1:hwk,c("rps")]$rps,na.rm = TRUE)
    
    dt_away5OldRps=fixture[ Team==awayTeam][wkp5:wk,c("rps")]
    away5OldRps=mean(as.numeric(dt_away5OldRps$rps))
    
    dt_awayOldRps=fixture[ Team==awayTeam][1:awk,c("rps")]
    #dt_homeOldRps=fixture[ Team=="bournemouth"][1:5,c("rps")]
    awayOldRps=mean(as.numeric(dt_awayOldRps$rps))
    
    
    awayinawayOldrps=median(fixture[ Team==awayTeam&Status==1][1:hwk,c("rps")]$rps,na.rm = TRUE)
    
    
    data[i,c("homeOldRps")]=homeOldRps
    data[i,c("awayOldRps")]=awayOldRps
    
    
    data[i,c("homeL5rps")]=home5OldRps
    data[i,c("awayL5rps")]=away5OldRps
    
    data[i,c("homeinhomeOldrps")]=homeinhomeOldrps
    data[i,c("awayinawayOldrps")]=awayinawayOldrps
    
    
  }
  
  #special case
  
  data<-data[,`:=`(Is.special = ifelse(((homeWinRatioInHome>awayWinRatioInAway & homeprob<awayprob) | (homeWinRatioInHome<awayWinRatioInAway & homeprob>awayprob)),1,0),
                   Is.normal = ifelse(((homeWinRatioInHome>awayWinRatioInAway & homeprob<awayprob) | (homeWinRatioInHome<awayWinRatioInAway & homeprob>awayprob)),0,1))]
  
  
  return(data)
}


# model
train_glmnet <- function(train_features, test_features,not_included_feature_indices=c(1:5), alpha=1,nlambda=10, tune_lambda=TRUE,nofReplications=2,nFolds=10,trace=T){
  
  set.seed(1)
  
  # glmnet works with complete data
  glm_features=train_features[complete.cases(train_features)]
  train_class=glm_features$MatchResult
  glm_train_data=glm_features[,-not_included_feature_indices,with=F]
  glm_test_data=test_features[,-not_included_feature_indices,with=F]
  if(tune_lambda){
    # to set lambda parameter, cross-validation will be performed and lambda is selected based on RPS performance
    
    cvindices=generateCVRuns(train_class,nofReplications,nFolds,stratified=TRUE)
    
    # first get lambda sequence for all data
    glmnet_alldata = glmnet(as.matrix(glm_train_data), as.factor(train_class), family="multinomial", alpha = alpha, nlambda=nlambda)
    lambda_sequence = glmnet_alldata$lambda
    
    cvresult=vector('list',nofReplications*nFolds)
    iter=1
    for(i in 1:nofReplications) {
      thisReplication=cvindices[[i]]
      for(j in 1:nFolds){
        if(trace){
          cat(sprintf('Iteration %d: Fold %d of Replication %d\n',iter,j,i))
        }
        testindices=order(thisReplication[[j]])
        
        cvtrain=glm_train_data[-testindices]    
        cvtrainclass=train_class[-testindices]   
        cvtest=glm_train_data[testindices]
        cvtestclass=train_class[testindices] 
        
        inner_cv_glmnet_fit = glmnet(as.matrix(cvtrain),as.factor(cvtrainclass),family="multinomial", alpha = alpha,lambda=lambda_sequence)
        valid_pred = predict(inner_cv_glmnet_fit, as.matrix(cvtest), s = lambda_sequence, type = "response")
        
        #check order of predictions
        order_of_class=attr(valid_pred,'dimnames')[[2]]
        new_order=c(which(order_of_class=='Home'),which(order_of_class=='Tie'),which(order_of_class=='Away'))
        foldresult=rbindlist(lapply(c(1:length(lambda_sequence)),function(x) { data.table(repl=i,fold=j,lambda=lambda_sequence[x],valid_pred[,new_order,x],result=cvtestclass)}))
        cvresult[[iter]]=foldresult
        iter=iter+1
      }
    }
    
    cvresult=rbindlist(cvresult)
    
    
    # creating actual targets for rps calculations
    cvresult[,pred_id:=1:.N]
    outcome_for_rps=data.table::dcast(cvresult,pred_id~result,value.var='pred_id')
    outcome_for_rps[,pred_id:=NULL]
    outcome_for_rps[is.na(outcome_for_rps)]=0
    outcome_for_rps[outcome_for_rps>0]=1
    setcolorder(outcome_for_rps,c('Home','Tie','Away'))
    
    # calculate RPS
    overall_results=data.table(cvresult[,list(repl,fold,lambda)],RPS=RPS_matrix(cvresult[,list(Home,Tie,Away)],outcome_for_rps))
    
    # summarize performance for each lambda
    overall_results_summary=overall_results[,list(RPS=mean(RPS)),list(repl,fold,lambda)]
    
    # find best lambdas as in glmnet based on RPS
    overall_results_summary=overall_results_summary[,list(meanRPS=mean(RPS),sdRPS=sd(RPS)),list(lambda)]
    overall_results_summary[,RPS_mean_lb := meanRPS - sdRPS]
    overall_results_summary[,RPS_mean_ub := meanRPS + sdRPS]
    
    cv_lambda_min=overall_results_summary[which.min(meanRPS)]$lambda
    
    semin=overall_results_summary[lambda==cv_lambda_min]$RPS_mean_ub
    cv_lambda.1se=max(overall_results_summary[meanRPS<semin]$lambda)
    
    cvResultsSummary = list(lambda.min =cv_lambda_min, lambda.1se = cv_lambda.1se,
                            meanRPS_min=overall_results_summary[lambda==cv_lambda_min]$meanRPS,
                            meanRPS_1se=overall_results_summary[lambda==cv_lambda.1se]$meanRPS)
    
  }
  
  # fit final glmnet model with the lambda with minimum error
  final_glmnet_fit = glmnet(as.matrix(glm_train_data),as.factor(train_class),family="multinomial", alpha = alpha,lambda=cvResultsSummary$lambda.min)
  print("a")
  # obtain predictions
  # as(x, "dgCMatrix")
  #predicted_probabilities=predict(final_glmnet_fit, as.matrix(glm_test_data), type = "response")
  predicted_probabilities=predict(final_glmnet_fit, data.matrix(glm_test_data), type = "response")
  
  #check order of predictions
  order_of_class=attr(predicted_probabilities,'dimnames')[[2]]
  new_order=c(which(order_of_class=='Home'),which(order_of_class=='Tie'),which(order_of_class=='Away'))
  
  final_result=data.table(test_features[,list(matchId,MatchResult)],predicted_probabilities[,new_order,1])
  
  return(list(predictions=final_result,cv_stats=cvResultsSummary))
}


### create dataset and model evaluation





fixture<-fixture_create(dt_season1)
model_data_sz1<-feature_extract(dt_season1)


fixture<-fixture_create(dt_season2)
model_data_sz2<-feature_extract(dt_season2)


fixture<-fixture_create(dt_season3)
model_data_sz3<-feature_extract(dt_season3)


fixture<-fixture_create(dt_season4)
model_data_sz4<-feature_extract(dt_season4)

fixture<-fixture_create(dt_season5)
model_data_sz5<-feature_extract(dt_season5)

# 
model_data<-rbind(model_data_sz1,model_data_sz2,model_data_sz3,model_data_sz4,model_data_sz5) #merge seasons

# selected best features,all fatures can be seen in above, First 5 features is for labelling

model_data<-model_data[,c("matchId","leagueId","odd1","date","MatchResult","homeprob","awayprob","drawprob","HomeTotalGrade","AwayTotalGrade","Is.special","homeL5rps","awayL5rps","homeinhomeOldrps","awayinawayOldrps")]


# date filter

train.data<- model_data[date<firstdayofweek] # 70% data goes in here 2018-12-03
test.data<- model_data[date>=firstdayofweek]



# model results
train_features=train.data

test_features=test.data


predictions=train_glmnet(train_features, test_features, not_included_feature_indices=c(1:5), alpha=1,nlambda=10, tune_lambda=TRUE,nofReplications=2,nFolds=10,trace=T)

data_predictions=predictions$predictions

print(data_predictions)


#Submission format

data_predictions_number=as.matrix(data_predictions)

for (i in 1:10)

  {
  #cat(data_predictions_number[i,1],",",data_predictions_number[i,3],",",data_predictions_number[i,4],",",data_predictions_number[i,5],",")
  cat(sprintf(" %s, %s, %s, %s,",data_predictions_number[i,1],data_predictions_number[i,3],data_predictions_number[i,4],data_predictions_number[i,5]))
  
}

## evaluate performance of  matches

## get played match results

dt_matches <- readRDS("~/Documents/Dersler/Fall-2018/IE-582/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")
dt_matches <- dt_matches[,date:=as.Date(as.POSIXct(date,origin="1970-01-01",tz = "UTC",tryformats="%Y/%m/%d"))]
dt_matches[,c("HomeGoal","AwayGoal"):= tstrsplit(score,":",fixed=TRUE)]
dt_matches[,`:=`(MatchResult = ifelse(HomeGoal == AwayGoal, "Tie" , ifelse(HomeGoal > AwayGoal, 'Home' , 'Away')))]
dt_matches[,`:=`(ResultHome = ifelse(MatchResult=="Home",1,0)
                 ,ResultTie = ifelse(MatchResult=="Tie",1,0)
                 ,ResultAway = ifelse(MatchResult=="Away",1,0))]

dt_playedmatches<-dt_matches[date>=firstdayofweek & date<=lastdayofweek]


data_predictions<-data_predictions[order(matchId),]
dt_playedmatches<-dt_playedmatches[order(matchId),]  

data_rps<-RPS_matrix(data_predictions[,.(Home,Tie,Away)],dt_playedmatches[,.(ResultHome,ResultTie,ResultAway)])

dt_playedmatches$rps<-data.frame(data_rps)

print(mean(dt_playedmatches$rps))

