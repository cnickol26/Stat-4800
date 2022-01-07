library(tidyverse)
#install.packages("fdrtool")
library(fdrtool)

data_19<-read.csv("2019 PFF All Plays.csv")
run<-data_19%>%filter(pff_RUNPASS == "R" ) #63509 runs or 47%
pass<-data_19%>%filter(pff_RUNPASS == "P" & pff_PASSRESULT != "SPIKE" & pff_PASSRESULT != "") #70847 passes or 53%

#Run
summary(run$pff_GAINLOSSNET) #Mean of 4.746
distance<-run$pff_GAINLOSSNET
sd(distance,na.rm=TRUE) #SD of 8.388
hist(distance) #Run distribution overall looks gamma

#Pass
summary(pass$pff_GAINLOSSNET) #Mean of 6.427
distance_pass<-pass$pff_GAINLOSSNET
sd(distance_pass,na.rm = TRUE) #SD of 11.21
hist(distance_pass) #Pass distribution overall looks gamma as well

#Breaking down the run distribution

#Long NEG Plays
long_neg_run<-run%>%filter(pff_GAINLOSSNET < -7 & pff_GAINLOSSNET > -15) #1.4%
summary(long_neg_run$pff_GAINLOSSNET) #Mean of -10.1
sd(long_neg_run$pff_GAINLOSSNET,na.rm=TRUE) #Var of 1.299
hist(long_neg_run$pff_GAINLOSSNET) #Normal

#Shorter Pos and Neg Run Plays
short_neg_run<-run%>%filter(pff_GAINLOSSNET < 12 & pff_GAINLOSSNET > -8) #87.6%
summary(short_neg_run$pff_GAINLOSSNET) #Mean of 2.964
sd(short_neg_run$pff_GAINLOSSNET,na.rm=TRUE) #Var of 3.381
hist(short_neg_run$pff_GAINLOSSNET) #Normal

#Medium Runs
medium_run<-run%>%filter(pff_GAINLOSSNET > 11 & pff_GAINLOSSNET < 25) #8%
summary(medium_run$pff_GAINLOSSNET) #Mean of 16
sd(medium_run$pff_GAINLOSSNET,na.rm=TRUE) #SD of 3.418
hist(medium_run$pff_GAINLOSSNET) #Truncated Normal

#Long Runs
long_run<-run%>%filter(pff_GAINLOSSNET > 25) #2.5%
summary(long_run$pff_GAINLOSSNET) #Mean of 41.74
sd(long_run$pff_GAINLOSSNET,na.rm=TRUE) #SD of 14.858
hist(long_run$pff_GAINLOSSNET) #Truncated Normal

test<-long_run%>%select(pff_GAINLOSSNET)
yards<-abs(rnorm(1,41.74,14.858))
yards

#Breaking down the pass distribution
results_pass<-pass%>%distinct(pff_PASSRESULT)

#Number of incomplete
incomp<-pass%>%filter(pff_PASSRESULT == "INCOMPLETE"|pff_PASSRESULT == "THROWN AWAY"|pff_PASSRESULT == "INTERCEPTION"|
                        pff_PASSRESULT == "BATTED PASS"|pff_PASSRESULT == "HIT AS THREW") #36.1%
not_incomp<-pass%>%filter(pff_PASSRESULT == "COMPLETE"|pff_PASSRESULT == "SACK"|pff_PASSRESULT == "RUN"|
                            pff_PASSRESULT == "LATERAL")#63.9%

#NEG Plays
neg_pass<-not_incomp%>%filter(pff_GAINLOSSNET < -3 & pff_GAINLOSSNET > -15) #8.9%
summary(neg_pass$pff_GAINLOSSNET) #Mean of -7.283
sd(neg_pass$pff_GAINLOSSNET,na.rm=TRUE) #SD of 2.44
hist(neg_pass$pff_GAINLOSSNET) #Uniform

#Short Passes
short_pass<-not_incomp%>%filter(pff_GAINLOSSNET > -3 & pff_GAINLOSSNET < 11) #52.8%
summary(short_pass$pff_GAINLOSSNET) #Mean of 5.01
sd(short_pass$pff_GAINLOSSNET,na.rm=TRUE) #SD of 3.217
hist(short_pass$pff_GAINLOSSNET) #Normal

#Medium Passes
medium_pass<-not_incomp%>%filter(pff_GAINLOSSNET > 10 & pff_GAINLOSSNET < 22) #24.0%
summary(medium_pass$pff_GAINLOSSNET) #Mean of 14.83
sd(medium_pass$pff_GAINLOSSNET,na.rm=TRUE) #SD of 3.055
hist(medium_pass$pff_GAINLOSSNET) #Truncated normal

#Long Passes
long_pass<-not_incomp%>%filter(pff_GAINLOSSNET > 21) #12.3%
summary(long_pass$pff_GAINLOSSNET) #Mean of 34.79
sd(long_pass$pff_GAINLOSSNET,na.rm=TRUE) #SD of 13.144
hist(long_pass$pff_GAINLOSSNET) #Truncated normal


standard_yds_gained<-function(){
  random_number<-runif(1,0,100)
  if (random_number>=53){
    comp_incomp<-rbinom(1,1,.639)
    if (comp_incomp == 1){
      random_pass_num<-runif(1,0,100)
      if (random_pass_num <= 10.9){
        runif(1,-15,-3)
      }
      else if (random_pass_num > 10.9 & random_pass_num <= 63.8){
        rnorm(1,5.01,3.217)
      }
      else if (random_pass_num > 63.8 & random_pass_num <= 87.8){
        abs(rnorm(1,14.83,3.055))
      }
      else{
        abs(rnorm(1,34.79,13.144))
      }
    }
    else{
      0
    } 
  }
  else{
    random_run_num<-runif(1,0,100)
    if (random_run_num < 1.5){
      rnorm(1,-10.1,1.299)
    }
    else if (random_run_num >= 1.5 & random_run_num < 89){
      rnorm(1,2.964,3.381) 
    }
    else if (random_run_num >= 89 & random_run_num < 97){
      abs(rnorm(1,16,3.418))
    }
    else{
      abs(rnorm(1,41.74,14.858))
    }
  }
}

standard_yds_gained()

#Looking at 4th and short
data_19_test<-data_19%>%select(pff_QUARTER,pff_DISTANCE,pff_DOWN,pff_FIELDPOSITION,pff_GAINLOSSNET,pff_RUNPASS,pff_PASSRESULT,
                               pff_GARBAGETIME,pff_GAMEID,pff_DEFSCORE,pff_OFFSCORE,
                               pff_DEFTEAM,pff_OFFTEAM,pff_SCORE,pff_SCOREDIFFERENTIAL,
                               pff_PLAYID)
data_19_test<-data_19_test%>%filter(pff_DOWN==4,pff_DISTANCE==1)%>%filter(pff_RUNPASS == "R" | pff_RUNPASS == "P") #1140 obs
run_data<-data_19_test%>%filter(pff_RUNPASS=="R")#87%
pass_data<-data_19_test%>%filter(pff_RUNPASS=="P")#13%

#Run
summary(run_data$pff_GAINLOSSNET) #Mean of 2.853
run_4<-run_data$pff_GAINLOSSNET
sd(run_4,na.rm=TRUE) #SD of 7.989
hist(run_4) #Run distribution overall looks Normal

results_pass_4<-pass_data%>%distinct(pff_PASSRESULT)

#Number of incomplete
incomp_4<-pass_data%>%filter(pff_PASSRESULT == "INCOMPLETE"|pff_PASSRESULT == "THROWN AWAY"|pff_PASSRESULT == "INTERCEPTION"|
                               pff_PASSRESULT == "BATTED PASS"|pff_PASSRESULT == "HIT AS THREW") #36%
not_incomp_4<-pass_data%>%filter(pff_PASSRESULT == "COMPLETE"|pff_PASSRESULT == "SACK"|pff_PASSRESULT == "RUN"|
                                   pff_PASSRESULT == "LATERAL")#64%

#Pass
summary(not_incomp_4$pff_GAINLOSSNET) #Mean of 7.815
pass_4<-not_incomp_4$pff_GAINLOSSNET
sd(pass_4,na.rm = TRUE) #SD of 11.536
hist(pass_4) #Pass distribution overall looks normal

yards_gained_4th_1<-function(){
  ran_num<-runif(1,0,100)
  if (ran_num < 87){
    rnorm(1,2.853,7.989)
  }
  else{
    incom_comp<-rbinom(1,1,.64)
    if (incom_comp == 1){
      rnorm(1,7.815,11.536)
    }
    else{
      0
    }
  }
}

yards_gained_4th_1()

#Looking at 3rd and short

data_19_test<-data_19%>%select(pff_QUARTER,pff_DISTANCE,pff_DOWN,pff_FIELDPOSITION,pff_GAINLOSSNET,pff_RUNPASS,pff_PASSRESULT,
                               pff_GARBAGETIME,pff_GAMEID,pff_DEFSCORE,pff_OFFSCORE,
                               pff_DEFTEAM,pff_OFFTEAM,pff_SCORE,pff_SCOREDIFFERENTIAL,
                               pff_PLAYID)
data_19_test2<-data_19_test%>%filter(pff_DOWN==3)%>%filter(pff_DISTANCE == 1 | pff_DISTANCE == 2)%>%filter(pff_RUNPASS == "R" | pff_RUNPASS == "P") #4954 obs
run_data_3<-data_19_test2%>%filter(pff_RUNPASS=="R")#78%
pass_data_3<-data_19_test2%>%filter(pff_RUNPASS=="P")#22%

#Run
summary(run_data_3$pff_GAINLOSSNET) #Mean of 3.865
run_3<-run_data_3$pff_GAINLOSSNET
sd(run_3,na.rm=TRUE) #SD of 7.9
hist(run_3) #Run distribution overall looks Normal

results_pass_3<-pass_data%>%distinct(pff_PASSRESULT)

#Number of incomplete
incomp_3<-pass_data_3%>%filter(pff_PASSRESULT == "INCOMPLETE"|pff_PASSRESULT == "THROWN AWAY"|pff_PASSRESULT == "INTERCEPTION"|
                               pff_PASSRESULT == "BATTED PASS"|pff_PASSRESULT == "HIT AS THREW") #34%
not_incomp_3<-pass_data_3%>%filter(pff_PASSRESULT == "COMPLETE"|pff_PASSRESULT == "SACK"|pff_PASSRESULT == "RUN"|
                                   pff_PASSRESULT == "LATERAL")#66%

#Pass
summary(not_incomp_3$pff_GAINLOSSNET) #Mean of 7.94
pass_3<-not_incomp_3$pff_GAINLOSSNET
sd(pass_3,na.rm = TRUE) #SD of 12.626
hist(pass_3) #Pass distribution overall looks normal

yards_gained_3rd_short<-function(){
  ran_num<-runif(1,0,100)
  if (ran_num < 78){
    rnorm(1,3.865,7.9)
  }
  else{
    incom_comp<-rbinom(1,1,.66)
    if (incom_comp == 1){
      rnorm(1,7.94,12.626)
    }
    else{
      0
    }
  }
}

yards_gained_3rd_short()

#Looking at 3rd and long

data_19_test<-data_19%>%select(pff_QUARTER,pff_DISTANCE,pff_DOWN,pff_FIELDPOSITION,pff_GAINLOSSNET,pff_RUNPASS,pff_PASSRESULT,
                               pff_GARBAGETIME,pff_GAMEID,pff_DEFSCORE,pff_OFFSCORE,
                               pff_DEFTEAM,pff_OFFTEAM,pff_SCORE,pff_SCOREDIFFERENTIAL,
                               pff_PLAYID)
data_19_test3<-data_19_test%>%filter(pff_DOWN==3)%>%filter(pff_DISTANCE > 7)%>%filter(pff_RUNPASS == "R" | pff_RUNPASS == "P") #4954 obs
run_data_3_long<-data_19_test3%>%filter(pff_RUNPASS=="R")#16%
pass_data_3_long<-data_19_test3%>%filter(pff_RUNPASS=="P")#84%

#Run
summary(run_data_3_long$pff_GAINLOSSNET) #Mean of 5.502
run_3_long<-run_data_3_long$pff_GAINLOSSNET
sd(run_3_long,na.rm=TRUE) #SD of 8.982
hist(run_3_long) #Run distribution overall looks Normal

results_pass_3_long<-pass_data%>%distinct(pff_PASSRESULT)

#Number of incomplete
incomp_3_long<-pass_data_3_long%>%filter(pff_PASSRESULT == "INCOMPLETE"|pff_PASSRESULT == "THROWN AWAY"|pff_PASSRESULT == "INTERCEPTION"|
                                 pff_PASSRESULT == "BATTED PASS"|pff_PASSRESULT == "HIT AS THREW") #40%
not_incomp_3_long<-pass_data_3_long%>%filter(pff_PASSRESULT == "COMPLETE"|pff_PASSRESULT == "SACK"|pff_PASSRESULT == "RUN"|
                                     pff_PASSRESULT == "LATERAL")#60%

#Pass
summary(not_incomp_3_long$pff_GAINLOSSNET) #Mean of 9.288
pass_3_long<-not_incomp_3_long$pff_GAINLOSSNET
sd(pass_3_long,na.rm = TRUE) #SD of 12.98
hist(pass_3_long) #Pass distribution overall looks normal

yards_gained_3rd_long<-function(){
  ran_num<-runif(1,0,100)
  if (ran_num < 17){
    rnorm(1,5.502,8.982)
  }
  else{
    incom_comp<-rbinom(1,1,.60)
    if (incom_comp == 1){
      rnorm(1,9.288,12.98)
    }
    else{
      0
    }
  }
}

yards_gained_3rd_long()

# Looking at yards gained in the redzone
new_data<-data_19%>%select(pff_QUARTER,pff_DISTANCE,pff_DOWN,pff_FIELDPOSITION,pff_GAINLOSSNET,pff_RUNPASS,pff_PASSRESULT,
                           pff_GARBAGETIME,pff_GAMEID,pff_DEFSCORE,pff_OFFSCORE,
                           pff_DEFTEAM,pff_OFFTEAM,pff_SCORE,pff_SCOREDIFFERENTIAL,
                           pff_PLAYID)
new_data$new_pos<-ifelse(new_data$pff_FIELDPOSITION<0,new_data$pff_FIELDPOSITION*-1,100-new_data$pff_FIELDPOSITION)
red_zone_data<-new_data%>%filter(new_pos>=80)%>%filter(pff_RUNPASS == "R" | pff_RUNPASS == "P")

run_data_red<-red_zone_data%>%filter(pff_RUNPASS=="R")#57%
pass_data_red<-red_zone_data%>%filter(pff_RUNPASS=="P")#43%

#Run
summary(run_data_red$pff_GAINLOSSNET) #Mean of 2.692
run_data_red<-run_data_red$pff_GAINLOSSNET
sd(run_data_red,na.rm=TRUE) #SD of 5.125
hist(run_data_red) #Run distribution overall looks Normal

results_pass_red_long<-pass_data_red%>%distinct(pff_PASSRESULT)

#Number of incomplete
incomp_red<-pass_data_red%>%filter(pff_PASSRESULT == "INCOMPLETE"|pff_PASSRESULT == "THROWN AWAY"|pff_PASSRESULT == "INTERCEPTION"|
                                           pff_PASSRESULT == "BATTED PASS"|pff_PASSRESULT == "HIT AS THREW") #44%
not_incomp_red<-pass_data_red%>%filter(pff_PASSRESULT == "COMPLETE"|pff_PASSRESULT == "SACK"|pff_PASSRESULT == "RUN"|
                                               pff_PASSRESULT == "LATERAL")#56%

#Pass
summary(not_incomp_red$pff_GAINLOSSNET) #Mean of 5.474
pass_red<-not_incomp_red$pff_GAINLOSSNET
sd(pass_red,na.rm = TRUE) #SD of 7.691
hist(pass_red) #Pass distribution overall looks normal

yards_gained_red<-function(){
  ran_num<-runif(1,0,100)
  if (ran_num < 57){
    rnorm(1,2.692,5.125)
  }
  else{
    incom_comp<-rbinom(1,1,.56)
    if (incom_comp == 1){
      rnorm(1,5.474,7.691)
    }
    else{
      0
    }
  }
}

yards_gained_red()

# Looking at yards gained on fumble recoveries
fumble_data<-data_19%>%select(pff_QUARTER,pff_DISTANCE,pff_DOWN,pff_FIELDPOSITION,pff_GAINLOSSNET,
                           pff_GAMEID,pff_DEFSCORE,pff_OFFSCORE,pff_DRIVEENDEVENT,
                           pff_DEFTEAM,pff_OFFTEAM,pff_SCORE,pff_SCOREDIFFERENTIAL,
                           pff_PLAYID)
fumble_data<-fumble_data%>%filter(pff_DRIVEENDEVENT == "FUMBLE")%>%mutate(pff_GAINLOSSNET=pff_GAINLOSSNET*-1)
fumble_data$new_pos<-ifelse(fumble_data$pff_FIELDPOSITION<0,fumble_data$pff_FIELDPOSITION*-1,100-fumble_data$pff_FIELDPOSITION)
fumble_data <- fumble_data%>%group_by(pff_GAMEID,pff_QUARTER)%>%slice(n())

# .6% of plays
summary(fumble_data$pff_GAINLOSSNET) #Mean of 0.3878
fumble<-fumble_data$pff_GAINLOSSNET
sd(fumble,na.rm = TRUE) #SD of 13.4179
hist(fumble) #Fumble return distribution overall looks normal

yards_gained_fumble<-function(){
  rnorm(1,0.3878,13.4179)
}

yards_gained_fumble()

# Looking at yards gained on interceptions
interception_data<-data_19%>%select(pff_QUARTER,pff_DISTANCE,pff_DOWN,pff_FIELDPOSITION,pff_GAINLOSSNET,
                              pff_GAMEID,pff_DEFSCORE,pff_OFFSCORE,pff_DRIVEENDEVENT,
                              pff_DEFTEAM,pff_OFFTEAM,pff_SCORE,pff_SCOREDIFFERENTIAL,
                              pff_PLAYID)
interception_data<-interception_data%>%filter(pff_DRIVEENDEVENT == "INTERCEPTION")%>%mutate(pff_GAINLOSSNET=pff_GAINLOSSNET*-1)
interception_data <- interception_data%>%group_by(pff_GAMEID,pff_QUARTER)%>%slice(n())

# .8% of plays
summary(interception_data$pff_GAINLOSSNET) #Mean of 0.2941
interception<-interception_data$pff_GAINLOSSNET
sd(interception,na.rm = TRUE) #SD of 2.207
hist(interception) #Fumble return distribution overall looks normal

yards_gained_interception<-function(){
  rnorm(1,0.2941,2.207)
}

yards_gained_interception()

new_data<-data_19%>%select(pff_QUARTER,pff_DISTANCE,pff_DOWN,pff_FIELDPOSITION,
                           pff_GARBAGETIME,pff_GAMEID,pff_DEFSCORE,pff_OFFSCORE,
                           pff_DEFTEAM,pff_OFFTEAM,pff_SCORE,pff_SCOREDIFFERENTIAL,
                           pff_PLAYID,pff_SPECIALTEAMSTYPE)
new_data<-new_data%>%mutate(score_change=0)

new_data<-new_data%>%mutate(pff_SCORE_2=pff_SCORE)

new_data<-new_data%>%mutate(pff_SCORE_2=lag(pff_SCORE_2,default = first(pff_SCORE)))

new_data$does_score_change<-ifelse(new_data$pff_SCORE == new_data$pff_SCORE_2,"NO","YES")

for (i in 1:length(new_data$pff_SCORE)){
  new_data$score_change[i+1]=new_data$pff_SCORE[i]-new_data$pff_SCORE[i+1]
}
new_data<-new_data%>%mutate(score_change=abs(score_change))
new_data$score_change<-ifelse(new_data$score_change<.1,new_data$score_change*100,new_data$score_change*1)

extra_point<-new_data%>%filter(pff_SPECIALTEAMSTYPE == "EXTRA POINT" | 
                                 pff_SPECIALTEAMSTYPE == "KICKOFF")

# dividing up into home and away 

data_19$Homescore <- floor(data_19$pff_SCORE) 

data_19$Awayscore <- ((data_19$pff_SCORE %% 1)*100) 

data_19$diffscore <- round((data_19$Homescore - data_19$Awayscore), digits = 0) 

data_19 



# lags the data and then takes the difference between the respective scores and lags 

data_19$Homescorelag <- lag(data_19$Homescore, k=1) 

data_19$Awayscorelag <- lag(data_19$Awayscore, k=1) 

data_19$HomescorelagD <- data_19$Homescore - data_19$Homescorelag 

data_19$AwayscorelagD <- data_19$Awayscore - data_19$Awayscorelag 

#filters data for made extra points 

madeextrahome1 <- filter(data_19, data_19$pff_DRIVEENDEVENT == 'TOUCHDOWN' & (data_19$HomescorelagD == 1)) 

missedextrahome1 <- filter(data_19, data_19$pff_DRIVEENDEVENT == 'TOUCHDOWN' & (data_19$HomescorelagD == 6 & (data_19$pff_SPECIALTEAMSTYPE == "EXTRA POINT"))) 



madeextraaway1 <- filter(data_19, pff_DRIVEENDEVENT == 'TOUCHDOWN' & (data_19$AwayscorelagD == 1)) 

missedextraaway1 <- filter(data_19, pff_DRIVEENDEVENT == 'TOUCHDOWN' & (data_19$AwayscorelagD == 6& (data_19$pff_SPECIALTEAMSTYPE == "EXTRA POINT"))) 