library(tidyverse)

data_2019<-read.csv("2019 PFF All Plays.csv")

#Pseudocode Explanation of method used
#Simplify the data set by select all the relevant columns needed for analysis
#Filtered the data to only include quaters 1 and 3 and non garbage time plays
#Rewrote field position on a 0 to 100 scale using an ifelse statement
#Used the cut function to put YTG in 2 yd bins and FP into 5 yard bins
#Inverted the entire data set in order to be able to work forwards and not backwards
#Duplicated the score column and lagged it by one in order to more easily compare when changes occur
#Created a column where a YES was recorded for a score change and NO if no score change happened 
#Used a for loop to record in a new column the value of a score change if one occurred
#Used an ifelse statement to convert the score from decimal form
#Used a for loop with an ifelse to change a score change of 6 to 7 or 8 if a conversion was successful
#Used a for loop with an ifelse and global variables to record the value of the next score change if no score happened on that play
#Used a for loop with an if else and global variables to record which team did the scoring
#Used an ifelse to check if the team with the second down was the team that scored (Then multiplied the score change by -1 or 1)
#Filtered out any drives that had scores changes from a different quarter or game (The lingering drives)
#Filtered to only include 2nd down plays
#Grouped by YTG and FP and averaged EP for each distance to go and field position
#Selected the rows and columns I wanted and created a table


data_simplified<-data_2019%>%select(pff_GAMEID,pff_PLAYID,pff_GARBAGETIME,pff_QUARTER,pff_DOWN,pff_DISTANCE,
                                    pff_FIELDPOSITION,pff_DEFSCORE,pff_OFFSCORE,pff_SCORE,pff_SCOREDIFFERENTIAL,
                                    pff_DEFTEAM,pff_OFFTEAM)

filtered_data_simplified<-data_simplified%>%filter(pff_QUARTER == 1|pff_QUARTER == 3)%>%
  filter(pff_GARBAGETIME == 0)%>%mutate(pff_FIELDPOSITION=as.numeric(pff_FIELDPOSITION))                        

#Rewrites field position to 0 to 100  
filtered_data_simplified$new_pos<-ifelse(filtered_data_simplified$pff_FIELDPOSITION<0,filtered_data_simplified$pff_FIELDPOSITION*-1,100-filtered_data_simplified$pff_FIELDPOSITION)

#Puts Yards to Go into 2 yard bins
filtered_data_simplified$YTG<-cut(filtered_data_simplified$pff_DISTANCE,breaks = c(0,1,3,5,7,9,11,13,15),labels = c("1","2-3","4-5","6-7","8-9","10-11","12-13","14-15"))

#Puts field position into 5 yard bins
filtered_data_simplified$Field_Position<-cut(filtered_data_simplified$new_pos,
                              breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),
                              labels = c("0-5","6-10","11-15","16-20","21-25","26-30","31-35","36-40",
                                         "41-45","46-50","51-55","56-60","61-65","66-70","71-75","76-80","81-85","86-90",
                                         "91-95","96-100"))

inverted_data<-filtered_data_simplified[order(nrow(filtered_data_simplified):1),]

inverted_data<-inverted_data%>%mutate(score_change=0)%>%mutate(pff_SCORE_2=pff_SCORE)%>%
  mutate(pff_SCORE_2=lag(pff_SCORE_2,default = first(pff_SCORE)))

inverted_data$does_score_change<-ifelse(inverted_data$pff_SCORE == inverted_data$pff_SCORE_2,"NO","YES")

for (i in 1:length(inverted_data$pff_SCORE)){
  inverted_data$score_change[i+1]=inverted_data$pff_SCORE[i]-inverted_data$pff_SCORE[i+1]
}

inverted_data$score_change<-ifelse(inverted_data$score_change<.1,inverted_data$score_change*100,inverted_data$score_change*1)

for (i in 1:length(inverted_data$score_change)){
  ifelse(inverted_data$score_change[i] == 1 & inverted_data$score_change[i+1] == 6,
         inverted_data$score_change[i+1] <- inverted_data$score_change[i] + inverted_data$score_change[i+1], 
         inverted_data$score_change[i] <- inverted_data$score_change[i]*1)
}

for (i in 1:length(inverted_data$score_change)){
  ifelse(inverted_data$score_change[i] == 2 & inverted_data$score_change[i+1] == 6,
         inverted_data$score_change[i+1] <- inverted_data$score_change[i] + inverted_data$score_change[i+1], 
         inverted_data$score_change[i] <- inverted_data$score_change[i]*1)
}


x<-0
team<-"UVA"

for (i in 1:length(inverted_data$pff_SCORE)){
  ifelse(inverted_data$does_score_change[i] == "YES",assign("x",inverted_data$score_change[i],envir = .GlobalEnv),inverted_data$score_change[i] <- x )
}

inverted_data<-inverted_data%>%mutate(who_scored=0)

for (i in 1:length(inverted_data$pff_SCORE)){
  ifelse(inverted_data$does_score_change[i] == "YES",assign("team",inverted_data$pff_DEFTEAM[i],envir = .GlobalEnv),inverted_data$who_scored[i] <- team )
}

inverted_data_filtered<-inverted_data%>%filter(pff_DOWN == 2)%>%filter(pff_DISTANCE <= 15)%>%filter(score_change == 8 | score_change == 7 | 
                                                     score_change == 6 | score_change == 3 |
                                                     score_change == 2)

inverted_data_filtered$score_change<-ifelse(inverted_data_filtered$who_scored==inverted_data_filtered$pff_OFFTEAM,inverted_data_filtered$score_change*1,inverted_data_filtered$score_change*-1)

output_table<-inverted_data_filtered%>%group_by(YTG,Field_Position)%>%mutate(exp_points=sum(score_change)/n())%>%distinct(pff_DOWN,YTG,Field_Position,exp_points)%>%arrange(Field_Position,YTG)

write.table(output_table)


