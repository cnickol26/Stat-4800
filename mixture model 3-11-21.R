yards<-function(){
  random<-rbinom(1,1,0.53) #Run to pass ratio was 53% pass in the 2019 pff data
  if (random == 0){
    run_prob<-rbinom(1,1,.037) #About a 4% chance that the running play amounted into a significant loss 
    if (run_prob == 1){
      runif(1,-15,-3) 
    }
     else{
      short_prob<-rbinom(1,1,0.69) #About a 69% chance that the run was between -2 and 5 yards
      if (long_prob == 1){
        rnorm(1,1.982,3.780)
      }
      else{
        runif(1,25,100) #31% chance that the run was "long"
      }
    }}
    else{
      comp_prob<-rbinom(1,1,0.6)
      if (comp_prob == 1){
        dist_prob<-rbinom(1,1,0.7)
        if (dist_prob == 1){
          rnorm(1,7,3)
        }
        else{
          long_pass<-rbinom(1,1,0.7)
          if (long_pass == 1){
            runif(1,15,35)
        }
          else{
            runif(1,35,100)
          }
      }}
       else{
         0
  }
}
}   
yards()    

rnorm(1,-6.833,7.626)

exp
