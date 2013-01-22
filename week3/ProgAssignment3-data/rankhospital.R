rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome2 <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  outcome2[,11]<-as.numeric(outcome2[,11])
  outcome2[,17]<-as.numeric(outcome2[,17])
  outcome2[,23]<-as.numeric(outcome2[,23])
  
  ## Check that state and outcome are valid
  if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia"){
    stop("invalid outcome")
  }
  index<-0
  if(outcome == "heart attack"){
    #print(11)
    index<-11
  }else if(outcome=="heart failure"){
    #print(17)
    index<-17
  }else if(outcome=="pneumonia"){
    #print(23)
    index<-23
  }
  t<-table(outcome2$State)
  if(!(state %in% names(t))){
    stop("invalid state")
  }
  
  outcome2.state<-outcome2[outcome2$State %in% state,]
  outcome2.state<-outcome2.state[!is.na(outcome2.state[[index]]),]
  
  if(num=="best"){
    num=1
  }else if(num=="worst"){
    num=nrow(outcome2.state)
  }
    
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  ii<-order(outcome2.state[[index]], outcome2.state[[2]])
  ret <- NA
  if(num<=nrow(outcome2.state))
    ret <- outcome2.state[ii[num],2]
  ret
}

