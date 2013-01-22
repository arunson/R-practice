best <- function(state, outcome) {
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
  
  ## Return hospital name in that state with lowest 30-day death
  outcome2.state<-outcome2[outcome2$State %in% state,]
  
  minimum <- min(outcome2.state[,index],na.rm=TRUE)
  minimum.hospital<-outcome2.state[outcome2.state[,index]==minimum,"Hospital.Name"]
  minimum.hospital<-minimum.hospital[!is.na(minimum.hospital)]
  ## rate
  sort(minimum.hospital)[1]
  
}