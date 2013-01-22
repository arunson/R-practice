rankall <- function(outcome, num = "best") {
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
  ## For each state, find the hospital of the given rank
  t<-table(outcome2$State)

  outcome2 <- outcome2[!is.na(outcome2[[index]]),]
  
  outcome2.split<-split(outcome2,outcome2$State)
  
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  getHospital <- function(data,index, num){
    
    if(num=="best"){
      num=1
    }else if(num=="worst"){
      num=nrow(data)
    }
    
    ii<-order(data[[index]],data[[2]])
    data[ii[num],2]
  }
  
  retHospital<-lapply(outcome2.split,getHospital,index=index,num=num)
  as.data.frame(cbind(hospital=retHospital,state=names(retHospital)))
}
