complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        getNobs <-function(directory, id){
        	convertedId<-id
        	if(id<10){
        		convertedId<-paste("00",id,sep="")
        	}else if(id<100){
        		convertedId<-paste("0",id,sep="")
        	}
        	filepath<-paste(directory,"/",convertedId,".csv",sep="")
        	x1<-read.csv(filepath)
        	y1<-complete.cases(x1)
        	result<-x1[y1,]
        	length(result[[1]])
        }
        nobs=sapply(id,getNobs,directory=directory)
        data.frame(id,nobs)
}
