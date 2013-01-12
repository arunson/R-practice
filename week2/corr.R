corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        getCor <-function(directory, id){
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
        	len <- length(result[[1]])
        	ret <- NA
        	if(len > threshold){
        		ret <- cor(result[[2]],result[[3]])
        	}
        	ret
        }
        id<-1:332
        cors = sapply(id,getCor,directory=directory)
        bad<-is.na(cors)
        as.numeric(cors[!bad])
}
