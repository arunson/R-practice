getmonitor <- function(id, directory, summarize = FALSE) {
        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'summarize' is a logical indicating whether a summary of
        ## the data should be printed to the console; the default is
        ## FALSE
        
        ## Your code here
        convertedId <- id
        if(id<10){
        	convertedId <- paste("00",id,sep="")
        } else if(id<100){
        	convertedId <- paste("0",id,sep="")
        }
        path<-paste(directory,convertedId,sep="/")
        filepath<-paste(path,".csv",sep="")
        x1<-read.csv(filepath)
        if(summarize==TRUE){
        	print(summary(x1))
        }
        x1
}
