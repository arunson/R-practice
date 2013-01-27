count <- function(cause = NULL) {
  
  ## Check that "cause" is non-NULL; else throw error
  if (is.null(cause)){
    stop("Specify cause")
  }
  ## Check that specific "cause" is allowed; else throw error
  cause_list = c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
  if (!(cause %in% cause_list)){
    stop("Specify available cause.")
  }
  ## Read "homicides.txt" data file
  homicides<-readLines("homicides.txt")
  
  ## Extract causes of death
  r <- regexec("<dd>[cC]ause: (.*?)</dd>", homicides)
  m <- regmatches(homicides, r)
  causes <- sapply(m, function(x) x[2])
  ## Return integer containing count of homicides for that cause
  reg_cause <- paste("[",substring(cause,1,1),toupper(substring(cause,1,1)),"]",substring(cause,2),sep="")
  filtered_causes<-causes[grepl(reg_cause, causes)]
  length(filtered_causes)
#   if(cause=="asphyxiation"){
#     
#   }else if(cause=="blunt force"){
#     
#   }else if(cause=="other"){
#     
#   }else if(cause=="shooting"){
#     
#   }else if(cause=="stabbing"){
#     
#   }else if(cause=="unknown"){
#     
#   }
}

