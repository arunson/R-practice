agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  if(is.null(age)){
    stop("age is null")
  }
  ## Read "homicides.txt" data file
  ## given
  homicides<-readLines("homicides.txt")
  
  ## Extract ages of victims; ignore records where no age is
  r <- regexec("([0-9]+) years old</dd>", homicides)
  m <- regmatches(homicides, r)
  ages <- sapply(m, function(x) x[2])
  
  ## Return integer containing count of homicides for that age
  ages<-ages[!is.na(ages)]
  length(ages[ages==age])
}