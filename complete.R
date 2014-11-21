
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

  filelist <- list.files(directory,full.names=TRUE)
  datasum <- data.frame()
  
  for (i in id) {
    data<-read.csv(filelist[i])
    
    data2 <- na.omit(data)
    nobs <- nrow(data2)
    ##id <- i
    datasum <- rbind(datasum, cbind(id=i, nobs))
    
    ##nobs <- sum(complete.cases(dataset))
    ##output <- rbind(output,cbind(id=i,nobs))
  }  
  datasum
}