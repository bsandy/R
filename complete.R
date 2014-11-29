
complete <- function(directory, id = 1:332) {

  filelist <- list.files(directory,full.names=TRUE)
  output <- data.frame()
  
  for (i in id) {
    dataset<-read.csv(filelist[i])
    
    nobs <- sum(complete.cases(dataset))
    output <- rbind(output,cbind(id=i,nobs))
  }  
  output
}