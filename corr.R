corr <- function(directory, threshold = 0) {  
  obs <- complete(directory) 
  obs <- obs[obs$nobs > threshold, ]
  
  filelist <- list.files(directory,full.names=TRUE)
  output <- numeric(length = 0)
  
  for(i in obs$id) {  
    data <- read.csv(filelist[i])
    output <- c(output,cor(data$sulfate,data$nitrate,use="complete.obs"))
  }                           
  output  
}



