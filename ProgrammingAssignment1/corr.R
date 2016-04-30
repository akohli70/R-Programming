corr <- function(directory, threshold=0) {
  dat <- complete(directory) #change hardcoded value afterwards
  newdat <- subset(dat,dat$nobs>threshold)
  
  files_full <- list.files(directory, full.names=TRUE)
  
  #mycor <- data.frame()
  mycor <- vector(mode="numeric",length=0)
  for (i in newdat$id) {
    newdat2 <- data.frame()
    newdat2 <- rbind (newdat2, read.csv(files_full[i]))
    newdat2 <- subset(newdat2,!is.na(newdat2$sulfate) & !is.na(newdat2$nitrate))
    newdat2 <- subset(newdat2, select = -c(1,4))
    newdat2 <- cor(newdat2)
  
    #if (length(mycor) == 0) {
    #  mycor <- c(newdat2[[1,2]])
    #} else {
      mycor <- c(mycor,newdat2[[1,2]])  
    #}
  }
  return(mycor)
  #return(as.vector(mycor))
}