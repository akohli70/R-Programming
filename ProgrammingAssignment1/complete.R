complete <- function(directory, id=1:332){
  
  files_full <- list.files(directory, full.names=TRUE)
  
  dat <- data.frame()
  for (i in id) {
    dat <- rbind (dat, read.csv(files_full[i]))
  }
  
  newdat <- data.frame()
  newdat <- subset(dat,!is.na(dat$sulfate) & !is.na(dat$nitrate))
  
  newdat2 <- data.frame()
  for (i in id) {
    newdat2 <- rbind(newdat2, c(i,sum(newdat$ID == i)))
  }
  colnames(newdat2) <- c("id","nobs")
  return(newdat2)
}