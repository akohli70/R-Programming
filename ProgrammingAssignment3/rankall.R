rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  data <- read.csv("outcome-of-care-measures.csv",na.strings = "Not Available", stringsAsFactors=FALSE, check.names = FALSE)
  
  ## Assign outcome index
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  
  ## check if the outcome is valid
  if (! outcome %in% names(outcomes)) {
    stop ("invalid outcome")
  }
  
  ## Build data frame to include 3 columns only
  data <- data[,c(2,7,outcomes[outcome])]
  names(data) <- c("hospital","state","rate")
  
  data <- na.omit(data)
  
  ## rank data
  data <- data[order(data$state,data$rate,data$hospital),]
  
  ## bind a new column and assign rank value
  data <- cbind(data,c(1:nrow(data)))
  
  ## relabel columns
  names(data) <- c("hospital","state", "rate","rank")
  

  ## Rank, group by state
  data$rank = ave(data$rate, data$state, 
              FUN = function(x) rank(x, ties.method = "first"))
  
  ## assign Rank index
  if (num == "best") { rankindex = 1
  } else if (num == "worst") {rankindex = max(data$rank)
  } else rankindex = num
  

  ## Return hospital name in that state with lowest 30-day death rate
  
  newdata <- data.frame()
  myState <- c(unique(data[,c(2)]))
  
  if (num == "worst"){

    for (i in 1:length(myState)){
      tempData <- data.frame()
      tempData <- data[data[,"state"] == myState[i],]
      ##print("index")
      ##print(max(tempData$rank))
      newdata <- rbind(newdata,tempData[tempData[,"rank"] == max(tempData$rank),])
    } 
    newdata <- newdata[,c(1,2)]
    }
    else {
  
    newdata <- data[data[,"rank"] == rankindex,]
    newdata <- newdata[,c(1,2)]
  
    for (i in 1:length(myState)){
      if (is.na(match(myState[i],newdata$state))) {
        newdata <- rbind(newdata,c("<NA>",myState[i]))
      } 
    }
  
    newdata <- newdata[order(newdata$state),]
    names(data) <- c("hospital","state")
  
    }
  
  return (newdata) 
}