rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  data <- read.csv("outcome-of-care-measures.csv",na.strings = "Not Available", stringsAsFactors=FALSE, check.names = FALSE)
  
  ## Assign outcome index
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  
  ##Check that state and outcome are valid
  hospitalState <- unique(data[,7])
  if (is.na(match(state,hospitalState))) {
    stop("invalid state")
  }
  
  ## check if the outcome is valid
  if (! outcome %in% names(outcomes)) {
    stop ("invalid outcome")
  }
  
  ## Build data frame to include 3 columns only
  data <- data[,c(2,7,outcomes[outcome])]
  names(data) <- c("hospital","state","rate")
  data <- na.omit(data)

  ## subset data
  data <- data[data[,"state"] == state,]
  
  ## rank data
  data <- data[order(data$state,data$rate,data$hospital),]
  
  ## use Hospital and Rate columns
  data <- data[,c(1,3)]
  
  ## bind a new column and assign rank value
  data <- cbind(data,c(1:nrow(data)))
  
  ## relabel columns
  names(data) <- c("Hospital.Name","Rate","Rank")
  
  ## assign Rank index
  if (num == "best") { rankindex = 1
  } else if (num == "worst") {rankindex = nrow(data)
  } else rankindex = num
  
  #print(rankindex)
  
  ## Return hospital name in that state with lowest 30-day death rate
  if (rankindex > nrow(data)) {
    return("NA")
  } else {
    data <- data[data[,"Rank"] == rankindex,]
    return (data$Hospital.Name)    
  }
}