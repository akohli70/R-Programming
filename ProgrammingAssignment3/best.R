best <- function (state, outcome) {
  ## Read outcome data
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
    stop ("invlaid outcome")
  }
  
  ## Build data frame to include 3 columns only
  data <- data[,c(2,7,outcomes[outcome])]
  names(data) <- c("hospital","state","outcome")
  data <- na.omit(data)
  
  ## rank data
  data <- data[order(data$state,data$outcome,data$hospital),]
  
  ## subset data
  data <- data[data[,"state"] == state,]

  ## Return hospital name in that state with lowest 30-day death rate
  return (data$hospital[1])
  
}