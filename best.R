best <- function (state, outcome) {
  
  # read data
  data <- read.csv("outcome-of-care-measures.csv", 
                   colClasses = "character")
  
  # check if state and outcome are valid
  errorCondition("Invalid State or Outcome", 
                 (! state %in% data$State | 
                    (! outcome %in% data[,c(11,17,23)])))
  
  
# return the hospital with the lowest outcome in that state
  out <- switch(outcome, 
                "heart attack"=11, "heart failure"=17, "pneumonia"=23)
  
  d1 <- data[data$State == state, ]
  print(d1$Hospital.Name[ which(d1[,out] == min(d1[,out], na.rm=T))])
  #d2 <- d1$Hospital.Name[min(d1[,out], na.rm = T)] 
  #print(d2)
}


