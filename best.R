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
  
  data[data$State == state & data[,out)[min(data[,out], na.rm = T)]
  
  
    
    
}


