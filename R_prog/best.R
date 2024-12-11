best <- function (state, outcome, num=NULL) {
  
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
  
  if (is.null(num)) {
    
  num <- which( as.numeric(d1[[out]]) == min(as.numeric(d1[[out]]), na.rm=TRUE) )
  }
  print(min(d1$Hospital.Name[ num ]))
  
}


