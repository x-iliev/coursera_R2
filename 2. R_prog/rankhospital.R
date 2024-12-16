rankhospital <- function (state, outcome, num=NULL) {
  
  # read data
  data <- read.csv("outcome-of-care-measures.csv", 
                   colClasses = "character")
  
  # check if state and outcome are valid
  errorCondition("Invalid State or Outcome", 
                 (! state %in% data$State | 
                    (! outcome %in% data[,c(11,17,23)])))
  
  
  
  out <- switch(outcome, 
                "heart attack"=3, "heart failure"=4, "pneumonia"=5)
  
  d1 <- data[,c(2,7,11,17,23)]
  d1[,c(3:5)] <- apply(d1[,c(3:5)], 2, function (x) as.numeric(x))
  d1 <- d1[d1$State == state, ]
  d1 <- d1[order(as.numeric(d1[[out]]), d1$Hospital.Name, na.last = T, decreasing = F), ]
  
  if (is.character(num)) {
  num <- switch(num,
         "best"=1, "worst"= max(which(d1[[out]] == max(d1[[out]], na.rm = T))))
  }
  
  print(d1$Hospital.Name[num])
         
  
}