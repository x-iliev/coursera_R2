rankall <- function (outcome, num=NULL) {
  
  # read data
  data <- read.csv("outcome-of-care-measures.csv", 
                   colClasses = "character")
  
  # check if state and outcome are valid
  errorCondition("Invalid State or Outcome",
                 (! outcome %in% data[,c(11,17,23)]))
  
  
  
  out <- switch(outcome, 
                "heart attack"=3, "heart failure"=4, "pneumonia"=5)
  
  d1 <- data[,c(2,7,11,17,23)]
  d1[,c(3:5)] <- apply(d1[,c(3:5)], 2, function (x) as.numeric(x))
  
  new_df <- split(d1, d1$State)
  new_df <- sapply(new_df, FUN = function(x){
    
   x <- x[order(x[[out]], x$Hospital.Name, decreasing = F), ]
    
    if (is.character(num)) {
      num <- switch(num,
                    "best"=1, "worst"= max(which(x[[out]] == max(x[[out]], 
                                                                 na.rm = T))))
    }
    
    return(x[num, c(1,2)])
    
  })
  
  return(as.data.frame(t(new_df)))
}

