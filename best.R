#This function returns the hospital with the lowest mortality rate associated with a given outcome and state.
best <- function(state, outcome) {
        ## Read outcome data
        outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcomeData[, 11] <- as.numeric(outcomeData[, 11])
        
        ## Check that state and outcome are valid
        if (sum(state == outcomeData[,7]) == 0) {
                #message("invalid state")
                stop("invalid state")
        }
        
        if (sum(outcome == c("heart attack", "heart failure", "pneumonia")) == 0) {
                #message("invalid outcome")
                stop("invalid outcome")
        }
        
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
}