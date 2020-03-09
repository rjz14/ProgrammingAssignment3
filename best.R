#This function returns the hospital with the lowest mortality rate associated with a given outcome and state.
best <- function(state, outcome) {
        ## Read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcome[, 11] <- as.numeric(outcome[, 11])
        
        ## Check that state and outcome are valid
        if sum(state == outcome[,7]) > 0 {
                message("invalid state")
                error
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
}