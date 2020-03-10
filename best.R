#This function returns the hospital with the lowest mortality rate associated with a given outcome and state.
best <- function(state, outcome) {
        ## Read outcome data
        outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcomeData[, 11] <- as.numeric(outcomeData[, 11])
        outcomeData[, 17] <- as.numeric(outcomeData[, 17])
        outcomeData[, 23] <- as.numeric(outcomeData[, 23])
        
        ## Check that state and outcome are valid
        if (sum(state == outcomeData[, 7]) == 0) {
                #message("invalid state")
                stop("invalid state")
        }
        
        if (sum(outcome == c("heart attack", "heart failure", "pneumonia")) == 0) {
                #message("invalid outcome")
                stop("invalid outcome")
        }
        
        if (outcome == "heart attack") {
                noNAOutcomeData <- cbind(outcomeData[, 2][!is.na(outcomeData[, 11])], outcomeData[, 11][!is.na(outcomeData[, 11])])
                
        } else if (outcome == "heart failure") {
                noNAOutcomeData <- cbind(outcomeData[, 2][!is.na(outcomeData[, 17])], outcomeData[, 17][!is.na(outcomeData[, 17])])
        } else if (outcome == "pneumonia") {
                noNAOutcomeData <- cbind(outcomeData[, 2][!is.na(outcomeData[, 23])], outcomeData[, 23][!is.na(outcomeData[, 23])])
        }
        minMortRate <- min(noNAOutcomeData[, 2])
        bestHospitalsBoolean <- minMortRate == noNAOutcomeData[, 2]
        bestHospitals <- noNAOutcomeData[, 1][bestHospitalsBoolean]
        ## Return hospital name in that state with lowest 30-day death
        ## rate
}