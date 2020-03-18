rankhospital <- function(state, outcome, num = "best") {
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
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if (outcome == "heart attack") {
                noNAOutcomeData <- cbind.data.frame(outcomeData[, 2][!is.na(outcomeData[, 11])], outcomeData[, 7][!is.na(outcomeData[, 11])], as.numeric(outcomeData[, 11][!is.na(outcomeData[, 11])]), stringsAsFactors = FALSE)
        } else if (outcome == "heart failure") {
                noNAOutcomeData <- cbind.data.frame(outcomeData[, 2][!is.na(outcomeData[, 17])], outcomeData[, 7][!is.na(outcomeData[, 17])], as.numeric(outcomeData[, 17][!is.na(outcomeData[, 17])]), stringsAsFactors = FALSE)
        } else if (outcome == "pneumonia") {
                noNAOutcomeData <- cbind.data.frame(outcomeData[, 2][!is.na(outcomeData[, 23])], outcomeData[, 7][!is.na(outcomeData[, 23])], as.numeric(outcomeData[, 23][!is.na(outcomeData[, 23])]), stringsAsFactors = FALSE)
        }
        
        noNAOutcomeData <- cbind.data.frame(noNAOutcomeData[,1][noNAOutcomeData[,2] == state], noNAOutcomeData[,2][noNAOutcomeData[,2] == state], noNAOutcomeData[,3][noNAOutcomeData[,2] == state], stringsAsFactors = FALSE)
        noNAOutcomeData <- noNAOutcomeData[order(noNAOutcomeData[,3], noNAOutcomeData[,1]),]
        
        if (num == "best") {
                num <- 1
        } else if (num == "worst") {
                num <- nrow(noNAOutcomeData)
        } 
        
        if (num > nrow(noNAOutcomeData)){
                hospital <- NA
        } else{
                hospital <- noNAOutcomeData[num, 1]
        }
        
        return(hospital)
}