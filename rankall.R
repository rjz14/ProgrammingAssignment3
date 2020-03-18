rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcomeData[, 11] <- as.numeric(outcomeData[, 11])
        outcomeData[, 17] <- as.numeric(outcomeData[, 17])
        outcomeData[, 23] <- as.numeric(outcomeData[, 23])
        
        ## Check that outcome is valid
        if (sum(outcome == c("heart attack", "heart failure", "pneumonia")) == 0) {
                #message("invalid outcome")
                stop("invalid outcome")
        }
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        if (outcome == "heart attack") {
                noNAOutcomeData <- cbind.data.frame(outcomeData[, 2][!is.na(outcomeData[, 11])], outcomeData[, 7][!is.na(outcomeData[, 11])], as.numeric(outcomeData[, 11][!is.na(outcomeData[, 11])]), stringsAsFactors = FALSE)
        } else if (outcome == "heart failure") {
                noNAOutcomeData <- cbind.data.frame(outcomeData[, 2][!is.na(outcomeData[, 17])], outcomeData[, 7][!is.na(outcomeData[, 17])], as.numeric(outcomeData[, 17][!is.na(outcomeData[, 17])]), stringsAsFactors = FALSE)
        } else if (outcome == "pneumonia") {
                noNAOutcomeData <- cbind.data.frame(outcomeData[, 2][!is.na(outcomeData[, 23])], outcomeData[, 7][!is.na(outcomeData[, 23])], as.numeric(outcomeData[, 23][!is.na(outcomeData[, 23])]), stringsAsFactors = FALSE)
        }
        
        stateList <- unique(noNAOutcomeData[,2])
        stateList <- stateList[sort.list(stateList)]
        
        hospitals <- vector("character", length(stateList))
        
        for(i in seq_along(stateList)) {
                noNAOutcomeDataState <- cbind.data.frame(noNAOutcomeData[,1][noNAOutcomeData[,2] == stateList[i]], noNAOutcomeData[,2][noNAOutcomeData[,2] == stateList[i]], noNAOutcomeData[,3][noNAOutcomeData[,2] == stateList[i]], stringsAsFactors = FALSE)
                noNAOutcomeDataState <- noNAOutcomeDataState[order(noNAOutcomeDataState[,3], noNAOutcomeDataState[,1]),]
                
                if (num == "best") {
                        numState <- 1
                } else if (num == "worst") {
                        numState <- nrow(noNAOutcomeDataState)
                } else {
                        numState <- num
                }
                
                if (numState > nrow(noNAOutcomeDataState)){
                        hospitals[i] <- NA
                } else{
                        hospitals[i] <- noNAOutcomeDataState[numState, 1]
                }
                
                
        }
        
        return(cbind.data.frame(hospital = hospitals, state = stateList))
        
        
}