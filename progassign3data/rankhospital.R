rankhospital <- function(state, outcome, num = "best") {
    ## Check validity of arguements
    stateCodes <- read.csv("statescodes.csv")
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    outcomeColNrs30DayDeathMortalityRates <- c(11, 17, 23)
    vData <- data.frame(validOutcomes, outcomeColNrs30DayDeathMortalityRates)
    if(!(state %in% stateCodes$State)) {
        stop("invalid state")
    }
    else if(!(outcome %in% validOutcomes)) {
        stop("invalid outcome")
    }
    
    colNrOutcome30DDMR <- as.numeric(vData[(vData$validOutcomes == outcome), 2])
    
    ## Read outcome data
    hData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## extract data for given state and only get the column for given outcome
    exData <- hData[(hData$State == state & hData[colNrOutcome30DDMR] != "Not Available"), c(2, colNrOutcome30DDMR)]
    
    exDataT <- data.frame(exData[, 1], as.numeric(exData[, 2]))
    
    exDataSorted <- exDataT[order(exDataT[2], exDataT[1]), ]
    
    ## Return hospital name in that state with required 30-day death rate
    returnVal <- NA
    if(num == "best") {
        returnVal <- as.character(exDataSorted[1, 1])
    }
    else if(num == "worst") {
        returnVal <- as.character(exDataSorted[nrow(exDataSorted), 1])
    }
    else if(!is.nan(num) & num <= nrow(exDataSorted)) {
        returnVal <- as.character(exDataSorted[num, 1])
    }
    returnVal
}