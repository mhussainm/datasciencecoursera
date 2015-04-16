rankall <- function(outcome, num = "best") {
    ## Check validity of arguements
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    impColNrs <- c(2, 7)
    outcomeColNrs30DayDeathMortalityRates <- c(11, 17, 23)
    vData <- data.frame(validOutcomes, outcomeColNrs30DayDeathMortalityRates)
    if(!(outcome %in% validOutcomes)) {
        stop("invalid outcome")
    }
    
    colNrOutcome30DDMR <- as.numeric(vData[(vData$validOutcomes == outcome), 2])
    
    ## Read outcome data
    hData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## factorize necessary columns
    hData[[7]] <- as.factor(hData[[7]]) #$ State
    hData[[colNrOutcome30DDMR]] <- as.numeric(hData[[colNrOutcome30DDMR]]) #$ 'outcome' argument's column
    ## get necessary columns, remove NA
    hData <- hData[(!is.na(hData[[colNrOutcome30DDMR]])), c(2, 7, colNrOutcome30DDMR)]
    ## sort by $State, $Rate, $Hospital.Name in order
    hData <- hData[order(hData[2], hData[3], hData[1]), ]
    ## split data by $State
    sData <- split(hData, hData$State)
    
    resultFrame <- data.frame(hospital=character(), state=character(), stringsAsFactors=FALSE)
    ## loop through all split data.frames
    i <- 1
    for(sd in sData) {
        ## Return hospital name in that state with required 30-day death rate
        returnVal <- NA
        if(num == "best") {
            returnVal <- as.character(sd[1, 1])
        }
        else if(num == "worst") {
            returnVal <- as.character(sd[nrow(sd), 1])
        }
        else if(!is.nan(num) & num <= nrow(sd)) {
            returnVal <- as.character(sd[num, 1])
        }
        resultFrame[nrow(resultFrame) + 1, ] <- c(returnVal, levels(sd$State)[i]) 
        i <- i + 1
    }
    resultFrame 
}