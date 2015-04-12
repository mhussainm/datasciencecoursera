corr <- function(directory, threshold = 0) {
    resultvec <- numeric()
    cclist <- read.csv(file.path(directory, "cclist.csv"))
    tmpcclist <- cclist[cclist$nobs >= threshold, ]    
    if(length(tmpcclist$id) > 0) {
        for(i in seq(length(tmpcclist$id))) {
            # format file name using sprintf for leading zeros
            formattedfilename <- paste(sprintf("%03d", tmpcclist$id[i]), ".csv", sep = "")        
            # using complete cases to get logical vector of where complete cases are present
            tempdata <- read.csv(file.path(directory, formattedfilename))
            tempdata <- tempdata[complete.cases(tempdata), ]
            
            resultvec[i] <- cor(x = tempdata$nitrate, y = tempdata$sulfate)
            #resultvec[i] <- round(cor(x = tempdata$nitrate, y = tempdata$sulfate), digits = 5)
            
        }
    }
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0sr
    
    ## Return a numeric vector of correlations
    resultvec <- resultvec[!is.na(resultvec)]
    resultvec
}