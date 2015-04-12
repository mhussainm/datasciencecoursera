complete <- function(directory, id = 1:332) {
    # initialize data.frame to hold the result
    finaldata <- data.frame(id = integer(0), nobs = integer(0))    
    i <- 1   
    # loop until the length of sequence is reached i.e. all files per the sequence are loaded
    while(i <= length(id)) {
        # format file name using sprintf for leading zeros
        formattedfilename <- paste(sprintf("%03d", id[i]), ".csv", sep = "")        
        # using complete cases to get logical vector of where complete cases are present
        tempCC <- complete.cases(read.csv(file.path(directory, formattedfilename)))
        # number of complete cases is the length of vector where value is TRUE
        finaldata[nrow(finaldata)+1, ]  <- c(id[i], length(tempCC[tempCC == TRUE]))
        i <- i + 1
    }            
    finaldata
}