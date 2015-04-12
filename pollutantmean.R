pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    i <- 1
    # read the first CSV file per the sequence (id) passed as parameter
    pollutiondata <- read.csv(file.path(directory, paste(sprintf("%03d", id[i]), ".csv", sep = "")))
    i <- i + 1    
    # loop until the length of sequence is reached i.e. all files per the sequence are loaded
    while(i <= length(id)) {
        # format file name using sprintf for leading zeros
        formattedfilename <- paste(sprintf("%03d", id[i]), ".csv", sep = "")        
        # using rbind to merge the data.frames read by read.csv into a single data.frame                
        pollutiondata <- rbind(pollutiondata, read.csv(file.path(directory, formattedfilename)))          
        i <- i + 1
    }
    
    round(mean(pollutiondata[, pollutant], na.rm = TRUE), digits = 3)
}