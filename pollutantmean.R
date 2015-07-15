pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    # Empty polldata to start
    polldata <- numeric()
    
    # loop round id range
    for (f in id){
        # zero-pad filename
        fname <- paste( directory,"/", sprintf("%03d", as.numeric(f)),".csv", sep = "" )
        # Skip non-existent files
        if ( ! file.exists(fname) ) {
            print(paste("Skipping ",fname) )
            next;
        }
        rawdata <- read.csv(fname)
        bad<-is.na(rawdata[pollutant])
        # Strip out NA
        polldata <- c(polldata,rawdata[pollutant][!bad])
        
    }
    pollmean <- mean(polldata, na.rm = TRUE)
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    return (pollmean)
}
