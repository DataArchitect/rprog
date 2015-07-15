corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    # Empty results to start
    sulf <- integer()
    nit <- integer()
    res <- integer()
    
    # loop round id range
    for ( fname in list.files(path = directory, full.names = TRUE) ) {
        rawdata <- read.csv(fname)
        polldata <- na.omit(rawdata)
        if ( nrow(polldata) < threshold ) {
      #      print(paste("Skipping ",fname, " as ",nrow(polldata)," < ", threshold) )
            next;
        } else {
    #        print(paste("Processing ",fname, " as ",nrow(polldata)," > ", threshold) )
        }
        sulf <- rbind(sulf,polldata["sulfate"])
        nit <- rbind(nit,polldata["nitrate"])
        res <- c( res, cor(polldata["sulfate"],polldata["nitrate"]) )
    }
    #res <- cor(sulf,nit)
    return( res )
    
}