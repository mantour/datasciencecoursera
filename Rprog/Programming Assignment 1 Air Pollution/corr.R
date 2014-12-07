corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    result <- numeric()
    for (i in 1:332){
        filename <- paste(formatC(i, format="d",width="3",
                                  flag="0"),".csv",sep="")
        url <- paste(directory,filename,sep="/")
        data <- as.data.frame(read.csv(url))
        if (sum(complete.cases(data)) > threshold){
            co <- cor(x=data[["sulfate"]],y=data[["nitrate"]],use="complete.obs")
            result <- c(result,co)
        }
    }
    return(result)
}