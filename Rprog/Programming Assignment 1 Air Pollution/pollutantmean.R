pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
  
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
  
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
  
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    values <- numeric()
    for (i in 1:length(id)) {
        filename <- paste(formatC(id[i], format="d",width="3",
                                  flag="0"),".csv",sep="")
        url <- paste(directory,filename,sep="/")
        dframe <- as.data.frame(read.csv(url))
        values <- c(values,dframe[[pollutant]])
    }
    mean_all <- mean(values,na.rm=TRUE)
    return(mean_all)
}