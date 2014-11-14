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
        
        tbl <- importCSVs(directory,id)
        round(mean(tbl[,pollutant], na.rm = TRUE), digits = 3)
}

importCSVs <- function (directory, id = 1:332) {
        # validating input of id
        if (max(id) > 332 | min(id) < 1) stop("out of the files range! \n")
        
        # initialize an empty data frame -- "tbl"         
        colClasses <- c("numeric","numeric","numeric","numeric")
        col.names <- c("Date", "sulfate", "nitrate", "ID")
        tbl <- read.table(text = "",colClasses = colClasses,col.names = col.names)       

        #read and combine files
        for (i in id){
                curCSV <- paste(directory, "/", 
                                formatC(i,width = 3, format ="d", flag = "0"), 
                                ".csv", sep="")
                tbl <- rbind(tbl,read.csv(curCSV))                        
        }
        tbl
}
