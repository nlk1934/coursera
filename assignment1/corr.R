corr <- function(directory,  threshold = 0)  {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        ## Return a numeric vector of correlations
        
        corV <- numeric(0)
        tbl <- na.omit(importCSVs(directory,1:332))
        tblx <- data.frame((table(tbl$ID)))
        colnames(tblx) <- c("id", "nobs")
        tblx <- tblx[tblx$nobs>threshold,]
        for ( i in tblx$id) {
                tblsub <- subset(tbl, ID == i, select = c("nitrate","sulfate"))
                corV <- c(corV, round(cor(tblsub[,2], tblsub[,1], use = "complete.obs"),5))
                }
        corV
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
                        formatC(i, width = 3, format = "d", flag = "0"), 
                        ".csv", sep="")
                        tbl <- rbind(tbl,read.csv(curCSV))                        
                }
        tbl
        }