#COURSERA - R PROGRAMMING - ASSIGNEMENT WEEK 2
    #PART 2

#downloading dataset into working directory
dataset_url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
download.file(dataset_url, "specdata.zip")
unzip("specdata.zip", exdir = "specdata")


#Part 2: creation of a function that reports the number of completely observed cases in each data file as a data frame
complete <- function(directory, id=1:332) {
    #merging our data from the directory to get one dataset for all stations
    files_list <- list.files(directory, full.names=TRUE)
    dat <- do.call(rbind,lapply(files_list, read.csv))
    
    #subset only the non missing observations and count them for each id in a data frame
    count <- as.data.frame.table(tapply(complete.cases(dat[,c("sulfate","nitrate")]), dat$ID, FUN=sum))
    colnames(count) <- c("id", "nobs")
    print(count[id,])
}


set.seed(42)
cc <- complete("specdata/specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])