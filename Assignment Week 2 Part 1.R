#COURSERA - R PROGRAMMING - ASSIGNEMENT WEEK 2
    #PART 1

#downloading dataset into working directory
dataset_url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
download.file(dataset_url, "specdata.zip")
unzip("specdata.zip", exdir = "specdata")


#Part 1: creation of a function that calculates the mean of a pollutant across a specified list of monitors
pollutantmean <- function(directory, pollutant, id=1:332) {
     #merging our data from the directory to get one dataset for all stations
     files_list <- list.files(directory, full.names=TRUE)
     dat <- do.call(rbind,lapply(files_list, read.csv))
     
     #comptuting the mean from the susbset of data containing the selected pollutant and id
     subset_dat <- dat[dat$ID %in% id,]
     round(mean(subset_dat[,pollutant], na.rm=TRUE),digits=3)
}




