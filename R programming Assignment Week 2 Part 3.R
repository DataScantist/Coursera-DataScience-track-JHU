#COURSERA - R PROGRAMMING - ASSIGNEMENT WEEK 2
    #PART 3

#downloading dataset into working directory
dataset_url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
download.file(dataset_url, "specdata.zip")
unzip("specdata.zip", exdir = "specdata")


#Part 3: creation of a function that reports the number of completely observed cases in each data file as a data frame
corr <- function(directory, threshold=0){
    #merging our data from the directory to get one dataset for all stations
    files_list <- list.files(directory, full.names=TRUE)
    
    #creating empty vector that will contain the correlation for each ID if condition is verified
    corr_v <- numeric()
    for (i in seq_along(files_list)) {
        df <- read.csv(files_list[i])
        if(nrow(df[complete.cases(df),]) > threshold){
            j <- length(corr_v) + 1
            corr_v[j] <- cor(df$sulfate, df$nitrate, use = "na.or.complete")
        }    
    }
    return(corr_v)
}



#
#corr <- function(directory, threshold=0){
    
   # files_list <- list.files(directory, full.names=TRUE)
    #dat <- do.call(rbind,lapply(files_list, read.csv))
    
    
   # corr_v <- numeric()
   # for (i in length(unique(dat$ID))) {
    #    if (sum(complete.cases(dat[which(dat$ID==i),])) > threshold) {
   #         corr_v <- c(corr_v,cor(dat$nitrate,dat$sulfate,use = "complete.obs"))
   #     } 
   # }
    #return(corr_v)
#}   




