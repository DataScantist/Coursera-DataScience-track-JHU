## COURSERA - R PROGRAMMING - ASSIGNEMENT WEEK 4
  
#importing data
dataset_url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"
download.file(dataset_url, "rprog3.zip")
unzip("rprog3.zip", exdir = "rprog3")

setwd(dir = "C:/Users/epadg/Desktop/poutou/R dir/rprog3")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

#1 histogram of the 30-day death rates from heart attack
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

#2 Finding the best hospital in a state
best <- function(state, outcome){
    ## Read outcome data convert outcomes to numeric
    bestdf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    bestdf[,11] <- as.numeric(bestdf[,11])
    bestdf[,17] <- as.numeric(bestdf[,17])
    bestdf[,23] <- as.numeric(bestdf[,23])
    
    ## create vector of outcomes and renaming columns
    checkoutcome <- c("heart attack","heart failure","pneumonia")
    colnames(bestdf)[c(11,17,23)] <- checkoutcome
  
    ## Check that state and outcome are valid
    if (!(state %in% unique(bestdf$State))) {
        stop("invalid state")
    }
    if (!(outcome %in% checkoutcome)) {
        stop("invalid outcome")
    }
    ##subset data to the selecterd state
    subbestdf <- subset(bestdf, bestdf$State== state)
    
    ## Return hospital name in that state with lowest 30-day death rate
        #find the lowest 30-day rate
    minrate <- min(subbestdf[,outcome],na.rm=TRUE)
        #find the correspondign hospital(s)
    hospital <- subbestdf[subbestdf[,outcome] == minrate,"Hospital.Name"]
        #find the first hospital sorted by alphabetical order
    sorthosp <- sort(hospital)
    besthosp <- sorthosp[1]
        #print the result
    print(besthosp)
    
}

# Quizz
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")


#3 Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data convert outcomes to numeric
    bestdf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    bestdf[,11] <- as.numeric(bestdf[,11])
    bestdf[,17] <- as.numeric(bestdf[,17])
    bestdf[,23] <- as.numeric(bestdf[,23])
    
    ## create vector of outcomes and renaming columns
    checkoutcome <- c("heart attack","heart failure","pneumonia")
    colnames(bestdf)[c(11,17,23)] <- checkoutcome
    
    ## Check that state and outcome are valid
    if (!(state %in% unique(bestdf$State))) {
        stop("invalid state")
    }
    if (!(outcome %in% checkoutcome)) {
        stop("invalid outcome")
    }
    
    ##subset data to the selecterd state, order by state name
    subbsetdf <- subset(bestdf, bestdf$State== state)
    subbsetdf <- subbsetdf[order(subbsetdf$Hospital.Name),] 
    
    ## Return hospital name in that state with the given rank 30-day death rate
    subbsetdf$rank <- round(rank(subbsetdf[,outcome], na.last="keep",ties.method ="first"))
    if (num=="best"){
        bestrank <- min(subbsetdf$rank,na.rm=TRUE)
        hospital <- subset(subbsetdf,rank == bestrank,select="Hospital.Name")
        hospital
        
        } else if (num=="worst")
            {
        worstrank <- max(subbsetdf$rank,na.rm=TRUE)
        hospital <- subset(subbsetdf,rank == worstrank,select="Hospital.Name")
        hospital
        
        } else if (num>length(unique(subbsetdf$Hospital.Name)))
            {
            print(NA)
            
        } else 
            {
        hospital <- subset(subbsetdf,rank == num,select="Hospital.Name")
        hospital
            }
}

#quizz 
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)


#4 Ranking hospitals in all states

rankall <- function(outcome, num = "best") {
    
    ## Read outcome data convert outcomes to numeric
    bestdf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    bestdf[,11] <- as.numeric(bestdf[,11])
    bestdf[,17] <- as.numeric(bestdf[,17])
    bestdf[,23] <- as.numeric(bestdf[,23])
    
    ## create vector of outcomes and renaming columns
    checkoutcome <- c("heart attack","heart failure","pneumonia")
    colnames(bestdf)[c(11,17,23)] <- checkoutcome
    
    ## Check that outcome is valid
    if (!(outcome %in% checkoutcome)) {
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    ordbestdf <- bestdf[order(bestdf$Hospital.Name),] 
    ordbestdf$rank <- ave(ordbestdf[,outcome], ordbestdf$State, FUN = function(x) round(rank(x, na.last="keep", ties.method = "first")))
    
    ## Return a data frame with the hospital names and the (abbreviated) state name
    states <- sort(unique(ordbestdf$State))
    rankalldf <- data.frame()
    for (i in 1:length(states)) {
        statesub <- subset(ordbestdf, ordbestdf$State== states[i])
        if (num=="best"){
            bestrank <- min(statesub$rank,na.rm=TRUE)
            hospital <- subset(statesub,rank == bestrank,select=c("Hospital.Name","State"))
        
        } else if (num=="worst")
            {
            worstrank <- max(statesub$rank,na.rm=TRUE)
            hospital <- subset(statesub,rank == worstrank,select=c("Hospital.Name","State"))
        
        } else if (num>length(unique(statesub$Hospital.Name)))
            {
            result <- c("NA",states[i])
            names(result) <- c("Hospital.Name","State")
            hospital <- t(as.data.frame(result))
        
        } else 
            {
            hospital <- subset(statesub,rank == num,select=c("Hospital.Name","State"))
        }
      
        rankalldf <- rbind(rankalldf,hospital)
        
    }
    
    return(rankalldf)
}
 
    
#test 
head(rankall("heart attack", 20),10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

#quizz
r <- rankall("heart attack", 4)
as.character(subset(r, State == "HI")[,"Hospital.Name"])

r <- rankall("pneumonia", "worst")
as.character(subset(r, State == "NJ")[,"Hospital.Name"])

r <- rankall("heart failure", 10)
as.character(subset(r, State == "NV")[,"Hospital.Name"])
