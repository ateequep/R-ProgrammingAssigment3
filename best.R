best <- function(state, outcome) {
        outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        outcomeData[which(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack=="Not Available"),11] <- NA
        outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.rm=T)
        outcomeData[which(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=="Not Available"),17] <- NA
        outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,na.rm=T)
        outcomeData[which(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=="Not Available"),23] <- NA
        outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,na.rm=T)
        
        if(!(state %in% unique(outcomeData$State))) stop("invalid state")
        
        if(!(outcome %in% unique(c("heart attack","heart failure","pneumonia")))) stop("invalid outcome")

        outcomeData <- outcomeData[outcomeData$State==state,]
        if(outcome=="heart attack") hospitals <- outcomeData[order(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,outcomeData$Hospital.Name),]
        if(outcome=="heart failure") hospitals <- outcomeData[order(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,outcomeData$Hospital.Name),]
        if(outcome=="pneumonia")  hospitals <- outcomeData[order(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,outcomeData$Hospital.Name),]
        
        return (head(hospitals$Hospital.Name,1))
}