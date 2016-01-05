library(data.table)

rankall <- function(outcome, num="best") {
        outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        outcomeData[which(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack=="Not Available"),11] <- NA
        outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.rm=T)
        outcomeData[which(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=="Not Available"),17] <- NA
        outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,na.rm=T)
        outcomeData[which(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=="Not Available"),23] <- NA
        outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,na.rm=T)
        
        if(!(state %in% unique(outcomeData$State))) stop("invalid state")
        
        if(!(outcome %in% unique(c("heart attack","heart failure","pneumonia")))) stop("invalid outcome")
        
        #outcomeData <- outcomeData[outcomeData$State==state,]
        if(outcome=="heart attack") hospitals <- outcomeData[order(outcomeData$State,outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,outcomeData$Hospital.Name),c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
        if(outcome=="heart failure") hospitals <- outcomeData[order(outcomeData$State,outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,outcomeData$Hospital.Name),c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
        if(outcome=="pneumonia")  hospitals <- outcomeData[order(outcomeData$State,outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,outcomeData$Hospital.Name),c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
        
        hospitals <- hospitals[complete.cases(hospitals),]
        hospitals <- as.data.table(hospitals)
        setnames(hospitals,c("state","hospital","rate"))
        hospitals <- hospitals[,.(hospital,state)]
        ifelse(num=="best",return(as.data.frame(setcolorder(hospitals[,.SD[1],by=state],c("hospital","state")))),
               ifelse(num=="worst",return(as.data.frame(setcolorder(hospitals[,.SD[.N],by=state],c("hospital","state")))),
                      ifelse((num > nrow(hospitals)), return(NA), return(as.data.frame(setcolorder(hospitals[,.SD[num],by=state],c("hospital","state")))
                                                                         ))))
        
        #return (head(hospitals$Hospital.Name,1))
}