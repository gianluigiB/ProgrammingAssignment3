rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data, only those needed, and coerce to character or numeric as needed
  outcomeCareMeasures <- subset(read.csv("outcome-of-care-measures.csv"), select=(c(2,7,11,17,23)), na.strings=c("Not Available","NA"))
  outcomeCareMeasures[ ,1] <- as.character(levels(outcomeCareMeasures[ , 1]))[outcomeCareMeasures[ , 1]]
  options(warn=-1)
  outcomeCareMeasures[ ,3] <- as.numeric(levels(outcomeCareMeasures[ , 3]))[outcomeCareMeasures[ , 3]]
  outcomeCareMeasures[ ,4] <- as.numeric(levels(outcomeCareMeasures[ , 4]))[outcomeCareMeasures[ , 4]]
  outcomeCareMeasures[ ,5] <- as.numeric(levels(outcomeCareMeasures[ , 5]))[outcomeCareMeasures[ , 5]]
  options(warn=0)
  
    ## Define valid argument values
  supportedOutcome <- c("heart attack", "heart failure", "pneumonia")
  supportedStates <- levels(factor(outcomeCareMeasures$State))
  
  ## Check that state and outcome are valid
  if (!(state %in% supportedStates)) {
    stop("invalid state", call.=TRUE)
  }
  else
    if (!(outcome %in% supportedOutcome)) {
      stop("invalid outcome", call.=TRUE)
    }
  else
  {
    ## Return hospital name in that state with requested rank (=num) for 30-day death rate
    measureID <- match(outcome,supportedOutcome) + 2
    selectedHospitalsRateWithNA <- outcomeCareMeasures[outcomeCareMeasures$State==state, c(1,measureID)]
    selectedHospitalsRate <- selectedHospitalsRateWithNA[complete.cases(selectedHospitalsRateWithNA),]
    
    ##set requested rank
    if (num == "best") rank <- 1 else 
      if (num == "worst") rank <- nrow(selectedHospitalsRate) else
        if (is.nan(as.numeric(num))) return ("NA") else
          if (as.numeric(num) > nrow(selectedHospitalsRate)  | num < 1) return ("NA") else
            rank <- num
    
    return(selectedHospitalsRate[order(selectedHospitalsRate[,2], selectedHospitalsRate[,1]),][rank,1])
  }
}