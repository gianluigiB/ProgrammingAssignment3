best <- function(state, outcome) {
  
  ## Read outcome data, only those needed, and coerce to character or numeric as needed
  outcomeCareMeasures <- subset(read.csv("outcome-of-care-measures.csv"), select=(c(2,7,11,17,23)), na.strings=c("Not Available","NA"))
  outcomeCareMeasures[ ,1] <- as.character(levels(outcomeCareMeasures[ , 1]))[outcomeCareMeasures[ , 1]]
  options(warn=-1)
  outcomeCareMeasures[ ,3] <- as.numeric(levels(outcomeCareMeasures[ , 3]))[outcomeCareMeasures[ , 3]]
  outcomeCareMeasures[ ,4] <- as.numeric(levels(outcomeCareMeasures[ , 4]))[outcomeCareMeasures[ , 4]]
  outcomeCareMeasures[ ,5] <- as.numeric(levels(outcomeCareMeasures[ , 5]))[outcomeCareMeasures[ , 5]]
  options(warn=-0)
  
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
    ## Return hospital name in that state with lowest 30-day death rate
    measureID <- match(outcome,supportedOutcome) + 2
    selectedHospitalsRate <- outcomeCareMeasures[outcomeCareMeasures$State==state, c(1,measureID)]
    return(selectedHospitalsRate[order(selectedHospitalsRate[,2]),][1,1])
  }
}