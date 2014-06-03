rankall <- function(outcome, num = "best") {
  
  ## Read outcome data, only those needed, and coerce to character or numeric as needed
  outcomeCareMeasures <- subset(read.csv("outcome-of-care-measures.csv"), select=(c(2,7,11,17,23)), na.strings=c("Not Available","NA"))
  outcomeCareMeasures[ ,1] <- as.character(levels(outcomeCareMeasures[ , 1]))[outcomeCareMeasures[ , 1]]
  outcomeCareMeasures[ ,2] <- as.character(levels(outcomeCareMeasures[ , 2]))[outcomeCareMeasures[ , 2]]
  options(warn=-1)
  outcomeCareMeasures[ ,3] <- as.numeric(levels(outcomeCareMeasures[ , 3]))[outcomeCareMeasures[ , 3]]
  outcomeCareMeasures[ ,4] <- as.numeric(levels(outcomeCareMeasures[ , 4]))[outcomeCareMeasures[ , 4]]
  outcomeCareMeasures[ ,5] <- as.numeric(levels(outcomeCareMeasures[ , 5]))[outcomeCareMeasures[ , 5]]
  options(warn=0)
  
  ## Define valid argument values
  supportedOutcome <- c("heart attack", "heart failure", "pneumonia")
  #supportedStates <- levels(factor(outcomeCareMeasures$State))
  
  ## Check that state and outcome are valid
  if (!(outcome %in% supportedOutcome)) stop("invalid outcome", call.=TRUE) else
  {
    ## Return state list with hospital name, state, outcome for 30-day death rate
    measureID <- match(outcome,supportedOutcome) + 2
    selectedHospitalsRateWithNA <- outcomeCareMeasures[, c(1, 2, measureID)]
    selectedHospitalsRate <- selectedHospitalsRateWithNA[complete.cases(selectedHospitalsRateWithNA),]
    selectedHospitalRateSplit <- split(selectedHospitalsRate, selectedHospitalsRate[,2])
    
    ##Loop over each state of the list and get the requested ranked hospital per given outcome
    ##set requested rank
    if (num!="worst" & num!="best" | num < 1) stop("Invalid argument \"num\". Supported values: \"best\";\"worst\";positive integer") else
      {
        ##allocate data frame to be returned loop over each state of the list an
        daf <- data.frame(character(), character())
        dafNA <- data.frame(character(), character())
                
        for(i in 1:length(selectedHospitalRateSplit)) 
        {
          rank <- 0
          if (num == "worst") rank <- nrow(selectedHospitalRateSplit[[i]])
          if (num == "best") rank <- 1
          if (num!="worst" & num!="best") if(num <= nrow(selectedHospitalRateSplit[[i]])) rank <- num
          
          if (rank > 0) #state has hospital at that rank
          daf <- rbind(daf, selectedHospitalRateSplit[[i]][order(selectedHospitalRateSplit[[i]][,3], selectedHospitalRateSplit[[i]][,1]),][rank,1:2]) 
          else
          {
            #state has no hospital at that rank
            dafNA <- data.frame(NA,selectedHospitalRateSplit[[i]][,2][1])
            names(dafNA) <- names(selectedHospitalsRate)[1:2]
            daf <- rbind(daf, dafNA)
          }  
        }
      }
    }
    row.names(daf) <- names(selectedHospitalRateSplit)
    names(daf) <- c("hospital", "state")
    return(daf)
}