##
## This function reads the outcome-of-care-measures.csv and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
## and ranking
##
## Author: S. Eannuzzi
## Created: April 23, 2017
##
## Usage:
## rankhospital(state,["heart attack" | "heart failure"| "pneumonia"],ranking [best | worst | number] )
##
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomes <-
    read.csv("outcome-of-care-measures.csv",
             colClasses = "character",
             na.strings = "Not Available")
  ## Check that state and outcome are valid
  if (missing(state)) {
    # Should be a different error message, but as per requirement
    stop("invalid state")
  }
  if (missing(outcome)) {
    # Should be a different error message, but as per requirement
    stop("No outcome specified.")
  }
  if (!any(outcomes$State == state)) {
    stop("invalid state")
  }
  if (!any(outcome == c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  ##
  ## Fixed numeric as String
  ##
  suppressWarnings(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <-
    as.numeric(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
  suppressWarnings(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <-
    as.numeric(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
  suppressWarnings(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <-
    as.numeric(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))

  ### Some housekeeping, get col index #'s
  
  hospitalNameIndex = match("Hospital.Name",names(outcomes))
  
  if (outcome == "heart attack") {
    rateColIndex = match("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",names(outcomes))
  }
  if (outcome == "heart failure") {
    rateColIndex = match("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",names(outcomes))
  }
  if (outcome == "pneumonia") {
    rateColIndex = match("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",names(outcomes))
  }
  # Get just what we need.  dat will have: Col 1 is hospital name, col 2 is rate
  dat <- subset(outcomes,State==state & !is.na(outcomes[rateColIndex]),select=c(hospitalNameIndex,rateColIndex))
  
  # default rank
  theRank = 1
  
  #looking for shortcuts
  if(num == "best") {
    theRank = 1
    # Sort ascending by rate and hospital
    dat1 <- dat[order(dat[2],dat[1],decreasing=FALSE),]
  } 
  
  if(num == "worst") {
    theRank = 1
    # gotta reverse name so it is correctly sorted (not descending)
    dat1 <- dat[order(rev(dat[2]),dat[1],decreasing=TRUE),]
  }
  
  ## else grab rank from parameter
  if (!any(num == c("best","worst")) & length(num)>0 & is.numeric(num)) {
    theRank = as.numeric(num)
    #sort ascending by rate and hospital
    dat1 <- dat[order(dat[2],dat[1],decreasing=FALSE),]
  }


   ## Bad rank request... and NA return code.  Should have given a message, but to spec
  if (theRank > nlevels(factor(dat1$Hospital.Name)))  {
    # message("Bad num passed, larger than # of hospitals")
    return(NA)
  }
  
  #return hospital name
  return(dat1[theRank,1])
}