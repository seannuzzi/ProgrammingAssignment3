##
## This function reads the outcome-of-care-measures.csv and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
## and ranking for each state
##
## Author: S. Eannuzzi
## Created: April 23, 2017
##
## Usage:
## rankAll(["heart attack" | "heart failure"| "pneumonia"],ranking [best | worst | number] )
##
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomes <-
    read.csv("outcome-of-care-measures.csv",
             colClasses = "character",
             na.strings = "Not Available")
  ## Check to see if outcome are valid
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
  
  stateIndex = match("State",names(outcomes))
  
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
  
  # Get a list of all the states sorted
  states <- sort(unique(outcomes[,stateIndex]))
  
  # Get just what we need.  dat will have: Col 1 is State, Col 2 is hospital name, col 4 is rate
  dat <- subset(outcomes,select=c(stateIndex, hospitalNameIndex,rateColIndex))
  
  ### 
  ### Function returns the hospital name for the given state at the specified rank.
  ###
  per_state <- function(state) {
    dats <- subset(dat, State==state)
    # default rank
    theRank = 1
    
    #looking for shortcuts
    if(num == "best") {
      theRank = 1
      # Sort ascending by rate and hospital
      dat1 <- dats[order(dats[3],dats[2],decreasing=FALSE),]
    } 
    
    if(num == "worst") {
      theRank = 1
      # should reverse name so it is correctly sorted (not descending)
      dat1 <- dats[order(dats[3],dats[2],decreasing=TRUE),]
    }
    
    ## else grab rank from parameter
    if (!any(num == c("best","worst")) & length(num)>0 & is.numeric(num)) {
      theRank = as.numeric(num)
      #sort ascending by rate and hospital
      dat1 <- dats[order(dats[3],dats[2],decreasing=FALSE),]
    }
    
    
    ## Bad rank request... and NA return code.  Should have given a message, but to spec
    if (theRank > nlevels(factor(dats$Hospital.Name)))  {
      # message("Bad num passed, larger than # of hospitals")
      return(NA)
    }
    
    return(dat1[theRank,])
  }
  per_state_data <- lapply(states, per_state)
  df <- do.call("rbind", per_state_data)
  # label due to NA's
  colnames(df) <- c("state","hospital", "rate")
  df$state <- states
  row.names(df) <- states
  #message(class(df))
  # return proper format
  return (df[,c(2,1)])
} 

# Test cases

#head(rankall("heart attack", 20), 10)
#hospital state
#AK                                <NA>    AK
#AL      D W MCMILLAN MEMORIAL HOSPITAL    AL
#AR   ARKANSAS METHODIST MEDICAL CENTER    AR
#AZ JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
#CA               SHERMAN OAKS HOSPITAL    CA
#CO            SKY RIDGE MEDICAL CENTER    CO
#CT             MIDSTATE MEDICAL CENTER    CT
#DC                                <NA>    DC
#DE                                <NA>    DE
#FL      SOUTH FLORIDA BAPTIST HOSPITAL    FL

#tail(rankall("pneumonia", "worst"), 3)
#WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
#WV                     PLATEAU MEDICAL CENTER    WV
#WY           NORTH BIG HORN HOSPITAL DISTRICT    WY

#tail(rankall("heart failure"), 10)
#hospital state
#TN                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
#TX                                        FORT DUNCAN MEDICAL CENTER    TX
#UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
#VA                                          SENTARA POTOMAC HOSPITAL    VA
#VI                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
##VT                                              SPRINGFIELD HOSPITAL    VT
#WA                                         HARBORVIEW MEDICAL CENTER    WA
#WI                                    AURORA ST LUKES MEDICAL CENTER    WI
#WV                                         FAIRMONT GENERAL HOSPITAL    WV
#WY                                        CHEYENNE VA MEDICAL CENTER    WY





