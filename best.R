##
## This function reads the outcome-of-care-measures.csv and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
##
## Author: S. Eannuzzi
## Created: April 23, 2017
##
## Usage:
## best(state,["heart attack" | "heart failure"| "pneumonia"] )
##
best <- function(state, outcome) {
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
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  #
  # Fixed numeric as String
  outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <-
    as.numeric(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <-
    as.numeric(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <-
    as.numeric(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  
  if (outcome == "heart attack") {
    # message("Heart Attack")
    hmin <-
      min(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[outcomes$State ==
                                                                               state],
          na.rm = TRUE)
    return(sort(outcomes$Hospital.Name[outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == hmin &
                                         outcomes$State == state]))
  }
  if (outcome == "heart failure") {
    #message("Heart Failure")
    hmin <-
      min(
        outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[outcomes$State ==
                                                                              state]
        ,
        na.rm = TRUE
      )
    return(sort(outcomes$Hospital.Name[outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == hmin &
                                         outcomes$State == state]))
  }
  if (outcome == "pneumonia") {
    #message("pnuemonia")
    hmin <-
      min(outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[outcomes$State ==
                                                                            state]
          ,
          na.rm = TRUE)
    return(sort(outcomes$Hospital.Name[outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == hmin &
                                         outcomes$State == state]))
  }
}
#
# Test scenarios
#
#source("best.R")
#best("TX", "heart attack")
# "CYPRESS FAIRBANKS MEDICAL CENTER"
#best("TX", "heart failure")
#"FORT DUNCAN MEDICAL CENTER"
#best("MD", "heart attack")
#"JOHNS HOPKINS HOSPITAL, THE"
#best("MD", "pneumonia")
#"GREATER BALTIMORE MEDICAL CENTER"
#best(outcome = "heart attack")
# Need to test for missing
#Error in best(outcome="heart attack") : invalid state
#best("BB", "heart attack")
#Error in best("BB", "heart attack") : invalid state
#best("NY", "hert attack")
#Error in best("NY", "hert attack") : invalid outcome
#
# Test Prep
# Clear workspace
#rm(list=ls())
## Go to project directory
#setwd("D:\\Users\\seannuzzi\\Documents\\R Studio Documents\\coursera\\programmingr\\week4")
##Read dataset
#outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
## and take a peek
#head(outcome)
#ncol(outcome)
##coerce string to  number.  Always needed with CSV's
#outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
#hist(outcome[, 11])
