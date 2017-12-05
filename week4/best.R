## Coursera: Week 4, Assignment 1
## For Week Ending: Nov. 19, 2017
## User: jeshwang

#########################################################################
# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of "heart attack", "heart failure", or "pneumonia". Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
#
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals "b", "c",
# and "f" are tied for best, then hospital "b" should be returned).
#########################################################################

## Clear Memory

rm(list=ls())
gc(reset=TRUE)

## Set Working Directory
setwd("C:/Users/jhwang/Documents/R/Class2/Week 4")


best <- function(state, outcome) {
        ## Read outcome data
                data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
                if(!(state %in% data$State)){
                        stop("invalid state")
                        }
                
                if(outcome == "heart attack"){
                        col <- 11
                }
                else if(outcome == "heart failure"){
                        col <- 17
                }
                else if(outcome == "pneumonia"){
                        col <- 23
                }
                else{
                        stop("invalid outcome")
                }
                
        ## Return hospital name in that state with lowest 30-day death rate
                # create dataframe containing only hospitals in given state and their values for given outcome
                df <- data[data$State == state, c(2,col)] 
                # remove all hospitals with NA values
                df[,2] <- as.numeric(df[,2])
                df <- df[complete.cases(df),]
                
                #return a vector with all names of hospitals with the lowest mortality estimates for given outcome
                names <- df[(df[,2]) == min(df[,2]), ]$Hospital.Name
                
                #sort vector in alphabetical order and return only the first hospital name
                sort(names)[1]
}

## Test
        best("TX", "heart attack")
        # [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
        
        best("TX", "heart failure")
        # [1] "FORT DUNCAN MEDICAL CENTER"
        
        best("MD", "heart attack")
        # [1] "JOHNS HOPKINS HOSPITAL, THE"
        
        best("MD", "pneumonia")
        # [1] "GREATER BALTIMORE MEDICAL CENTER"
        
        best("BB", "heart attack")
        # Error in best("BB", "heart attack") : invalid state
        
        best("NY", "hert attack")
        # Error in best("NY", "hert attack") : invalid outcome