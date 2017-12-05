## Coursera: Week 4, Assignment 2
## For Week Ending: Nov. 19, 2017
## User: jeshwang

#########################################################################
# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument. For example, the call
# rankhospital("MD", "heart failure", 5)
# would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
# for heart failure. The num argument can take values "best", "worst", or an integer indicating the ranking
# (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
# state, then the function should return NA. Hospitals that do not have data on a particular outcome should
# be excluded from the set of hospitals when deciding the rankings.
#
# Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
# of death. In those cases ties should be broken by using the hospital name. For example, in Texas ("TX"),
# the hospitals with lowest 30-day mortality rate for heart failure are shown here.
#########################################################################

## Clear Memory

rm(list=ls())
gc(reset=TRUE)

## Set Working Directory
setwd("C:/Users/jhwang/Documents/R/Class2/Week 4")

rankhospital <- function(state, outcome, num = "best") {
        
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
        ## Return hospital name in that state with the given rank
        df <- data[data$State == state, c(2,col)]
        names(df)[2] = "Deaths"

                
        #Remove all hospitals with Not Available values
        df[,2] <- suppressWarnings(as.numeric(df[,2]))
        df <- df[!is.na(df$Deaths),]
                
        #Order the hospitals in ascending order based on mortality rate. 
        #If hospitals are tied, sort then by alphabetical order.
                
        df_order <- df[order(df$Deaths, df$Hospital.Name),]
                
        if(class(num) == "numeric" & num > nrow(df_order)){
                return(NA)
        }
                
        if(num == "best"){
                return(df_order$Hospital.Name[1])
        }
        else if(num == "worst"){
                return(df_order$Hospital.Name[nrow(df_order)])
        }
                
                return(df_order$Hospital.Name[num])

}

## Test
rankhospital("TX","heart failure", 4)
#[1] "DETAR HOSPITAL NAVARRO"

rankhospital("MD", "heart attack", "worst")
#[1] "HARFORD MEMORIAL HOSPITAL"

rankhospital("MN", "heart attack", 5000)
#[1] NA
