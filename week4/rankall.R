## Coursera: Week 4, Assignment 3
## For Week Ending: Nov. 19, 2017
## User: jeshwang

#########################################################################
# Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking
# (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num. For example the function call
# rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
# are the best in their respective states for 30-day heart attack death rates. The function should return a value
# for every state (some may be NA). The first column in the data frame is named hospital, which contains
# the hospital name, and the second column is named state, which contains the 2-character abbreviation for
# the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
# hospitals when deciding the rankings.
# 
# The rankall function should handle ties in the 30-day mortality rates in the same way
# that the rankhospital function handles ties.
# 
# The function should check the validity of its arguments. If an invalid outcome value is passed to rankall,
# the function should throw an error via the stop function with the exact message "invalid outcome". The num
# variable can take values "best", "worst", or an integer indicating the ranking (smaller numbers are better).
# If the number given by num is larger than the number of hospitals in that state, then the function should
# return NA.
#########################################################################

## Clear Memory

rm(list=ls())
gc(reset=TRUE)

## Set Working Directory
setwd("C:/Users/jhwang/Documents/R/Class2/Week 4")

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ##Check that state and outcome are valid
        
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
        
        ##For each state, find the hospital of the given rank
        df <- data[ , c(2, 7,col)]
        names(df) <- c("Hospital", "State", "Deaths")
        
        ##Remove all hospitals with Not Available values
        df[,3] <- suppressWarnings(as.numeric(df[,3]))
        df <- df[!is.na(df$Deaths),]
        
        ##Rank hospitals within each state
        df_order <- df[order(df$State, df$Deaths, df$Hospital), ]
        
        ##Return a dataframe with the hospital names and abbreviated state names
        df_split <- split(df_order, df_order$State)
        
        df_final <- lapply(df_split, function(x,num) {
                if(class(num) == "character"){
                        if(num == "best"){
                                return(x$Hospital[1])
                        }
                        else if (num == "worst"){
                                return(x$Hospital[nrow(x)])
                        }
                }
                else {
                        if(num > nrow(x)){
                                return(NA)
                        }
                        else {
                                return(x$Hospital[num])
                        }
                }
        }, num)
        
        return (data.frame(hospital=unlist(df_final), state=names(df_final)) )
}

##Test

head(rankall("heart attack", 20),10)
# hospital state
# AK                                <NA>    AK
# AL      D W MCMILLAN MEMORIAL HOSPITAL    AL
# AR   ARKANSAS METHODIST MEDICAL CENTER    AR
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
# CA               SHERMAN OAKS HOSPITAL    CA
# CO            SKY RIDGE MEDICAL CENTER    CO
# CT             MIDSTATE MEDICAL CENTER    CT
# DC                                <NA>    DC
# DE                                <NA>    DE
# FL      SOUTH FLORIDA BAPTIST HOSPITAL    FL

tail(rankall("pneumonia", "worst"),3)
# hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
# WV                     PLATEAU MEDICAL CENTER    WV
# WY           NORTH BIG HORN HOSPITAL DISTRICT    WY

tail(rankall("heart failure"), 10)
# hospital state
# TN                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
# TX                                        FORT DUNCAN MEDICAL CENTER    TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
# VA                                          SENTARA POTOMAC HOSPITAL    VA
# VI                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
# VT                                              SPRINGFIELD HOSPITAL    VT
# WA                                         HARBORVIEW MEDICAL CENTER    WA
# WI                                    AURORA ST LUKES MEDICAL CENTER    WI
# WV                                         FAIRMONT GENERAL HOSPITAL    WV
# WY                                        CHEYENNE VA MEDICAL CENTER    WY
