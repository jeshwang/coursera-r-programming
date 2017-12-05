## Coursera: Week 2, Assignment 1-1
## For Week Ending: Nov. 5, 2017
## User: jeshwang
#########################################################################
## Write a function named 'pollutantmean' that calculates the mean of a pollutant 
## (sulfate or nitrate) across a specified list of monitors.
## The function 'pollutantmean' takes three arguments: 
## directory', 'pollutant', and 'id'. 
## 
## Given a vector monitor ID numbers, 
## pollutantmean' reads that monitors' particulate matter data from the directory 
## specified in the 'directory' argument and returns the mean of the pollutant 
## across all of the monitors, ignoring any missing values coded as NA. 
#########################################################################

rm(list=ls())

## Clear Memory
gc(reset=TRUE)

## Set Working Directory
## setwd("C:/Users/jhwang/Documents/R/Class2/Week 2/rprog_data_specdata")

pollutantmean <- function(directory, pollutant, id = 1:332){
        
        ## Produces a character vector of the names of files or directories in the named directory.
        filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
        
        ## Create empty vector to store the values that you will find the average of later on
        values <- numeric()
        
        for(i in id){
                ## Creates a dataframe called "df" for each csv file read.
                df <- read.csv(filelist[i])
                ## From each dataframe, pull out the column with the pollutant values.
                ## Append the pollutant values to a single numeric vector listing of all values across the monitors.
                values <- c(values, df[[pollutant]] )
                
               
        }
        
        ## Find the mean of all values of the specified pollutant across the specified monitors.
        ## Ignores NA values.
        mean(values, na.rm = TRUE)
}


## Test
pollutantmean("specdata", "sulfate", 1:10)
#### 4.064128
pollutantmean("specdata", "nitrate", 70:72)
#### 1.706047
pollutantmean("specdata", "nitrate", 23)
#### 1.280833
