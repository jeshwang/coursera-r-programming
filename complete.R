## Coursera: Week 2, Assignment 1-2
## For Week Ending: Nov. 5, 2017
## User: jeshwang
#########################################################################
## Write a function that reads a directory full of files 
## and reports the number of completely observed cases in each data file. 
## The function should return a data frame where the first column is the 
## name of the file and the second column is the number of complete cases. 
#########################################################################

rm(list=ls())

## Clear Memory
gc(reset=TRUE)

## Set Working Directory
## setwd("C:/Users/jhwang/Documents/R/Class2/Week 2/rprog_data_specdata")

complete <- function(directory, id = 1:332){
        
        filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
        
        ## count is a numeric vector that will store the number of complete cases for each csv file.
        count_case <- numeric()
        
        for(i in id){
                data <- read.csv(filelist[i])
                count_case <- c(count_case, sum(complete.cases(data)))   
        }
        
        ## returns a dataframe that has the id number in one column and the number of completed cases in the second column.
        data.frame(id, count_case)
}

## Test
complete("specdata", 1)
####   id count_case
#### 1  1  117
complete("specdata", c(2, 4, 8, 10, 12))
####   id count_case
#### 1  2 1041
#### 2  4  474
#### 3  8  192
#### 4 10  148
#### 5 12   96
complete("specdata", 30:25)
####   id count_case
#### 1 30  932
#### 2 29  711
#### 3 28  475
#### 4 27  338
#### 5 26  586
#### 6 25  463
complete("specdata", 3)
####   id count_case
#### 1  3 243