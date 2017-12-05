## Coursera: Week 2, Assignment 1-3
## For Week Ending: Nov. 5, 2017
## User: jeshwang
#########################################################################
## Write a function that takes a directory of data files and a 
## threshold for complete cases and calculates the correlation between 
## sulfate and nitrate for monitor locations where the number of completely 
## observed cases (on all variables) is greater than the threshold. 
## The function should return a vector of correlations for the monitors 
## that meet the threshold requirement. If no monitors meet the threshold requirement,
## then the function should return a numeric vector of length 0.
#########################################################################

rm(list=ls())
## Clear Memory
gc(reset=TRUE)

## Set Working Directory
## setwd("C:/Users/jhwang/Documents/R/Class2/Week 2/rprog_data_specdata")

corr <- function(directory, threshold = 0) { 
        ## 'directory' is a character vector of length 1 indicating 
        ## the location of the CSV files 
        
        
        ## 'threshold' is a numeric vector of length 1 indicating the 
        ## number of completely observed observations (on all 
        ## variables) required to compute the correlation between 
        ## nitrate and sulfate; the default is 0 

        ## Return a numeric vector of correlations 
        
        
        tcorr <- function(fname) { 
                data <- read.csv(file.path(directory, fname)) 
                nobs <- sum(complete.cases(data)) 
                if (nobs > threshold) { 
                         return (cor(data$nitrate, data$sulfate, use="complete.obs")) 
                      } 
                } 
        tcorrs <- sapply(list.files(directory), tcorr) #get all correlations + NULLs 
        tcorrs <- unlist(tcorrs[!sapply(tcorrs, is.null)]) #remove NULLs 
        return (tcorrs) 

}

cr <- corr("specdata", 150)
head(cr)
## [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
summary(cr)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313
