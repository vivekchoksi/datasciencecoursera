## Course: R Programming
## Solution to part 2 of assignment 1

DATA.FILE.EXTENSION <- '.csv'

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  completeCasesCounts = NULL

  for (fileId in id) {
    # Construct the file path
    normalizedFileId <- formatC(fileId, width=3, flag='0')
    filePath <- paste(directory, '/', normalizedFileId, DATA.FILE.EXTENSION, sep='')
    
    # Read the pollutant data and keep track of number of complete cases
    pollutantTable <- read.table(file=filePath, header=TRUE, sep=',')
    completeCasesCount <- sum(complete.cases(pollutantTable))
    completeCasesCounts <- c(completeCasesCounts, completeCasesCount)
  }
  data.frame(id, nobs=completeCasesCounts)
}