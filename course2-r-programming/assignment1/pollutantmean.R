## Course: R Programming
## Solution to part 1 of assignment 1

DATA.FILE.EXTENSION <- '.csv'

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".

  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used

  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  total <- 0
  count <- 0

  for (fileId in id) {
    # Construct the file path
    normalizedFileId <- formatC(fileId, width=3, flag='0')
    filePath <- paste(directory, '/', normalizedFileId, DATA.FILE.EXTENSION, sep='')

    # Read the pollutant data and omit NA values
    pollutantTable <- read.table(file=filePath, header=TRUE, sep=',')
    pollutantColumn <- na.omit(pollutantTable[pollutant])

    # Keep running total and count to eventually return the average
    if (nrow(pollutantColumn) > 0) {
      total <- total + sum(pollutantColumn)
      count <- count + nrow(pollutantColumn)
    }
  }

  total / count
}
