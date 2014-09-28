## Course: R Programming
## Solution to part 3 of assignment 1

corr <- function(directory, threshold = 0, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  correlations <- numeric()
  
  fileNames <- list.files(directory, full.names=TRUE)
  for (fileId in id) {
    pollutantTable <- read.table(file=fileNames[fileId], header=TRUE, sep=',')
    normalizedTable <- pollutantTable[complete.cases(pollutantTable), ]
    if (nrow(normalizedTable) >= threshold) {
      correlations <- c(correlations, cor(normalizedTable["sulfate"], normalizedTable["nitrate"]))
    }
  }
  correlations
}

# weightmedian <- function(directory, day) {
#   files_list <- list.files(directory, full.names=TRUE) #creates a list of files
#   dat <- data.frame() #creates an empty data frame
#   for (i in 1:5) { #loops through the files, rbinding them together
#     dat <- rbind(dat, read.csv(files_list[i]))
#   }
#   dat_subset <- dat[which(dat[, "Day"] == day),] #subsets the rows that match the 'day' argument
#   median(dat_subset[, "Weight"], na.rm=TRUE) #identifies the median weight
#   #while stripping out the NAs
# }
