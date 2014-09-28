OUTCOMES.FILE <- "rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv"

rankhospital <- function(state, outcome, num) {
	## Given a state and an outcome, return the name of the hospital within that
	## state with the lowest 30-date mortalilty rate for that outcome.

	# Read outcome data.
	outcomes.data <- read.csv(OUTCOMES.FILE, colClasses = "character")
	outcomes.for.state <- outcomes.data[outcomes.data$State == state, ]

	# Stop if state name is not in the data frame.
	if (nrow(outcomes.for.state) == 0) stop("invalid state")

	# Set `sel` to the appropriate column number based on `outcome`.
	sel <- 	if (outcome == "heart attack") 11
					else if (outcome == "heart failure") 17
					else if (outcome == "pneumonia") 23
					else stop("invalid outcome")

	# Sort hospital names by 30-day death rate and by alphabetical order.
	mortality.rate <- suppressWarnings(as.numeric(outcomes.for.state[, sel]))
	hospital.name <- outcomes.for.state[, 2]
	sorted.row.indices <- order(mortality.rate, hospital.name, na.last = NA)

	index <-  if (num == "best") 1
						else if (num == "worst") length(sorted.row.indices)
						else if (class(num) == "numeric") num
						else stop("invalid num")

	outcomes.for.state[sorted.row.indices, 2][index]
}
