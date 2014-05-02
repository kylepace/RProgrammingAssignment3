cleanData <- function(outcome) {
	if (outcome == "heart attack")
		measureCol <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	else if (outcome == "heart failure")
		measureCol <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	else if (outcome == "pneumonia")
		measureCol <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	else
		stop("invalid outcome")
	
	outcomes <- read.csv('./data/outcome-of-care-measures.csv', colClasses = "character")
	outcomes <- subset(outcomes, select = c("Hospital.Name", "State", measureCol))
	outcomes
}

subsetState <- function(state, outcome) {
	outcomes <- cleanData(outcome)
	onlyStates <- subset(outcomes, State == state)
	
	onlyStates[, c(3)] <- as.numeric(onlyStates[, c(3)])
	if (nrow(onlyStates) == 0)
		stop("invalid state")
	onlyStates
}

best <- function(state, outcome) {
	cleaned <- subsetState(state, outcome)
	ordered <- arrange(cleaned, Hospital.Name)
	ordered[which.min(ordered[, c(3)]), c(1)]
}