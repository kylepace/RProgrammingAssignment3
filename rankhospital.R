rankhospital <- function(state, outcome, num = "best") {
	cleaned <- subsetState(state, outcome)
	ordered <- cleaned[with(cleaned, order(cleaned[, 3], cleaned[, 1], na.last = NA)), ]
	if (num == "worst") {
		ordered[nrow(ordered), c(1)]	
	} else if (is.numeric(num)) {
		ordered[as.numeric(num), c(1)]
	} else {
		ordered[1, c(1)]
	}
}