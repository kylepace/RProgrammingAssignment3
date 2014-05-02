getRanked <- function(hosp, state, num) {
	ordered <- hosp[with(hosp, order(hosp[, 3], hosp[, 1], na.last = NA)), ]
	if (num == "worst") {
		ordered[nrow(ordered), c(1, 2)]	
	} else if (is.numeric(num)) {
		if (nrow(ordered) < as.numeric(num)) {
			data.frame(hospital = c(NA), state = c(ordered(1, 2)))
		} else {
			ordered[as.numeric(num), c(1, 2)]
		}
	} else {
		ordered[1, c(1, 2)]
	}
}

rankall <- function(outcome, num = "best") {
	outcomes <- cleanData(outcome)
	colnames(outcomes) <- c("hospital", "state", "measure")
	outcomes[, c(3)] <- as.numeric(outcomes[, c(3)])
	outcomes <- arrange(outcomes, state)
	output <- lapply(split(outcomes, outcomes$state), getRanked, num = num)
	do.call(rbind.data.frame, output)
}