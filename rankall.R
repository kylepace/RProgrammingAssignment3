getRanked <- function(hosp, state, num) {
	hosp <- subset(hosp, State == state)
	ordered <- hosp[with(hosp, order(hosp[, 3], hosp[, 1], na.last = NA)), ]
	if (nrow(ordered) == 0)
		stop("invalid state")
	
	if (num == "worst") {
		ordered[nrow(ordered), c(1, 2)]	
	} else if (is.numeric(num)) {
		if (nrow(ordered) < as.numeric(num)) {
			data.frame(Hospital.Name = c(NA), State = c(state))
		} else {
			ordered[as.numeric(num), c(1, 2)]
		}
	} else {
		ordered[1, c(1, 2)]
	}
}

rankall <- function(outcome, num = "best") {
	outcomes <- cleanData(outcome)
	outcomes[, c(3)] <- as.numeric(outcomes[, c(3)])
	for (state in unique(outcomes$State)) {
		row <- getRanked(outcomes, state, num)
		if (exists("acc")) {
			acc <- rbind(acc, row)
		} else {
			acc <- row	
		}
	}
	colnames(acc) <- c("hospital", "state")
	arrange(acc, state)
}