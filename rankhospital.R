rankhospital <- function(state, outcome, num="best"){
		
		#Load the data into memory
		df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
		
		# make a character vector for outcome type
		attack_type <- c("heart attack", "heart failure", "pneumonia")
		
		# Check if outcome is valid
		if(!any(outcome %in% attack_type))
			stop(" invalid outcome", call. = TRUE)
		
		# Check if state is valid	
		states <- df[,7]
		if(!any(state %in% states))
			stop(" invalid state", call. = TRUE)
		
		# Set the colvalue as per outcome type
		if(outcome == "heart attack")
			colvalue <- 11
		else if(outcome == "heart failure")
			colvalue <- 17
		else 
			colvalue <-23
		
		# Subset the data 
		temp <- df[df$State ==state,c(2,colvalue)]
		
		# Coerce the attack value for 30 days as numeric
		# Warnings are supressed as dataframe contains NAs
		temp[,2] <- suppressWarnings(as.numeric(temp[,2]))
		
		# Remove all NA's 
		tempdata <- temp[complete.cases(temp),]
		
		# Order the data first by attack value and then by hospital name
		newdata <- tempdata[order(tempdata[,2],tempdata[,1]),]
		
		# Set the colInex as per num
		if(num == "best")
			rowIndex <- 1
		else if (num == "worst")
			rowIndex <- nrow(newdata)
		else 
			rowIndex <- as.numeric(num)
		
		# print the fist row's hospital name 
		newdata$Hospital.Name[rowIndex]
}
