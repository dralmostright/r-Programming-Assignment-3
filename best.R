######################################################
# This is the R function in coursera R programming   #
# forth week assignment which find the best hospital #
# name in the state from provided data               #
# outcome-of-care-measures.csv                       #
######################################################

best <- function (state, outcome){
		
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
			colvalue <- 13
		else if(outcome == "heart failure")
			colvalue <- 19
		else 
			colvalue <-25
		
		# Subset the data 
		temp <- df[df$State ==state,c(2,colvalue)]
		
		# Coerce the attack value for 30 days as numeric
		# Warnings are supressed as dataframe contains NAs
		temp[,2] <- suppressWarnings(as.numeric(temp[,2]))
		
		# Order the data first by attack value and then by hospital name
		newdata <- temp[order(temp[,2],temp[,1] , na.last=TRUE),]
		
		# print the fist row's hospital name 
		head(newdata$Hospital.Name,1)
}
