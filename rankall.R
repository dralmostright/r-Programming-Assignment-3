rankall <- function(outcome, num = "best"){
		#Load the data into memory
		df <- read.csv("outcome-of-care-measures.csv")
		
		# make a character vector for outcome type
		attack_type <- c("heart attack", "heart failure", "pneumonia")
		
		# Check if outcome is valid
		if(!any(outcome %in% attack_type))
			stop(" invalid outcome", call. = TRUE)
		
		# Set the colvalue as per outcome type
		if(outcome == "heart attack")
			colvalue <- 11
		else if(outcome == "heart failure")
			colvalue <- 17
		else 
			colvalue <- 23
		
		# coherce the states as levles/factors 
		# will automatically sort the levels in ascending order and remove duplicates
		states <- levels(df[,7])
		
		# coherce the mortality rate form levels to back to its original value
		df[, colvalue] <- suppressWarnings(as.numeric(levels(df[, colvalue])[df[, colvalue]]))
		
		# coherce the name of hospital to character
		df[, 2] <- as.character(df[, 2])
		
		# initialize the empty vecotor
		output <- vector()
		
		# go through subset of data belongig to each state by grouping them
		# sort the subseted data with mortality rate first and with name of hospital
		# assigin values ofr best and worst
		
		for(i in 1:length(states)) {
				# group the data belonging to specific state i.e states[i]
                statedata <- df[grep(states[i], df$State), ]
				
				# Order the data first by mortality rate and secondly with name of hospital
                orderdata <- statedata[order(statedata[, colvalue], statedata[, 2], na.last = NA), ]
				
				# Extract the name of hospital best on the value passed in num variables
                hname <- if(num == "best") {
                     as.character(orderdata[1, 2])
                } else if(num == "worst") {
                     as.character(orderdata[nrow(orderdata), 2])
                } else{
                     as.character(orderdata[num, 2])
                }
				
				# append the values each in output vector
                output <- append(output, c(hname, states[i]))
        }
		
		# coherce the vector to character matrix and then to dataframe
		output <- as.data.frame(matrix(output, length(states), 2, byrow = TRUE))
		
		# Set the column names
        colnames(output) <- c("Hospital.Name", "State")
		
		# Set the row index
		rownames(output) <- states
		
		# Display the result
		output
}
