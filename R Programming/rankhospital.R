rankhospital <- function(state, by, num = 'best') {
    
    # Read the data and input the acceptable arguments 
    data <- read.csv('outcome-of-care-measures.csv')
    states <- unique(data$State)
    outcomes <- list('heart attack', 'heart failure', 'pneumonia')
    
    # Check that the state and outcome are valid
    if(state %in% states == FALSE) stop('invalid state')
    if(by %in% outcomes == FALSE) stop('invalid outcome')
    if(!is.numeric(num) && num != 'best' && num != 'worst') stop('invalid rank')
    
    # Make a new data frame containing just the specified state
    x <- split(data, data$State)
    y <- as.data.frame(x[state])
    
    # Make a new data frame containing just the specified outcome
    if(by == outcomes[1]) {
        new <- y[, c(2, 11)]
    } else if(by == outcomes[2]) {
        new <- y[, c(2, 17)]
    } else {
        new <- y[, c(2, 23)]
    }
    
    # Remove NAs, sort by rate and state name and return the requested rank
    colnames(new) <- c('Hospital', 'Avg')
    new <- new[!(new$Avg == 'Not Available'),]
    new$Avg <- as.numeric(as.character(new$Avg)) 
    new <- new[order(new$Avg, new$Hospital), ]
    if(is.numeric(num) && num < length(new[,1])) {
        return(as.character(new$Hospital[num]))
    } else if(num == 'best') {
        return(as.character(new$Hospital[1])) 
    } else if(num == 'worst') {
        return(as.character(new$Hospital[length(new[,1])]))
    } else {return(NA)}
}