best <- function(state, by) {
    
    # Read the data and input the acceptable arguments 
    data <- read.csv('outcome-of-care-measures.csv')
    states <- unique(data$State)
    outcomes <- list('heart attack', 'heart failure', 'pneumonia')
    
    # Check that the state and outcome are valid
    if(state %in% states == FALSE) stop('invalid state')
    if(by %in% outcomes == FALSE) stop('invalid outcome')
    
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
    
    # Remove NAs, sort by rate and state name and return the first
    colnames(new) <- c('Hospital', 'Avg')
    new <- new[!(new$Avg == 'Not Available'),]
    new$Avg <- as.numeric(as.character(new$Avg)) 
    new <- new[order(new$Avg, new$Hospital), ]
    return(as.character(new$Hospital[1]))
}