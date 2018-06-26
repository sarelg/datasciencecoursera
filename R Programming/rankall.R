rankall <- function(by, num = 'best') {
    
    # Read the data and input the acceptable arguments 
    data <- read.csv('outcome-of-care-measures.csv')
    states <- unique(data$State)
    states <- states[order(states)]
    outcomes <- list('heart attack', 'heart failure', 'pneumonia')
    
    # Check that the state and outcome are valid
    if(by %in% outcomes == FALSE) stop('invalid outcome')
    if(!is.numeric(num) && num != 'best' && num != 'worst') stop('invalid rank')
    
    # Make a new data frame containing just the specified state
    x <- split(data, data$State)
    df <- data.frame()
    for(state in states) {
        y <- as.data.frame(x[state])
        state_vector = c()
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
            state_vector <- c((as.character(new$Hospital[num])), state)
        } else if(num == 'best') {
            state_vector <- c((as.character(new$Hospital[1])), state) 
        } else if(num == 'worst') {
            state_vector <- c((as.character(new$Hospital[length(new[,1])])), state)
        } else {state_vector <- c(NA, state)}
        df <- rbind(df, state_vector, stringsAsFactors=FALSE)
    }
    colnames(df) <- c('hospital', 'state')
    rownames(df) <- df$state
    return(df)
}