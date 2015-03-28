rankall <- function(outcome, num="best") {
  data <- read.csv('outcome-of-care-measures.csv', na.strings='Not Available')
  
  if (identical(outcome, 'heart attack')) {
    outcome_col <- names(data)[11]
  } else if (identical(outcome, 'heart failure')) {
    outcome_col <- names(data)[17]
  } else if (identical(outcome, 'pneumonia')) {
    outcome_col <- names(data)[23]
  } else {
    stop('invalid outcome')
  }
  
  hospital <- c()
  states <- sort(unique(data$State))
  for (state in states) {
    state_hospitals <- subset(data[c('Hospital.Name', outcome_col)], data$State==state & !is.na(data[outcome_col]))
    rankings <- order(state_hospitals[outcome_col], state_hospitals$Hospital.Name)
    ranked_hospitals <- state_hospitals[rankings,]
    if (num == 'best') {
      n <- 1
    } else if (num == 'worst') {
      n <- nrow(ranked_hospitals)
    } else if (num > nrow(ranked_hospitals)) {
      hospital <- c(hospital, NA)
      next
    } else {
      n <- num
    }
    selection <- ranked_hospitals[n, 1]
    h <- as.vector(as.character(selection))
    hospital <- c(hospital, h)
  }
  
  state <- states
  df <- data.frame(hospital, state, row.names = state)
  df
}