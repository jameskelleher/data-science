rankhospital <- function(state, outcome, num='best') {
  data <- read.csv('outcome-of-care-measures.csv', na.strings=c('Not Available'))
  
  if (identical(outcome, 'heart attack')) {
    outcome_col <- names(data)[11]
  } else if (identical(outcome, 'heart failure')) {
    outcome_col <- names(data)[17]
  } else if (identical(outcome, 'pneumonia')) {
    outcome_col <- names(data)[23]
  } else {
    stop('invalid outcome')
  }
  
  if (!state %in% data$State) {
    stop('invalid state')
  }
    
  state_hospitals <- subset(data[c('Hospital.Name', outcome_col)], data$State==state & !is.na(data[outcome_col]))
#   ranked_hospitals <- order(state_hospitals['Hospital.Name'], state_hospitals[outcome_col])
#   ranked_hospitals <- order(state_hospitals)
#   state_hospitals[ranked_hospitals,]
  rankings <- order(state_hospitals[outcome_col], state_hospitals$Hospital.Name)
  ranked_hospitals <- state_hospitals[rankings,]

  if (num == 'best') {
    num <- 1
  } else if (num == 'worst') {
    num <- nrow(ranked_hospitals)
  } else if (num > nrow(ranked_hospitals)) {
    return(NA)
  }

  selection <- ranked_hospitals[num, 1]
  s <- as.vector(as.character(selection))
  s
  
}