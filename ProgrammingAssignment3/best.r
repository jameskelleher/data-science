best <- function(state, outcome) {
  data <- read.csv('outcome-of-care-measures.csv', na.strings=c('Not Available'))
  if (identical(outcome, 'heart attack')) {
    outcome_no <- 11
  } else if (identical(outcome, 'heart failure')) {
    outcome_no <- 17
  } else if (identical(outcome, 'pneumonia')) {
    outcome_no <- 23
  } else {
    stop('invalid outcome')
  }

  if (!state %in% data$State) {
    stop('invalid state')
  }
  
  data.subset <- subset(data[outcome_no], data$State == state)
  data.subset <- data.subset[!is.na(data.subset)]
  topscore <- min(data.subset)
  best_hospitals <- subset(data$Hospital.Name, data[outcome_no] == topscore & data$State == state)
  the_best <- sort(best_hospitals)[1]
  r <- as.vector(as.character(the_best))
  r
}