complete <- function(directory, id=1:332) {
  files <- list.files(directory)
  nobs <- c()
  for (file in files[id]) {
    relpath <- paste(directory, file, sep='/')
    data <- read.csv(relpath)
    sulfmask <- !is.na(data$sulfate)
    nitmask <- !is.na(data$nitrate)
    complete <- sulfmask & nitmask
    complete_clean <- complete[which(complete==T)]
    nobs <- c(nobs, length(complete_clean))
  }
  data.frame(id, nobs)
}