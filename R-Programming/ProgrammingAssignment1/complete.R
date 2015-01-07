complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  filenames <- sprintf("%03d.csv", id)
  filenames <- paste(directory, filenames, sep="/")
  ldf <- sapply(filenames, function(f) sum(complete.cases(read.csv(f))))
  data.frame(row.names=NULL, id=id, nobs=ldf)
}
