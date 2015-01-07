source("complete.R")
corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  df <- complete("specdata")
  ids <- df[df["nobs"] > threshold, ]$id
  corrr = numeric()
  
  
  ## filenames <- sprintf("%03d.csv", id)
  ## filenames <- paste(directory, filenames, sep="/")
  ## ldf <- sapply(filenames, read.csv)
  ## eliminate na values from ldf and calculate corr
  
  for (i in ids) {
    
    newRead = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                             ".csv", sep = ""))
    dff = newRead[complete.cases(newRead), ]
    corrr = c(corrr, cor(dff$sulfate, dff$nitrate))
  }

  ## Return a numeric vector of correlations
  return(corrr)
}