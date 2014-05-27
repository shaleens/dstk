corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  file_list <- dir("specdata", full.name=T)
  #use the data frame from part 2 of this assignment
  completeRows <- complete(directory)
  #see if they meet the threshhold and filter out
  thresholdMet <- completeRows[completeRows$nobs>threshold,]
  #get a vector of good IDs
  ids <- thresholdMet$id
  #vec = vector(mode="numeric",length=as.integer(length(ids)))
  vec <- NULL
  for(i in ids) {
    df = read.csv(file_list[i])
    completeCases = complete.cases(df)
    dfComplete = df[completeCases,]
    correlation = cor(dfComplete$sulfate,dfComplete$nitrate)
    vec <- c(vec,correlation)
  }
  vec
}