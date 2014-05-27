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
  
  file_list <- dir("specdata", full.name=T)
  dFrame <- data.frame(p=NULL,q=NULL)
  for (i in id) {
    file_name <- file_list[i]
    df <- read.csv(file_name)
    #append only the count of the complete tuples in the data frame
    #achieved by df[complete.cases(df)==T,]
    dFrame <- rbind(dFrame,c(i,nrow(df[complete.cases(df)==T,])))
  }
  names(dFrame) <- c("id","nobs")
  dFrame
}