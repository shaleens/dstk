pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  file_list <- dir("specdata", full.name=T)
  elems = NULL
  for (i in id) {
    file_name <- file_list[i]
    df <- read.csv(file_name)
    elems <- c(elems,df[[pollutant]])
  }
  isna = is.na(elems)
  elems = elems[isna==F]
  mean1 <- mean(elems,na.rm=T)

}