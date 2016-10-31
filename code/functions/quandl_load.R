### Download + Unzip file
# create function bellow to be called in main function

loadUnzip <- function(){
  install.packages("Quandl")
  library(Quandl)
  
  # extract 'key' from file
  source("code/keys.R")
  Quandl.api_key(key)
  
  ### GET ENTIRE DATABASE: ZIP -> CSV
  database_code <- "SF1"
  file_path <- paste("data/", database_code, "_dump.zip", sep="")
  
  Quandl.database.bulk_download_to_file(database_code, file_path)
  
  ### OPEN CSV
  file <- unzip(file_path, exdir = "./data")
  
  return(file)
}