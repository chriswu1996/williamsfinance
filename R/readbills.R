#' @title Read College Bills
#' @description  Scan for the college bills from text file and create a list of fees.
#' @param year Sets the year of bills to be read. (2000-2016)
#' @return A list of fees for the year \code{year}
#' @usage readbills(year)
#' @import stringr
#' @export
readbills <- function(year){

  # Check parameter
  if ( year < 2000 || year > 2016 ){
    warning("Invalid parameter \"year\", \"year\" must be between 2000-2016." )
    return()
  }

  # Reads and handles data
  filename = str_c(year,".txt")
  input <- readLines(system.file("extdata", filename, package = "williamsfinance"), warn = FALSE)
  input <- gsub("[^[:digit:]]","",input)
  input <- input[input != ""]
  input <- as.integer(input)
  input <- input[input < 2000000]

  # Returns data as a dataframe
  data.frame("Tuition"=input[1], "Room"=input[2], "Board"=input[3], "Activity"=input[4], "Total"=input[5])
}
