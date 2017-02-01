#' @title Read Inflation
#' @description  Scan the inflation rate from text file and create a list.
#' @return A list of inflation rates from 2000-2016
#' @usage readinflation()
#' @export
readinflation <- function(){

  # Reads in flation rate from txt file
  input <- readLines(system.file("extdata", "inflation.txt", package = "williamsfinance"), warn = FALSE)

  # Converts the data to double
  input <- as.double(input)

  # Returns the inflation rates
  input
}
