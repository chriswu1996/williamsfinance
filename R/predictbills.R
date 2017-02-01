#' @title Predict Bills
#' @description Predicts the cost of school bills at \code{year} based on a linear model
#' @param year Sets the year of bills to predict
#' @return The cost of school bills at \code{year}
#' @usage predictbills(year)
#' @export
predictbills <- function(year){

  # Reads the data
  bills <- list("2000"=readbills(2000), "2001"=readbills(2001),
                "2002"=readbills(2002), "2003"=readbills(2003),
                "2004"=readbills(2004), "2005"=readbills(2005),
                "2006"=readbills(2006), "2007"=readbills(2007),
                "2008"=readbills(2008), "2009"=readbills(2009),
                "2010"=readbills(2010), "2011"=readbills(2011),
                "2012"=readbills(2012), "2013"=readbills(2013),
                "2014"=readbills(2014), "2015"=readbills(2015),
                "2016"=readbills(2016))

  # x variable for the linear model
  years = 2000:2016

  # y variable for the linear model
  amount = vector(mode="integer", length=17)
  for(i in 1:17){
    amount[i] <- bills[[i]][[5]]
  }

  # Calculates the linear model
  fit = lm(amount~years)

  # Returns the predicted cost of school bills
  fit[[1]][[2]]*year+fit[[1]][[1]]

}
