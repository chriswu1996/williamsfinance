#' @title Predict Percentage Change
#' @description Predicts the percentage change of school bills at \code{year} based on an exponential model
#' @param year Sets the year of bills to predict
#' @return The percentage change of school bills at \code{year}
#' @usage predictpc(year)
#' @export
predictpc <- function(year){

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

  # x variable for the model
  years = 2003:2016

  # y variable for the model
  pctinc <- vector( mode = "double", length = 16 )
  absinc <- vector( mode = "double", length = 16 )
  for( i in 1:16 ){
    absinc[i] <- bills[[i+1]][[5]] - bills[[i]][[5]]
    pctinc[i] <- absinc[i] / bills[[i]][[5]] * 100
  }

  # Calculates the model
  fit = lm(log(pctinc[3:16])~years)

  # Returns the predicted percentage increase of school bills
  exp(fit[[1]][[2]]*year+fit[[1]][[1]])

}
