#' @title Show Statistics
#' @description Uses bills data from the read function to perform appropriate analysis and generate graphics associated with the data.
#' @param type The kind of analysis to be done. The possible options are \code{summary}, \code{bills}, \code{pctchange}, \code{inflation}, \code{predictbills}, \code{predictpc} and \code{inflationbills}. \code{summary} displays the total amount, percentage increase and total increase of school bills and the inflation rate of each year. \code{bills} displays the timeplot of school bills across the years. \code{pctchange} shows a timeplot of percentage increase of bills across the years. \code{inflation} shows a timeplot of percentage increase of school bills compared to inflation. \code{predictbills} shows a plot of the amount of school bills and the model used to predict future school bills. \code{predictpc} shows a plot of the percentage increase of school bills and the model used to predict future percentage increases. \code{inflationbills} shows a plot of a comparison between the actual cost of school bills and the cost of school bills if it has been increasing at the inflation rate since 2000.
#' @return Results from the kind of analysis specified by \code{type}.
#' @usage showstats("type")
#' @import ggplot2
#' @export
showstats <- function(type){

  # Loads all school bills data
  bills <- list( "2000"=readbills(2000), "2001"=readbills(2001),
                 "2002"=readbills(2002), "2003"=readbills(2003),
                 "2004"=readbills(2004), "2005"=readbills(2005),
                 "2006"=readbills(2006), "2007"=readbills(2007),
                 "2008"=readbills(2008), "2009"=readbills(2009),
                 "2010"=readbills(2010), "2011"=readbills(2011),
                 "2012"=readbills(2012), "2013"=readbills(2013),
                 "2014"=readbills(2014), "2015"=readbills(2015),
                 "2016"=readbills(2016))

  # Total cost for school bills each year
  amount = vector( mode = "integer", length = 17 )
  for( i in 1:17 ){
    amount[i] <- bills[[i]][[5]]
  }

  # Absolute and percentage increase of each component of school bills
  pctinc <- vector( mode = "double", length = 16 )
  absinc <- vector( mode = "double", length = 16 )
  for( i in 1:16 ){
      absinc[i] <- bills[[i+1]][[5]] - bills[[i]][[5]]
      pctinc[i] <- absinc[i] / bills[[i]][[5]] * 100
  }

    # Construct dataframe that summarizes the data
  summary <- data.frame( Year = 2000:2016,
                         Amount = amount,
                         Total.Increase = c( NA, absinc ),
                         Percentage.Increase = c( NA, pctinc ),
                         Inflation = c( readinflation() ))

  #-----------------------------------------------------------------
  if( type == "summary" ){

    # Returns summary
    summary

  }else if( type == "bills" ){

    # Constructs a timeplot of school bills
    ggplot(data=summary)+
      geom_point(aes(x=Year, y=Amount, col="Cost of School Bills"), size=6, alpha=0.8)+
      geom_smooth(aes(x=Year, y=Amount), method="loess")+
      ylab("Cost of School Bills in USD")+
      theme(legend.position="none")+
      ggtitle("Timeplot of School Bills")

  }else if( type == "pctchange" ){

    # Constructs a timeplot of percentage increase of bills
    ggplot(data=summary[-1,])+
    geom_point(aes(x=Year, y=Percentage.Increase, col="Percentage Increase of School Bills"), size=6, alpha=0.8)+
    geom_smooth(aes(x=Year, y=Percentage.Increase), alpha=0.3, method="loess")+
    ylab("Percentage Increase")+
    theme(legend.position="none")+
    ggtitle("Timeplot of Percentage Increase of School Bills")

  } else if( type == "inflation" ){

  # Timeplot of percentage increase of bills compared to inflation
  ggplot(data=summary[-1,])+
    geom_point(aes(x=Year, y=Percentage.Increase, col="Percentage Increase of School Bills"), size=6, alpha=0.8)+
    geom_smooth(aes(x=Year, y=Percentage.Increase), method = 'loess')+
    geom_point(aes(x=Year, y=Inflation, col="Inflation Rate"), size=6, alpha=0.8)+
    geom_smooth(aes(x=Year, y=Inflation), method = 'loess', colour="red", alpha=0.2)+
    ylab("Percentage Change")+
    ggtitle("Inflation Rate Compared to Percentage Increase of School Bills")+
    scale_color_discrete(name="Legend")

  } else if ( type == "predictbills"){

    # Constructs the linear model to predict future school bills
    fit = lm(summary[,2]~summary[,1])

    # Plots the amount of school bills and the model used for prediction
    ggplot(data=summary)+
      geom_point(aes(x=Year, y=Amount, col="Cost of school bills"), size=6)+
      ylab("Cost in USD")+
      ggtitle("Model Used to Predict Future School Bills")+
      geom_abline(aes(intercept=fit[[1]][[1]], slope=fit[[1]][[2]], colour="Linear model for prediction"), size=1)+
      scale_color_manual(name="Legend", values=c("lightcoral","black"))

  } else if ( type == "predictpc") {

    # Constructs exponential model to predict future percentage change
    fit = lm(log(summary[4:17,4])~summary[4:17,1])
    expfun <- function(x) {
      exp(fit[[1]][[2]]*x+fit[[1]][[1]])
    }

    # Plots percentage change of bills and the model used for prediction
    ggplot(data=summary[-1,])+
      geom_point(aes(x=Year, y=Percentage.Increase, col="Percentage Increase of School Bills"), size=6, alpha=0.8)+
      ylab("Percentage Increase")+
      ggtitle("Model Used to Predict Future Percentage Increase of School Bills")+
      stat_function(fun = expfun, aes(colour="Exponential Model for prediction"),size=1)+
      scale_color_manual(name="Legend", values=c("black","lightcoral"))

  } else if( type == "inflationbills"){

    # Bills compared to what bills are supposed to be if it has been incrasing at the inflation rate since 2000

    # Calculates college bills based on inflation
    v = vector(mode="integer", length=17)
    v[1] = summary[1,2]
    for(i in 2:17){
      v[i] <- v[i-1]*(100+summary[i,5])/100
    }
    df = data.frame(year=2000:2016, points=v)

    # Plots the graph
    ggplot(data=summary)+
      geom_point(aes(x=Year, y=Amount, col="Actual Cost of School Bills"), size=6, alpha=0.6)+
      geom_smooth(aes(x=Year, y=Amount), colour="red", method="loess")+
      geom_point(data=df, aes(x=year, y=points, col="The cost of school bills if it\nhas been increasing at the\ninflation rate since 2000"), size=6, alpha=0.6)+
      geom_smooth(data=df, aes(x=year, y=points), se=FALSE, method="loess")+
      ylab("Cost in USD")+
      scale_color_discrete(name="Legend")+
      ggtitle("The cost of school bills if it has been increasing at the inflation rate since 2000")

  }else{
    warning("The specified type does not exist. Enter ?showstats or help(showstats) to view all the options available.")
  }
}
