#' @title Percentage Change
#' @description Visualizes percentage change of components of school bills data from \code{readbills}.
#' @param year The year of bills to be read. (2001-2016)
#' @return Bar charts showing the percentage change of components of school bill in \code{year} and the inflation rate.
#' @usage pctchange(year)
#' @import ggplot2
#' @import stringr
#' @export
pctchange <- function(year){

  if ( year < 2001 || year > 2016 ){
    warning("Invalid parameter \"year\", \"year\" must be between 2001-2016." )
    return()
  }

  # Loads necessary bills data
  bills <- list( readbills(year-1), readbills(year))

  # Inflation
  inflation = readinflation()[year-2000]

  # Absolute and percentage increase of each component of school bills
  pctinc <- vector( mode = "double", length = 5 )
  absinc <- vector( mode = "double", length = 5 )
  for( i in 1:5 ){
    absinc[i] <- bills[[2]][[i]] - bills[[1]][[i]]
    pctinc[i] <- absinc[i] / bills[[1]][[i]] * 100
  }

  # Reads the data
  df <- data.frame(Components=c(names(readbills(year)),"Inflation"),
                   Inc=c(pctinc, inflation) )

  # Plots the graph
  ggplot(data=df, aes(x=Components, y=Inc))+
    geom_bar(stat="identity", fill="steelblue", width=0.6)+
    ylab("Percentage Increase")+
    scale_x_discrete(limits=c("Inflation","Activity","Board","Room","Tuition","Total"))+
    ggtitle(str_c("Percentage Increase of Components of School Bills for ",year))+
    theme_minimal()
}
