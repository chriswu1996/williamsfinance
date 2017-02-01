#' @title Plot Bills
#' @description Visualizes components of school bills data from \code{readbills}.
#' @param year The year of bills to be read. (2000-2016)
#' @return Bar charts showing components of school bill in \code{year}.
#' @usage plotbills(year)
#' @import ggplot2
#' @import stringr
#' @export
plotbills <- function(year){

  # Check parameter
  if ( year < 2000 || year > 2016 ){
    warning("Invalid parameter \"year\", \"year\" must be between 2000-2016." )
    return()
  }

  # Reads the data
  df <- data.frame(Bills=names(readbills(year)),
                   Cost=as.double(readbills(year)))

  # Plots the graph
  ggplot(data=df, aes(x=Bills, y=Cost)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=Cost), vjust=-0.6, color="black", size=3.5)+
    scale_x_discrete(limits=c("Activity","Board","Room","Tuition","Total"))+
    ggtitle(str_c("Components of School Bills for ",year))+
    theme_minimal()
}
