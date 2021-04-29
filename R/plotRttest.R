#' Generic Plot function
#'
#' This function can be used to plot box plots for paired and non paired samples
#'
#' @param x object of class Rttest
#'
#' @return Returns a boxplot for two values using \code{ggplot()}
#'
#' @importFrom ggplot2 ggplot aes theme_bw geom_boxplot labs geom_errorbar
#'
#' @export
#'
#' @examples
#' t = myttest(x=rnorm(30,10,12), y=rnorm(40, 7, 10)); plot(t)
plot.Rttest <- function(x) {
  if (!is.list(x))
    stop('The input is not a list')
  if (class(x) != 'Rttest')
    stop('The given input is not an object of class Rttest')

  if (x$Test_Type != 'PAIRED') {
    #Box Plot for Non-Paired samples
    df = x$Data
    gplot <-
      ggplot(df,
             aes(
               x = Variable,
               y = Data,
               fill = Variable
             )) + geom_boxplot(outlier.colour = "blue",
                               outlier.size = 2) + labs(
                                 title = "Box Plot for non-paired data",
                                 x = "Sample",
                                 y = "Values",
                                 fill = "Sample"
                               ) + theme_bw()
    print(gplot)
  }
  else{
    #Box Plot for PAIRED Samples

    xValues <-
      x$Data$Data[x$Data$Variable == 'x'] #Extracting the values of sample x

    yValues <-
      x$Data$Data[x$Data$Variable == 'y'] #Extracting the values of sample y

    df <-
      data.frame('data' = xValues - yValues, Variable = 'x') #Difference between x and y

    gplot <-
      ggplot(df,
             aes(x = Variable,
                 y = data)) + geom_boxplot(outlier.color = 'red', outlier.size = 2) + labs(title = "Box Plot for Difference between x and y ", x = "Difference", y = "Values(x-y)") +   geom_errorbar(aes(
                   ymin  = x$Test_Summary$conf.int[1],
                   ymax  = x$Test_Summary$conf.int[2],
                   width = 0.01
                 )) + theme_bw()
    gplot
  }
}
