#' Generic print function
#'
#' This function can be used to print the confidence interval for difference in means of two samples with it's confidence level and also prints the type of test being performed
#' @param x  object of class Rttest
#'
#' @return Prints list containing the confidence intervel and type of test
#' @export
#'
#' @examples
#' t = myttest(x=rnorm(30,10,12), y=rnorm(40, 7, 10)); print(t)
print.Rttest <- function(x) {
  if (!is.list(x))
    stop('The input is not a list')
  if (class(x) != 'Rttest') stop('The given input is not an object of class Rttest')

  ci <- x$Test_Summary$conf.int   #Accessing the confidence interval from test summary

  type <- x$Test_Type

  lst = list('Confidence Interval' = ci, 'Test Type' = type)

  print(lst)
}
