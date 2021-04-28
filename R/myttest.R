#' This is constructor function mttest
#'
#' @param x vector of the data
#' @param y vector of the data
#' @param paired  Logical vector
#' @param alpha   Significance level
#'
#' @return  List of data, type of t-test and it's statistics
#' @export
#'
#' @examples
#' myttest(x=rnorm(30,10,12), y=rnorm(40, 7, 10))
myttest <- function(x, y, paired = FALSE, alpha = 0.05) {
  #Integrity Check for Paired variable
  if (!is.logical(paired))
    stop('paired must be either TRUE or FALSE only')

  #Checking if the alpha value is between 0 and 1
  if (alpha > 1 || alpha < 0)
    stop('alpha must be between 0 and 1')

  # Checking if the given input vectors are paired or not
  if (paired == FALSE)
  {
    #If the given samples are not paired then
    #Perform F-Test to know the variability of the samples

    testType <- 'T-Test'
    ftest <- var.test(x, y)
    #alternative hypothesis: true ratio of variances is not equal to 1
    #If p-value is > alpha or significance level we fail to reject null

    if (ftest$p.value > alpha) {
      #Equal variance
      testType <- 'Welch'

      ttest <-
        t.test(x, y, var.equal = TRUE, conf.level = 1 - alpha)

      if (ttest$p.value > alpha) {
        conclusion <- 'N'
      } else {
        conclusion <- 'Y'
      }
    }
    else {
      #Unequal Variance
      testType <- 'T-Test'

      ttest <-
        t.test(x, y, var.equal = FALSE, conf.level = 1 - alpha)

      if (ttest$p.value > alpha) {
        conclusion <- 'N'
      } else {
        conclusion <- 'Y'
      }
    }

  }
  else{
    #Given samples are paired
    testType <- 'Paired'

    if (length(x) != length(y))
      stop('For Paired Samples Length of X and Y must be same')

    ttest <- t.test(x, y, paired = TRUE, conf.level = 1 - alpha)

    if (ttest$p.value > alpha) {
      conclusion <- 'N'
    } else {
      conclusion <- 'Y'
    }
  }

  inputData <- c(x, y) #Vector of  input values
  variableName <-
    rep(c('x', 'y'), c(length(x), length(y))) #Vector of input variables

  inputDF = data.frame('Data' = inputData, 'Variable' = variableName) #Creating a dataframe of the input samples

  #List containing the results
  lst = list(
    'Test' = testType,
    'Conclusion' = conclusion,
    'Summary' = ttest,
    'Data' = inputDF
  )

  #Assigning the class object to the return list and releasing it

  attr(lst, "class") <- "Rttest"

  lst  #Releasing the list with class name Rttest
}
