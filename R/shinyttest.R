#' Shiny Application
#'
#' This function can be called to start shiny application and run ADVTTEST in reactive manner
#'
#'
#' @return The web page will display input sample data, summary of T-Test and box plot's of paired and unpaired samples
#'
#'
#' @import shiny
#' @export
#'
#' @examples
#' \dontrun{ shinyttest()}
shinyttest <- function () {
  shiny::runApp(system.file("ttest", package="ADVTTEST"),launch.browser = TRUE)
}
