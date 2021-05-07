#' Shiny Application
#'
#' This function can be called to start shiny application
#'
#' @import shiny
#' @return Plots
#' @export
#'
#' @examples
#' \dontrun{ shinyttest()}
shinyttest <- function () {
  shiny::runApp(system.file("ttest", package="ADVTTEST"),launch.browser = TRUE)
}
