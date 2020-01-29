#` @export
GoFo <- function(){

  library("shiny")

  appDir <- system.file("shinyApp", "app", package = "GoFo")
  if (appDir == "")
  {
    stop("Could not find app directory. Try re-installing `GoFo`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = T)
}
