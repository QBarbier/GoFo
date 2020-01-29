#` @export
GoFos <- function(){

  library("shiny")

  appDir <- system.file("shinyApp", "app", package = "Jarvis")
  if (appDir == "")
  {
    stop("Could not find app directory. Try re-installing `Jarvis`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = T)
}
