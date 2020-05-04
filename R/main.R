#` @export
GoFo <- function(){

  library("shiny")

  appDir <- system.file("shinyApp", "app", package = "GoFo")
  if (appDir == "")
  {
    stop("Could not find app directory. Try re-installing `GoFo`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = T)
}

#`@export
theorique.prob <- function(x, law="Poisson"){
  br <- seq(round(min(x))-0.5,round(max(x))+0.5,1)
  h <- hist(x, plot=FALSE, freq = TRUE,breaks=br)
  mid <- h$mid
  dens <- h$density
  if(law=="Uniforme"){
    prob <- rep(1/length(mid),length(mid))
  }
  if(law="Exponentille"){
    prob <- dexp(abs(ceiling(mid)),1/mean(mid))
  }
  if(law=="Poisson"){
    prob <- dpois(abs(ceiling(mid)), lambda=abs(mean(x)))
  }
  if(law=="Normale"){
    prob <- dnorm(abs(ceilling(mid),mean(x),sd(x)))
  }
  return(prob)
}







}
