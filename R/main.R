#` @export
GoFo <- function(){
  appDir <- system.file("shinyApp", "app", package = "GoFo")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `GoFo`.",call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = T)
}

#`@export
theorique.prob <- function(x, law="Poisson",br=NULL){
  if(is.null(br)){
    br <- seq(round(min(x))-0.5,round(max(x))+0.5,1)
  }
  h <- hist(x, plot=FALSE, freq = TRUE,breaks=br)
  mid <- h$mid
  dens <- h$density
  p <- abs(ceilling(mid))
  if(law=="Uniforme"){
    prob <- rep(1/length(mid),length(mid))
  }
  if(law="Exponentille"){
    res <- fitdist(x,"exp")
    prob <- dexp(p,res@estimate)
  }
  if(law=="Poisson"){
    res <- fitdist(x,"pois")
    prob <- dpois(p,lambda=res@estimate)
  }
  if(law=="Normale"){
    res <- fitdist(x,"norm")
    prob <- dnorm(p,res$estimate[[1]],res$estimate[[2]])
  }
  if(law=="LogNormale"){
    res <- fitdist(x,"lnorm")
    prob <- dlnorm(p,res$estimate[[1]],res$estimate[[2]])
  }
  if(law=="Gamma"){
    res <- fitdistr(x,"gamma")
    prob <- dgamma(p,res$estimate[[1]],res$estimate[[2]])
  }
  return(prob)
}

#`@export
test.adjust <- function(x,test="chi2",prob){

  if(test=="chi2"){

  }

  if(test=="ks"){

  }

  if(test=="sw"){

  }

}



