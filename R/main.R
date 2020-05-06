#` @export
GoFo <- function(){
  appDir <- system.file("shinyApp", "app", package = "GoFo")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `GoFo`.",call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = T)
}

#`@export
theorique.prob <- function(x, law="Poisson"){
  p <- density(x)$x
  if(law=="Uniforme"){
    prob <- rep(1/length(p),length(p))
  } else if(law=="Exponentielle"){
    res <- fitdist(x,"exp")
    prob <- dexp(p,res$estimate)
  } else if(law=="Poisson"){
    res <- fitdist(x,"pois")
    prob <- dpois(ceiling(p),lambda=as.numeric(res$estimate))
  } else if(law=="Normale"){
    res <- fitdist(x,"norm")
    prob <- dnorm(p,res$estimate[[1]],res$estimate[[2]])
  } else if(law=="LogNormale"){
    res <- fitdist(x,"lnorm")
    prob <- dlnorm(p,res$estimate[[1]],res$estimate[[2]])
  } else if(law=="Gamma"){
    res <- fitdistr(x,"gamma")
    prob <- dgamma(p,res$estimate[[1]],res$estimate[[2]])
  } else if(law=="NegBinomial"){
    res <- fitdistr(x,"nbinom")
    prob <- dnbinom(p,res$estimate[[1]],res$estimate[[2]])
  }
  else {
    prob <- NULL
  }
  return(prob)
}

#`@export
test.adjust <- function(x,test="ks",prob){
  p <- density(x)$y
  prob <- prob/sum(prob)
  if(test=="chi2"){
    pval <- chisq.test(density(x)$x,p=prob)$p.value
  }
  if(test=="ks"){
    pval <- ks.test(x, y=prob)$p.value
  }
  if(test=="sw"){
    pval <- shapiro.test(x)$p.value
  }
  return(pval)
}

#`@export
plot.ecdf.adjust <- function(x,probs){
  a <- density(x)$x
  par(mar=c(2,2,2,2))
  plot(ecdf(x),main="",col="gray")
  colors <- c("red","blue","green","purple","orange")
  for(i in c(1:length(probs))){
    p <- probs[[i]]/sum(probs[[i]])
    lines(x=sort(a),y=cumsum(p),col=colors[[i]],lty=2,lwd=2)
  }
}

#`@export
plot.hist.adjust <- function(x, probs){
  a <- density(x)$x
  b <- density(x)$y
  par(mar=c(2,2,2,2))
  plot(a,b,type="h",col="gray",main=NULL,xlab=NULL,ylab=NULL,ylim=c(0,max(b)*1.33))
  colors <- rainbow(length(probs))
  colors <- c("red","blue","green","purple","orange")
  for(i in c(1:length(probs))){
    lines(x=a,y=probs[[i]],col=colors[[i]],lty=2,lwd=2)
  }
}

#`@export
demo <- function(){
  x <- rpois(1000,50)
  laws <- c("Uniforme","Normale","Gamma","Poisson","Exponentielle","NegBinomial")
  probs <- lapply(laws,function(i){theorique.prob(x,law=i)})
  names(probs) <- laws
  pvals <- lapply(c(1:length(probs)),function(i){test.adjust(x,test="chi2",probs[[i]])})
  names(pvals) <- laws
}
