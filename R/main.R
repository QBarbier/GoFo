#` @export
GoFo <- function(){
  appDir <- system.file("shinyApp", "app", package = "GoFo")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `GoFo`.",call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = T)
}

#`@export
theorique.prob <- function(x, law="Poisson",estimate.by="histogram"){
  if(estimate.by == "histogram"){
    br <- seq(round(min(x))-0.5,round(max(x))+0.5,1)
    h <- hist(x, plot=FALSE, freq=TRUE,breaks=br)
    p <- h$mid
    y.obs <- h$density
  } else {
    p <- density$x
  }
  if(law=="Uniforme"){
    prob <- rep(1/length(p),length(p))
  } else if(law=="Exponentielle"){
    if(length(which(x<0))>0){
      prob <- rep(0,length(p))
    } else {
      res <- fitdist(x,"exp")
      prob <- dexp(p,res$estimate)
    }
  } else if(law=="Poisson"){
    if(length(which(x<0))>0){
      prob <- rep(0,length(p))
    } else {
      res <- fitdist(x,"pois")
      prob <- dpois(p,lambda=as.numeric(res$estimate))
    }
  } else if(law=="Normale"){
    res <- fitdist(x,"norm")
    prob <- dnorm(p,res$estimate[[1]],res$estimate[[2]])
  } else if(law=="LogNormale"){
    res <- fitdist(x,"lnorm")
    prob <- dlnorm(p,res$estimate[[1]],res$estimate[[2]])
  } else if(law=="Gamma"){
    if(length(which(x<0))>0){
      prob <- rep(0,length(p))
    } else {
      res <- fitdistr(x,"gamma")
      prob <- dgamma(p,res$estimate[[1]],res$estimate[[2]])
    }
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
test.adjust <- function(x,test="ks",prob,estimate.by="histogram"){
  if(estimate.by == "histogram"){
    br <- seq(round(min(x))-0.5,round(max(x))+0.5,1)
    h <- hist(x, plot=FALSE, freq=TRUE,breaks=br)
    p <- h$count
  } else {
    p <- density$y
  }
  prob <- prob/sum(prob)
  prob[is.na(prob)] <- 0
  if(test=="chi2"){
    if(sum(prob)!=1){
      pval = 0
    } else {
      pval <- chisq.test(x=p,p=prob)$p.value
    }
  }
  if(test=="ks"){
    pval <- ks.test(x, y=prob)$p.value
  }
  return(pval)
}

#`@export
plot.ecdf.adjust <- function(x,probs,estimate.by="histogram"){
  if(estimate.by == "histogram"){
    br <- seq(round(min(x))-0.5,round(max(x))+0.5,1)
    h <- hist(x, plot=FALSE, freq=TRUE,breaks=br)
    a <- h$mid
    b <- h$density
  } else {
    a <- density$x
    b <- density$y
  }
  par(mar=c(2,2,2,2))
  plot(ecdf(x),main="",col="gray")
  colors <- c("red","blue","green","purple","orange")
  for(i in c(1:length(probs))){
    p <- probs[[i]]/sum(probs[[i]])
    lines(x=sort(a),y=cumsum(p),col=colors[[i]],lty=2,lwd=2)
  }
}

#`@export
plot.hist.adjust <- function(x, probs,estimate.by="histogram"){
  if(estimate.by == "histogram"){
    br <- seq(round(min(x))-0.5,round(max(x))+0.5,1)
    h <- hist(x, plot=FALSE, freq=TRUE,breaks=br)
    a <- h$mid
    b <- h$density
  } else {
    a <- density$x
    b <- density$y
  }
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
  x <- rnorm(1000,100,10)
  laws <- c("Uniforme","Normale","Gamma","Poisson","Exponentielle")
  probs <- lapply(laws,function(i){print(i);theorique.prob(x,law=i)})
  names(probs) <- laws
  plot.hist.adjust(x,probs)
  plot.ecdf.adjust(x,probs)
  pvals <- lapply(c(1:length(probs)),function(i){print(i);
    test.adjust(x,test="chi2",probs[[i]])})
  names(pvals) <- laws
}
