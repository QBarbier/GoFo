## Load Library
library("shiny")
library("shinydashboard")
library("shinyjs")

## Setting input size max
options(shiny.maxRequestSize=100*1024^2,
        shiny.fullstacktrace = TRUE)

shinyServer(function(input, output) {

	## Save table value
	listValues <- reactiveValues(
		table =  1
	)
	## edit

	## Upload a csv Table
	output$fileData <- reactive({
		if(is.null(input$csv_table)) return(NULL)
		header <- input$header
		listValues$table <- read.csv(input$csv_table$datapath, header=header, check.names=FALSE)
		return(TRUE)
	})

	## Select one columns to analyse
	output$selectCol <- renderUI({
		if(is.double(listValues$table)) return(NULL)
		colSelect <- colnames(listValues$table)
		selectInput("cols","Select Row",colSelect)
	})

	## Plot summary and head of the values
	output$summary <- renderTable({
		if(is.double(listValues$table)) return(NULL)
		if(is.null(input$cols)) return(NULL)
		if(!input$cols%in%colnames(listValues$table)) return(NULL)
		tb <- summary(listValues$table[,input$cols])
		df <- data.frame(matrix(tb, nrow=1))
		row.names(df) <- "Summary"
		colnames(df) <- names(tb)
		df <- t(df)
		df <- cbind(df,head(listValues$table[,input$cols]))
		colnames(df)[2] <- "Head file"
		return(df)
	},rownames = TRUE)

	## Run Demo, a table object create with multiple law distribution for test tools
	observeEvent(input$demo, {
		norm <- rnorm(1000,10,2)
		pois <- rpois(1000,lambda=2)
		equi <- sample(c(1:10), 1000,replace=TRUE)
		bino <- rbinom(1000,c(1:5), prob=0.5)
		pois2 <- rpois(1000, lambda=50)
		mat <- matrix(c(norm,pois,equi,bino,pois2), nrow=1000, byrow=FALSE)
		colnames(mat) <- c("Normale(10,2)","Poisson(5)","Equiprobable(1:10)","Binomial(1:5,0.5)","Poisson(50)")
		listValues$table <- data.frame(mat, check.names=FALSE)
	})

	## main function of the tools, compute theorical distribution, observed distribution and make statistical test
	observeEvent(input$run, {
		obs <- as.vector(listValues$table[,input$cols])
		br <- seq(round(min(obs))-0.5,round(max(obs))+0.5,1)
		h <- hist(obs, plot=FALSE, freq=TRUE,breaks=br)
		x.obs <- h$mid
		y.obs <- h$density

		# Merge event for khideux test
		if(input$merged==TRUE){

		}

		# Normal theorical parts
		if(input$law =="Normal"){
			norm <- dnorm(x.obs, mean=mean(obs), sd = sd(obs))
			x.the <- x.obs
			y.the <- norm
		}

		# Poisson theorical parts
		if(input$law =="Poisson"){
			norm <- dpois(abs(ceiling(x.obs)), lambda=abs(mean(obs)))
			if(length(which(x.obs<0))>0 || length(which((obs%%1==0)==FALSE))>0){
				output$warnings <- renderText({"Poisson law have discret values and non-negative"})
			}
			x.the <- x.obs
			y.the <- norm
		}

		# Equiprobality distribution theorical
		if(input$law == "Equiprobability"){
			x.the <- x.obs
			y.the <- rep(1/length(x.the),length(x.the))
		}

		# Compute statistical test with theorical distribution and observed values and plot it
		output$pvalueTable <-renderTable({
			p <- y.the/sum(y.the)
			if(input$test=="Khi-deux") pval <- chisq.test(h$count,p=p)$p.value
			if(input$test=="Kolmogorov-Smirnov") pval <- ks.test(obs, y=p)$p.value
			if(input$test=="Shapiro-Wilks") pval <- shapiro.test(obs)$p.value
			if(pval<=0.05){
				fitting <- "FALSE"
			} else {
				fitting <- "TRUE"
			}
			mat <- data.frame(c(input$law,round(pval,5),input$test, fitting))
			row.names(mat) <- c("Law Fitting","Pvalue","Test","Fitting")
			colnames(mat) <- "Result Table"
			return(mat)
		},rownames = TRUE)

		# Plot observed in blue bar and red theorical line for theorical law
		output$plot <- renderPlot({
			plot(x.obs, y.obs, ylab="Freq",xlab="Values", type="h", lwd=20, col="blue", ylim=c(0,1.5*max(y.obs,y.the)), cex.lab=1.2, cex.axis=1.2)
			grid()
			legend(min(x.obs), 1.5*max(y.obs,y.the), legend=c("Theorical","Observed"), col=c("red","blue"), lty=2:1, cex=1.2, lwd=c(2,2))
			lines(x.the, y.the, col="red", lwd=4, type="b", lty=1)
		})

		# print the theorical distribution with observed distribution
		output$sumbitTable <- renderTable({
			tab <- matrix(c(x.obs,h$count,y.obs,y.the/sum(y.the)),nrow=length(y.obs), byrow=FALSE)
			colnames(tab)<- c("Values","Events","Observed freq","Theorical freq")
			return(tab)
		})
	})

	output$tableLoad <- reactive({
		if(is.double(listValues$table)) return(NULL)
		return(TRUE)
	})

	observe({
		if(input$law =="Normal"){
			output$lawInfo <- renderUI({a("Normal", href='https://en.wikipedia.org/wiki/Normal_distribution', targer="_blank")})
		}
		if(input$law =="Poisson"){
			output$lawInfo <- renderUI({a("Poisson", href='https://en.wikipedia.org/wiki/Poisson_distribution', targer="_blank")})
		}
		if(input$law =="Equiprobability"){
			output$lawInfo <- renderUI({a("Equiprobability", href='https://en.wikipedia.org/wiki/Equiprobability', targer="_blank")})
		}
		if(input$law =="Binomial"){
			output$lawInfo <- renderUI({a("Binomial", href='https://en.wikipedia.org/wiki/Binomial_distribution', targer="_blank")})
		}
	})

	observe({
		if(input$test =="Khi-deux"){
			output$testInfo <- renderUI({a("Khi-deux", href='https://fr.wikipedia.org/wiki/Test_du_χ²', targer="_blank")})
		}
		if(input$test =="Kolmogorov-Smirnov"){
			output$testInfo <- renderUI({a("Kolmogorov", href='https://fr.wikipedia.org/wiki/Test_de_Kolmogorov-Smirnov', targer="_blank")})
		}
		if(input$test =="Shapiro-Wilks"){
			output$testInfo <- renderUI({a("Shapiro-Wilks", href='https://fr.wikipedia.org/wiki/Test_de_Shapiro-Wilk', targer="_blank")})
			output$warnings <- renderText({"Shapiro-Wilks test is just used to test normal fitting law ! \nThe Law's adequation is not used"})
		}
	})

	outputOptions(output, "tableLoad", suspendWhenHidden=FALSE)
	outputOptions(output, "fileData", suspendWhenHidden=FALSE)

})
