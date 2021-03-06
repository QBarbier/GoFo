library("shiny")

shinyUI(fluidPage(
	titlePanel("GoFo"),
	tags$head(
		tags$style(HTML("
			table {
				border:1px solid gray;
				background-color:white;
				margin-top:20px;
			}
		"))
	),
	tags$h3("Goodness of Fitting online"),
	sidebarLayout(
		sidebarPanel(
			fileInput("csv_table","Upload (100 Mo)",multiple=FALSE),
			checkboxInput("header" ,label="Header Used" ,value = TRUE),
			selectInput("law","Law's adequation",
				c("Normale"="Normale",
				  "Poisson"="Poisson",
				  "Uniforme"="Uniforme",
				  "NegBinomial"="NegBinomial",
				  "Gamma"="Gamma",
				  "LogNormale"="LogNormale"
				)
			),
			uiOutput("lawInfo"),
			selectInput("test","Statistical's Test",
				c("Khi-deux"="chi2",
				  "Kolmogorov-Smirnov"="Kolmogorov-Smirnov",
				  "Shapiro-Wilks"="Shapiro-Wilks"
				 )
			),
			uiOutput("testInfo"),
			conditionalPanel(
				condition="input.test == 'Khi-deux'",
				checkboxInput("merged",label="Merge Events", value=TRUE)
			),
			actionButton("demo","Demo"),
			tags$br(),tags$br(),
			conditionalPanel(
			 	condition="output.tableLoad",
				uiOutput("selectCol"),
				actionButton("run","Run"),
				tableOutput("summary")
			)
		),
		mainPanel(
			conditionalPanel(
				condition="output.tableLoad",
				verbatimTextOutput("warnings",placeholder = FALSE),
				plotOutput("plot", heigh=600),
				tableOutput("pvalueTable"),
				tableOutput("sumbitTable")
			)
		)
	)
))
