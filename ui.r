library(shiny)
library(shinyjs)

shinyUI(
	fluidPage(theme = "bootstrap.css",
		# Main Action is where most everything is happenning in the
		# object (where the welcome message, survey, and results appear)
		useShinyjs(),
		uiOutput("MainAction"),
		
		wellPanel(
			textOutput("caption"),
			actionButton("pg", "Next")
		)

		
		# This displays the action putton Next.
		 
		
		# sidebarLayout(position = "right",
			
            # sidebarPanel(width = 2,
				# textOutput("caption"),
				# br(), 
				# actionButton("pg", "Next")  ),
            # mainPanel(uiOutput("MainAction"))
		# )			
	)
)