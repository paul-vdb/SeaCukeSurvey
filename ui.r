library(shiny)

shinyUI(
	fluidPage(
		# Main Action is where most everything is happenning in the
		# object (where the welcome message, survey, and results appear)
		actionButton("pg", "Next"),
		uiOutput("MainAction")
		# This displays the action putton Next.
		 
		
		# sidebarLayout(position = "right",
            # sidebarPanel("Select", br(), actionButton("pg", "Next")  ),
            # mainPanel(uiOutput("MainAction"))
		# )			
	)
)