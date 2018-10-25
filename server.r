library(shiny)
library(shinyjs)

cukes <- expand.grid(smp = paste0("S", 1:6), cell = 1:6, trap = c("A", "B", "C", "D"))
c.name <- paste0(cukes$smp, "_", cukes$cell, cukes$trap)
s1 <- grep("S1", c.name, value = TRUE)

cuke1 <- sample(s1, 1)
cuke1_choice <- sample(c(cuke1, sample(s1, 3)))

# Build survey:
NCukes <- 12
surv <- data.frame(Cuke = sample(s1, NCukes, replace = TRUE), 
	Period = sample(c("S2", "S3", "S4", "S5", "S6"), NCukes, replace = TRUE), 
	"Present" = sample(c(rep(1, 8), rep(0, 4))))
surv$answer <- sample(1:4, NCukes, replace = TRUE)
surv$answer[surv$Present == 0] <- 5

cuke.opt <- c()
for(i in 1:nrow(surv))
{
	t.p <-  gsub("S1", paste0(surv$Period[i]), surv$Cuke[i])
	cuke.poss <- grep(surv$Period[i], c.name[c.name != t.p], value = TRUE)
	cuke.opt <- rbind(cuke.opt, sample(cuke.poss, 4))
	if(surv$Present[i] == 1) cuke.opt[i,surv$answer[i]] <- t.p
}

shinyServer(function(input, output) {

	#Observe consent
	# observeEvent(input$Consent, {
		# removeUI(selector='#Consent', immediate=TRUE)
	# }, autoDestroy=TRUE)
		
	# Create some page types:
	output$MainAction <- renderUI( {
		dynamicUi()
	  })
	 
	# Dynamic UI is the interface which changes as the survey
	# progresses.  
	dynamicUi <- reactive({

    # Initially it shows a welcome message. 
		if (input$pg == 0){
				output$caption <- renderText({ "Click next to the begin the survey." })
			return(
				list(
					includeMarkdown("data/Consent.md")
				)
			)
		}
		
		if (input$pg == 1){
				output$caption <- renderText({ paste0("Click next to submit.") })
			return(
				list(					
					fluidRow(style='padding:14px;',
						h3("Please answer the following demographic questions:"),
						br(),
						"I would describe my knowledge of marine invertebrates as:",
						selectInput("D_Q1", "",
						c("None",	"A Little",	"Moderate",	"Experienced",	"Very Experienced")),
						"I would describe my knowledge of sea cucumbers as:",
						selectInput("D_Q2", "",
						c("None",	"A Little",	"Moderate",	"Experienced",	"Very Experienced")),
						"My ages is:",
						numericInput("D_Q3", "", min = 15, max = 80, step = 1, value = 20),
						"I have problems differentiating colours:",
						radioButtons("D_Q4", "", 
							choices = c("True" = 1, "False" = 0),
							inline = TRUE),
							"Is there anything else you'd like us to know about your ability to match sea cucumbers or experience with marine invertebrates",
						textAreaInput("D_Q5", "", value = "")
						)
					)
				)
		}
		
		# Once the next button has been clicked once we see each question
		# of the survey.
		if (input$pg >= 2 & input$pg <= NCukes + 1 ){
			delay(20, shinyjs::runjs("window.scrollTo(0, 0)"))	#Scroll to top to start question
			output$caption <- renderText({ paste0("Click next to submit. (Correct Answer:", LETTERS[surv$answer[input$pg]], ")") })
			return(
			list(
				fluidRow(style='padding:14px;',
					h3("Sea Cucumber Photo ID"),
				p("On the left are 3 photographs taken of the same sea cucumber (Left, Top, Right sides) all taken at the same time. 
				On the right are a number of sea cucumbers that may or may not be the same sea cucumber in a different time period. 
				By clicking on the different options you can look at the different sea cucumber images.
				Once you are confident you have a match please scroll down and submit it by pressing next."),
					column(6, h3("Match this Sea Cucumber")),
					column(6,
						radioButtons("Q1", "Look at the different Sea Cucumbers", 
							choices = c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "Not Present" = 5),
							inline = TRUE)
						)
					),	
				fluidRow(
					column(6,
						imageOutput(outputId = "fig1",  height = "800px")
					),
					column(6,
						imageOutput(outputId = "fig1_choice",  height = "800px")
					)
				)
			)
			)
		}		
	})
	
	# The option list is a reative list of elements that
	# updates itself when the click counter is advanced.
	option.list <- reactive({
		if(input$pg >= 1 & input$pg <= NCukes) 
			return(cuke.opt[input$pg,]) # Make something happen
		})	
	cuke.match <- reactive({
		if(input$pg >= 1 & input$pg <= NCukes) 
			return(surv$Cuke[input$pg]) # Make something happen
		})	

		
	output$fig1 <- renderImage({
		# When input$n is 1, filename is ./images/image1.jpeg
		filename <- normalizePath(paste0("images/", cuke.match(), ".png"))
		# Return a list containing the filename
		list(src = filename)
	}, deleteFile = FALSE)

	output$fig1_choice <- renderImage({
		cuke <- option.list()
		# When input$n is 1, filename is ./images/image1.jpeg
		if(input$Q1 != 5) filename <- normalizePath(paste0("images/", cuke[as.integer(input$Q1)], ".png"))
		if(input$Q1 == 5) filename <- normalizePath(paste0("images/Blank.jpg"))
		# Return a list containing the filename
		list(src = filename)
	}, deleteFile = FALSE)
 
 })
