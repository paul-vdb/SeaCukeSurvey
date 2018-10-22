library(shiny)

cukes <- expand.grid(smp = paste0("S", 1:6), cell = 1:6, trap = c("A", "B", "C", "D"))
c.name <- paste0(cukes$smp, "_", cukes$cell, cukes$trap)
s1 <- grep("S1", c.name, value = TRUE)

cuke1 <- sample(s1, 1)
cuke1_choice <- sample(c(cuke1, sample(s1, 3)))

# Build survey:
surv <- data.frame(Cuke = sample(s1, 12, replace = TRUE), 
	Period = sample(c("S2", "S3", "S4", "S5", "S6"), 12, replace = TRUE), 
	"Present" = sample(c(rep(1, 8), rep(0, 4))))
surv$answer <- sample(1:4, 12, replace = TRUE)
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
			return(
				list(
					includeMarkdown("data/Consent.md")
				)
			)
		}

		# Once the next button has been clicked once we see each question
		# of the survey.
		if (input$pg >= 1){
			return(
			list(
				fluidRow(
					column(6, h2("Can You Match this Sea Cucumber?")),
					column(6,
						radioButtons("Q1", "Look at the different Sea Cucumbers", 
							choices = c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "Not Present" = 5),
							inline = TRUE),
						renderText(paste("Cheet Sheet", surv$answer[input$pg]))
						)
					),	
				fluidRow(
					column(6,
						imageOutput(outputId = "fig1",  height = "400px")
					),
					column(6,
						imageOutput(outputId = "fig1_choice",  height = "400px")
					)
				)
			)
			)
		}		
	})
	
	# The option list is a reative list of elements that
	# updates itself when the click counter is advanced.
	option.list <- reactive({
		if(input$pg >= 1 & input$pg <= 12) return(cuke.opt[input$pg,]) # Make something happen
		})	
	cuke.match <- reactive({
		if(input$pg >= 1 & input$pg <= 12) return(surv$Cuke[input$pg]) # Make something happen
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
