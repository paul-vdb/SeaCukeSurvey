library(shiny)
library(shinyjs)
library(markdown)
library(rdrop2)
library(shinyWidgets)


cukes <- expand.grid(smp = paste0("S", 1:6), cell = 1:6, trap = c("A", "B", "C", "D"))
c.name <- paste0(cukes$smp, "_", cukes$cell, cukes$trap)
s1 <- grep("S1", c.name, value = TRUE)

cuke1 <- sample(s1, 1)

# Build survey:
NCukes <- 16
n.opts <- 5
surv <- data.frame(Cuke = sample(s1, NCukes, replace = TRUE), 
	Period = sample(c("S2", "S3", "S4", "S5", "S6"), NCukes, replace = TRUE), 
	"Present" = sample(c(rep(1, NCukes - 4), rep(0, 4))))
surv$answer <- sample(1:n.opts, NCukes, replace = TRUE)
surv$answer[surv$Present == 0] <- n.opts + 1

cuke.opt <- c()
for(i in 1:nrow(surv))
{
	t.p <-  gsub("S1", paste0(surv$Period[i]), surv$Cuke[i])
	cuke.poss <- grep(surv$Period[i], c.name[c.name != t.p], value = TRUE)
	cuke.opt <- rbind(cuke.opt, sample(cuke.poss, n.opts))
	if(surv$Present[i] == 1) cuke.opt[i,surv$answer[i]] <- t.p
}
results <- surv
results$response <- "0"
demograph <- data.frame(Cuke = paste0("Demo_Question_", 1:5), Period = NA, Present = NA, answer = NA, response = "0", stringsAsFactors = FALSE)

shinyServer(function(input, output, session) {

	#Observe consent
	# observeEvent(input$Consent, {
		# removeUI(selector='#Consent', immediate=TRUE)
	# }, autoDestroy=TRUE)
		
	# Text button Label starts as "Next"	
	output$Button <- renderText({"Next"})
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
				output$progress <- renderText({""})

			return(
				list(
					includeMarkdown("data/Consent.md")
				)
			)
		}
		
	# Demographic information for the survey	
		if (input$pg == 1){
				output$caption <- renderText({ paste0("Click next to begin matching sea cucumbers.") })
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
		# Once the next button has been clicked twice we begin the sea cucumber photo id portion.
		# This should be 12 questions.
		if (input$pg >= 2 & input$pg <= NCukes + 1){
			output$progress <- renderText({paste0("You have completed ", input$pg - 2, " out of ", NCukes, " matches.")})
		
			# Let people know how long it is taking.
			updateProgressBar(session = session, id = "pb1", value = (input$pg - 2)/NCukes*100)
		
			delay(20, shinyjs::runjs("window.scrollTo(0, 0)"))	#Scroll to top to start question
			output$caption <- renderText({"Click next to submit."})
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
							radioButtons("QMatch", "Look at the different Sea Cucumbers", 
								choices = c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5, "Not Present" = 6),
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
		if (input$pg > NCukes + 1 ){
		output$score <- renderText({ paste0("You scored ", sum(results$response == results$answer), " correct out of ", NCukes, ".") })
		output$caption <- renderText({ paste0("Click Submit to send us your results and exit the survey.") })
		output$progress <- renderText({paste0("Congrats you completed the Survey!")})
		# Let people know how long it is taking.
		updateProgressBar(session = session, id = "pb1", value = 100)


		output$Button <- renderText({"Submit"})
			return(
				list(
					fluidRow(style='padding:14px;',
						h3("Sea Cucumber Photo ID"),
					textOutput("score"),
					p("You have now completed the sea cucumber photo identification study."),
					p("Thank you for your participation!")
					)
				)
			)
		}
	})
	
	###############################################################
	# Build sea cucumber photos:
	# The option list is a reative list of elements that
	# updates itself when the click counter is advanced.
	###############################################################
	option.list <- reactive({
		if(input$pg >= 2 & input$pg <= NCukes + 1) 
			return(cuke.opt[input$pg - 1,]) # Make something happen
		})	
	cuke.match <- reactive({
		if(input$pg >= 2 & input$pg <= NCukes + 1) 
			return(surv$Cuke[input$pg - 1]) # Make something happen
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
		if(input$QMatch != (n.opts + 1)) filename <- normalizePath(paste0("images/", cuke[as.integer(input$QMatch)], ".png"))
		if(input$QMatch == (n.opts + 1)) filename <- normalizePath(paste0("images/Blank.jpg"))
		# Return a list containing the filename
		list(src = filename)
	}, deleteFile = FALSE)
	###
		
	####################
	# Keep track of data
	####################
	# Demographics
	observe({

	})	
	
	# Sea cuke match
	observeEvent(input$pg,{
			if(input$pg == 2){
				demograph$response[1] <<- paste(input$D_Q1)
				demograph$response[2] <<- paste(input$D_Q2)
				demograph$response[3] <<- paste(input$D_Q3)
				demograph$response[4] <<- paste(input$D_Q4)
				demograph$response[5] <<- paste(input$D_Q5)
			} 
			if(input$pg >= 3 & input$pg <= NCukes + 2){
				results$response[input$pg - 2] <<- input$QMatch
			}
			if(input$pg == NCukes + 3){
				results <- rbind(demograph, results)
				sessionID <- floor(runif(1, 1, 1000000))
				fn <- paste0("output/", Sys.Date(), "_", sessionID,  "_Results.csv")
				write.csv(results, file = fn, row.names = FALSE)
				drop_upload(fn, path = "SeaCucumberPhotoIDResults")
				shinyjs::runjs("window.close();")
				stopApp() 
			}
	})

 })
