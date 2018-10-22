library(rsconnect)

setwd("./SeaCukeSurvey")

source("ui.r")
source("server.r")
shinyAppDir("C:/Users/Paul/Documents/Sea Cucumbers/PhotoID/Survey/SeaCukeSurvey")

rsconnect::deployApp("C:/Users/Paul/Documents/Sea Cucumbers/PhotoID/Survey/SeaCukeSurvey")