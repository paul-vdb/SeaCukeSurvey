library(rsconnect)

setwd("C:/Users/Paul/Documents/Sea Cucumbers/PhotoID/Survey/ShinyApp")

source("ui.r")
source("server.r")
shinyAppDir("C:/Users/Paul/Documents/Sea Cucumbers/PhotoID/Survey/ShinyApp")

rsconnect::deployApp("C:/Users/Paul/Documents/Sea Cucumbers/PhotoID/Survey/ShinyApp")