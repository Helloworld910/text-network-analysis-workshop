#Install Package Once
install.packages("httr")
install.packages("jsonlite")
install.packages("tidyverse")

#Load Libraries
library(httr)
library(jsonlite)

#Set Working Directory (Provide the Path to Your Working Folder)
setwd(dir = "E:/Charles University/Semantic Network Workshop/text-network-analysis-workshop")
getwd()

#Use Package to Make a Get Request
response <- GET("https://cat-fact.herokuapp.com/facts/?animal_type=cat")

#Process Response
response <- fromJSON(content(response, "text"))

#See Response
response$text
