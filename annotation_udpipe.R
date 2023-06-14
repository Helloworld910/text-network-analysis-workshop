#Uploading the Required Libraries
library(tidyverse)
library(magrittr)
library(udpipe)
library(pdftools)

#Environment Setup
rm(list = ls())
setwd("e:/charles university/semantic network workshop/text-network-analysis-workshop")
getwd()

#Loading Libraries
library(pdftools)

#Loading Udpipe Model
ud_model <- udpipe_load_model('english-ewt-ud-2.5-191206.udpipe')

#Uploading Data
rightwing_text <- pdf_text("data/rightwing_text_1.pdf")


#Annotating
x <- udpipe_annotate(ud_model, x = rightwing_text)
x <- as.data.frame(x)
x <- as_tibble(x)

#Saving Annotated Tibble
write_csv(x, "data/annotated_rightwing.csv", col_names = TRUE, na = "NA")

