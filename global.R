library(shiny)
library(leaflet)
library(ShinyDash)

source("Functions/functions.R")

load("data.RData")
vars <- names(dataW)[c(2:7, 16)]
