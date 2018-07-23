# File: ui.r                                                               
# Created by: Mickey Guo                                                   
# UI Function of the Buyer Overview Shiny App                         

# Setup, Library Imports --------------------------------------------------

library(tidyverse)                                                         
library(plotly)                                                            
library(kableExtra)                                                        
library(lubridate)                                                         
library(DT)                                                                
library(scales)                                                            
library(shiny)                                                             
library(crosstalk)                                                         
library(magrittr)                                                          
library(formattable)

# Clear workspace                                                          
rm(list = ls())                                                            

# Plotly APIs, no use right now but just in case                           
Sys.setenv("plotly_username" = "Zenmai0822")                               
Sys.setenv("plotly_api_key" = "1qC2QkZBYFrJzOG9RW9i")                      


# Variables, Verify before Running ----------------------------------------

# Working Directory                                                        
setwd("C:/Users/nguo/Documents/github/BuyerOverview")     


# Grab all variables and data from each page ------------------------------

source("pages/1-buyer_overview.r")

navbarPage("Buyer Performance", 
           uiPage1,
           tabPanel("Overview"))