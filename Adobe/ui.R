
library(RColorBrewer) 
library(scales) 
library(lattice) 
library(dplyr) 
library(shiny)
library(ggplot2)



ui <- bootstrapPage(
  headerPanel ("Final Result"),
  
  sidebarPanel( 
    selectInput("id", "Select Module ID", unique(shiny_assessments$id)),
    selectInput("info", "Select Demographic Type",c("gender", "region", "highest_education",
                                                "imd_band","age_band", "disability"))
   
  ),
  
  
  mainPanel(
    
    plotOutput("plot", width = "100%"),
    plotOutput("plot2", width = "100%"),
    
    plotOutput("plot3", width = "100%")
  
  )
  
    

    
    
  
)
