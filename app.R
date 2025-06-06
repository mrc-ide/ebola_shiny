#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load libraries
library(shiny)
library(shinydashboard)
library(shinysurveys)
library(tidyverse)
# library("googlesheets4")
# library("DT")

# # get token to access
# shiny_token <- gs4_auth()
# saveRDS(shiny_token,"shiny_app_token.rds")

# set up data in google drive
#Data <- gs4_create("Ebola_data")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Expert elicitation for the basic reproduction number of Ebola"),
  
  sidebarPanel(
    "The basic reproduction number of a pathogen (R0) is the number of infections caused by an infectious individual in an otherwise completely susceptible population. 
    Based on your knowledge and experience of recent Ebola outbreaks:",
    
    selectInput("answerR0","Can you provide an estimate for the distribution of RO?",
                c("No","Yes")),
    
    conditionalPanel(
      condition="input.answerR0=='Yes'",
      selectInput("r0_shape","What do you think the shape of the distribution of R0 is?",
                  c("Uniform","Normal","Skewed")),
      
      conditionalPanel(
        condition="input.r0_shape=='Uniform'",
        sliderInput("r0_min","What do you think the minimum value of R0 is?",min=0,max=3,value=1.5,step=0.1,round=-1)
      ),
      
      conditionalPanel(
        condition="input.r0_shape=='Uniform'",
        sliderInput("r0_max","What do you think the maximum value of R0 is?",min=1.5,max=10,value=3,step=0.1,round=-1)
      ),
      
      conditionalPanel(
        condition="input.r0_shape=='Normal'",
        sliderInput("r0_mean","What do you think the mean value of R0 is?",min=1,max=5,value=2,step=0.1,round=-1)
      ),
      
      conditionalPanel(
        condition="input.r0_shape=='Normal'",
        sliderInput("r0_sd","What do you think the standard deviation of R0 is?",min=0.1,max=2,value=0.5,step=0.01,round=-2)
      ),
      
      conditionalPanel(
        condition="input.r0_shape=='Skewed'",
        sliderInput("r0_means","What do you think the mean value of R0 is?",min=1,max=5,value=1.5,step=0.1,round=-1)
      ),
      
      conditionalPanel(
        condition="input.r0_shape=='Skewed'",
        sliderInput("r0_var","What do you think the variance of R0 is?",min=0.01,max=2,value=0.5,step=0.01,round=-2)
      )
    )
    
  ),
  
  mainPanel(
    conditionalPanel(
      condition="input.answerR0=='Yes'",
      plotOutput("plot"),
      
      selectInput("conf_R0","How confident are you in your estimate?",
                  c("Very","Somewhat","Not very","Not at all")),
      
      textAreaInput("source_R","Please provide any context or sources for your estimate:",
                    ),
      
      actionButton("submit","Submit")
    )
  )
 
)

# Define server logic required to plot the output
server <- function(input, output, session) {
  
  plotType <- reactive({input$r0_shape
  })
  
  xmin<-0
  xmax<-10
  
  output$plot <- renderPlot({
    if(plotType()=="Uniform"){
      r0min<-input$r0_min
      r0max<-input$r0_max
      xpos<-seq(xmin,xmax,by=0.01)
      ypos<-dunif(xpos,min=r0min,max=r0max,log=FALSE)
      
      plot(xpos,ypos,type='l',xlab='R0',ylab='probability',main="Probability density function for R0")
    }
    else if(plotType()=="Normal"){
      r0mean<-input$r0_mean
      r0sd<-input$r0_sd
      xpos<-seq(xmin,xmax,by=0.01)
      ypos<-dnorm(xpos,mean=r0mean,sd=r0sd,log=FALSE)
      denom<-(pnorm(xmax,r0mean,r0sd)-pnorm(xmin,r0mean,r0sd))
      ypos<-ypos/denom
      
      plot(xpos,ypos,type='l',xlab="R0",ylab='probability',main="Probability density function for R0")
    }
    else if(plotType()=="Skewed"){
      r0means<-input$r0_means
      r0var<-input$r0_var
      #then make these into gamma distribution parameters
      r0scale<-r0var/r0means
      r0sh<-(r0means*r0means)/r0var
      
      xpos<-seq(0,10,by=0.01)
      ypos<-dgamma(xpos,shape=r0sh,scale=r0scale,log=FALSE)
      
      plot(xpos,ypos,type='l',xlab="R0",ylab='probability',main="Probability density function for R0")
    }
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
