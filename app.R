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
library(tidyverse)
library(bslib)
library(ggplot2)
library(truncnorm)
# library("googlesheets4")
# library("DT")

# # get token to access
# shiny_token <- gs4_auth()
# saveRDS(shiny_token,"shiny_app_token.rds")

# set up data in google drive
#Data <- gs4_create("Ebola_data")

# Define UI for application that draws a histogram
ui <- page_navbar(
  
  title="Expert elicitation for Ebola epidemiological parameters",
  id="mainpage",
  
  bslib::nav_panel(title="Overview",
      
    span(p("Rationale"),style="font-size: 24px"),
    
    p("Mathematical modelling studies to inform Ebola virus disease (EVD) outbreak response policy are highly reliant on input parameters, which are 
      often derived from epidemiological data. Whilst there is reasonable empirical evidence underpinning some parameters (see Nash et al, 2024), 
      other parameters often cannot be fully observed or are not routinely reported, and therefore do not appear in published literature. However, 
      many of these parameters, related to both transmission and outbreak response, have important implications for policy decisions."),
    
    p("This survey aims to fill these gaps in the literature using the subjective judgement of experts with experience working in EVD outbreak response.
       We are specifically interested in parameters pertaining to: reproduction nunber, case ascertainment, contact tracing and vaccination. We would welcome your 
       input on any or all of these parameters; please feel free to skip any parameters for which you don't feel confident making a judgement about."),
    
    span(p("Explanatory notes"),style="font-size: 24px"),
    
    p("The first page of the survey asks for some basic information on your experience with EVD outbreak responses. The remaining pages focus 
      on parameter areas of interest. For each parameter, you can select a distribution and use sliders to vary the shape of your judgement. 
      Visualisations and summary statistics are provided to help inform your intuition. There is also a dropdown box that enables you to specify 
      how confident you are in your judgement and a free text field to provide extra context or sources related to your judgement. 
      Please consider the following when making judgements about your confidence:"),
    
    p("Very:"),
    
    p("Somewhat:"),
    
    p("Slightly:"),
    
    p("Not at all:"),
    
    p("For more information on this survey, please contact: Anne Cori (anne.cori@imperial.ac.uk), Katharina Hauck (k.hauck@imperial.ac.uk) or 
      Gemma Nedjati-Gilani (g.nedjati-gilani@imperial.ac.uk)"),
    
  
    actionButton("nextOverview","Next")
  ),
  
  nav_panel(title="Your experience",
            p("We'd like to start the survey by asking about your experience during EVD outbreak responses:"),
            
            radioButtons(inputId="ExpEpi",label="Do you have experience analysing epidemiological data?",choices=c("No","Yes"),width="50%"),
            conditionalPanel(condition="input.ExpEpi=='Yes'",
                             numericInput("ExpEpi_length","If yes, how many years experience do you have?",min=0,max=20,value=0,step=0.5,width="50%")
                             ),
            radioButtons(inputId="ExpCase",label="Do you have experience with case investigation and ascertainment?",choices=c("No","Yes"),width="100%"),
            conditionalPanel(condition="input.ExpCase=='Yes'",
                             numericInput("ExpCase_length","If yes, how many years experience do you have?",min=0,max=20,value=0,step=0.5,width="50%")
            ),
            radioButtons(inputId="ExpCT",label="Do you have experience with contact tracing?",choices=c("No","Yes"),width="100%"),
            conditionalPanel(condition="input.ExpCT=='Yes'",
                             numericInput("ExpCT_length","If yes, how many years experience do you have?",min=0,max=20,value=0,step=0.5,width="50%")
            ),
            radioButtons(inputId="ExpVacc",label="Do you have experience with reactive vaccination campaigns?",choices=c("No","Yes"),width="100%"),
            conditionalPanel(condition="input.ExpVacc=='Yes'",
                             numericInput("ExpVacc_length","If yes, how many years experience do you have?",min=0,max=20,value=0,step=0.5,width="50%")
            ),
            
            layout_column_wrap(1/2,
              actionButton("previousExp","Previous"),
              actionButton("nextExp","Next")
              )
            ),
  
  nav_panel(title="Reproduction number",
            p("The basic reproduction number of a pathogen (R0) is the number of infections caused by an infectious individual in an otherwise completely susceptible population.
            If you feel able to share your intuition for R0, please use the options in the sidebar to calibrate your guesstimate. If you don't feel able to give a guesstimate for R0, please continue to the next section"),
            
            
            card(
            layout_sidebar(
              
              sidebar=sidebar(title="Reproduction number (R0)",
                              width=300,
              p("Based on your knowledge and experience of recent Ebola outbreaks:"),

              selectInput("answerR0","Can you provide your intuition about the distribution of R0?",
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
                  sliderInput("r0_max","What do you think the maximum value of R0 is?",min=3,max=10,value=3,step=0.1,round=-1)
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

            conditionalPanel(
              condition="input.answerR0=='Yes'",
              plotOutput("plotR0",width="100%",height='500px'),
              textOutput("R0median"),
              textOutput("R0conf"),

              selectInput("conf_R0","How confident are you in your estimate?",
                            c("Very","Somewhat","Not very","Not at all"),width="50%"),

              textAreaInput("source_R","Please provide any context or sources for your estimate:",width="80%"
              )
            ),
            
            layout_column_wrap(1/2,
              actionButton("previousR0","Previous"),
              actionButton("nextR0","Next")
            )
          )
        )
            
  ),
  
  bslib::nav_panel(title="Case ascertainment",
                   
                   p("Info about case ascertainment"),
                   
                   layout_sidebar(
                     sidebar=sidebar(title="Case ascertainment",
                                     width=300,
                                     p("Based on your knowledge and experience of recent Ebola outbreaks:"),
                                     
                                     selectInput("answerAsc","Can you provide your intuition about the distribution of case ascertainment?",
                                                 c("No","Yes")),
                                     
                                     conditionalPanel(
                                       condition="input.answerAsc=='Yes'",
                                       selectInput("Asc_shape","What do you think the shape of the distribution of case ascertainment is?",
                                                   c("Uniform","Normal","Skewed")),
                                       
                                       conditionalPanel(
                                         condition="input.Asc_shape=='Uniform'",
                                         sliderInput("Asc_min","What do you think the minimum value of case ascertainment is?",min=0,max=1,value=0.5,step=0.1,round=-1)
                                       ),
                                       
                                       conditionalPanel(
                                         condition="input.Asc_shape=='Uniform'",
                                         sliderInput("Asc_max","What do you think the maximum value of case ascertainment is?",min=0.5,max=1,value=0.7,step=0.1,round=-1)
                                       ),
                                       
                                       conditionalPanel(
                                         condition="input.Asc_shape=='Normal'",
                                         sliderInput("Asc_mean","What do you think the mean value of case ascertainment is?",min=0,max=1,value=0.5,step=0.05,round=-2)
                                       ),
                                       
                                       conditionalPanel(
                                         condition="input.Asc_shape=='Normal'",
                                         sliderInput("Asc_sd","What do you think the standard deviation of case ascertainment is?",min=0.1,max=1,value=0.5,step=0.01,round=-2)
                                       ),
                                       
                                       conditionalPanel(
                                         condition="input.Asc_shape=='Skewed'",
                                         sliderInput("Asc_means","What do you think the mean value of case ascertainment is?",min=0,max=1,value=0.5,step=0.1,round=-1)
                                       ),
                                       
                                       conditionalPanel(
                                         condition="input.Asc_shape=='Skewed'",
                                         sliderInput("Asc_var","What do you think the variance of case ascertainment is?",min=0.01,max=0.25,value=0.1,step=0.001,round=-3)
                                       )
                                     )
                     ),
                     
                     conditionalPanel(
                       condition="input.answerAsc=='Yes'",
                       plotOutput("plotAsc",width="100%",height='500px'),
                       textOutput("Ascmedian"),
                       textOutput("Ascconf"),
                       
                       selectInput("conf_Asc","How confident are you in your estimate?",
                                   c("Very","Somewhat","Not very","Not at all"),width="50%"),
                       
                       textAreaInput("source_Asc","Please provide any context or sources for your estimate:",width="80%"
                       )
                     ),
                     layout_column_wrap(1/2,
                       actionButton("previousAsc","Previous"),
                       actionButton("nextAsc","Next")
                     )
                   )
                   
  ),
  
  bslib::nav_panel(title="Contact tracing",
                   layout_sidebar(
                     sidebar=sidebar(title="Contact tracing",
                                     width=400,
                                     p(""),
                                     
                     ),
                     
                     layout_column_wrap(1/2,
                                       actionButton("previousAsc","Previous"),
                                       actionButton("nextAsc","Next")
                     )
                   )
                   
  ),
  
  bslib::nav_panel(title="Vaccination",
                   layout_sidebar(
                     sidebar=sidebar(title="Vaccination",
                                     width=400,
                                     p(""),
                                     
                     ),
                     
                     layout_column_wrap(1/2,
                                        actionButton("previousAsc","Previous"),
                                        actionButton("nextAsc","Next")
                     )
                   )
                   
  ),
  
  bslib::nav_panel(title="Submit",
                   layout_column_wrap(1/2,
                                     actionButton("previousAsc","Previous"),
                                     actionButton("nextAsc","Next")
                   )
                   
                   
  ),
  
  nav_spacer(),
  
 
)

# Define server logic required to plot the output
server <- function(input, output, session) {
  
  
  xmin<-0
  xmax<-10
  xmaxUnit<-1
  qrt<-c(0,0.025,0.25,0.5,0.75,0.975,1)
  
  observeEvent(input$nextOverview,{
    updateNavbarPage(session=session,"mainpage",selected="Your experience")
  })
  
  observeEvent(input$previousExp,{
    updateNavbarPage(session=session,"mainpage",selected="Overview")
  })
  
  observeEvent(input$nextExp,{
    updateNavbarPage(session=session,"mainpage",selected="Reproduction number")
  })
  
  plotTypeR0 <- reactive({input$r0_shape
  })
  
  observeEvent(input$r0_min,{
    updateSliderInput(session,"r0_max",min=input$r0_min+0.1)
  })

  output$plotR0 <- renderPlot({
    if(plotTypeR0()=="Uniform"){
      dat<-data.frame(xpos=seq(xmin,xmax,by=0.01))
      dat$ypos<-dunif(dat$xpos,min=input$r0_min,max=input$r0_max,log=F)
      dat$qt  <- cut(punif(dat$xpos,min=input$r0_min,max=input$r0_max,log=F),breaks=qrt,labels=F)
    }
    else if(plotTypeR0()=="Normal"){
      dat<-data.frame(xpos=seq(xmin,xmax,by=0.01))
      dat$ypos<-dtruncnorm(x=dat$xpos,a=0,mean=input$r0_mean,sd=input$r0_sd)
      dat$qt  <- cut(ptruncnorm(dat$xpos,a=0,mean=input$r0_mean,sd=input$r0_sd),breaks=qrt,labels=F)
    }
    else if(plotTypeR0()=="Skewed"){
      #then make these into gamma distribution parameters
      r0scale<-input$r0_var/input$r0_means
      r0sh<-(input$r0_means*input$r0_means)/input$r0_var
      dat<-data.frame(xpos=seq(xmin,xmax,by=0.01))
      dat$ypos<-dgamma(dat$xpos,shape=r0sh,scale=r0scale,log=F)
      dat$qt  <- cut(pgamma(dat$xpos,shape=r0sh,scale=r0scale,log=F),breaks=qrt,labels=F)
    }
    
    ggplot(dat,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="R0",y="pdf",color="Percentile",title="Probability density of R0")+
      theme_gray()+theme(legend.position ="none")
    
  }
  )
  
  output$R0conf<-renderText({
    if(plotTypeR0()=="Uniform"){
      lower50<-qunif(0.25,input$r0_min,input$r0_max)
      upper50<-qunif(0.75,input$r0_min,input$r0_max)
      lower95<-qunif(0.025,input$r0_min,input$r0_max)
      upper95<-qunif(0.975,input$r0_min,input$r0_max)
    }
    else if(plotTypeR0()=="Normal"){
      lower50<-qtruncnorm(p=0.25,a=0,mean=input$r0_mean,sd=input$r0_sd)
      upper50<-qtruncnorm(p=0.75,a=0,mean=input$r0_mean,sd=input$r0_sd)
      lower95<-qtruncnorm(p=0.025,a=0,mean=input$r0_mean,sd=input$r0_sd)
      upper95<-qtruncnorm(p=0.975,a=0,mean=input$r0_mean,sd=input$r0_sd)
    }
    else if(plotTypeR0()=="Skewed"){
      lower50<-qgamma(0.25,scale=input$r0_var/input$r0_means,shape=(input$r0_means*input$r0_means)/input$r0_var)
      upper50<-qgamma(0.75,scale=input$r0_var/input$r0_means,shape=(input$r0_means*input$r0_means)/input$r0_var)
      lower95<-qgamma(p=0.025,scale=input$r0_var/input$r0_means,shape=(input$r0_means*input$r0_means)/input$r0_var)
      upper95<-qgamma(p=0.975,scale=input$r0_var/input$r0_means,shape=(input$r0_means*input$r0_means)/input$r0_var)
    }
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$R0median<-renderText({
    if(plotTypeR0()=="Uniform"){
      median<-qunif(0.5,input$r0_min,input$r0_max)
    }
    else if(plotTypeR0()=="Normal"){
      median<-qtruncnorm(p=0.5,a=0,mean=input$r0_mean,sd=input$r0_sd)
    }
    else if(plotTypeR0()=="Skewed"){
      median<-qgamma(p=0.5,scale=input$r0_var/input$r0_means,shape=((input$r0_means*input$r0_means)/input$r0_var))
    }
    paste("Your median value for R0 is:",round(median,digits=2))
  })
  
  observeEvent(input$previousR0,{
    updateNavbarPage(session=session,"mainpage",selected="Your experience")
  })
  
  observeEvent(input$nextR0,{
    updateNavbarPage(session=session,"mainpage",selected="Case ascertainment")
  })
  
  observeEvent(input$Asc_min,{
    updateSliderInput(session,"Asc_max",min=input$Asc_min+0.1)
  })
  
  plotTypeAsc <- reactive({input$Asc_shape
  })
  
  output$plotAsc <- renderPlot({
    if(plotTypeAsc()=="Uniform"){
      datAsc<-data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
      datAsc$ypos<-dunif(datAsc$xpos,min=input$Asc_min,max=input$Asc_max,log=F)
      datAsc$qt  <- cut(punif(datAsc$xpos,min=input$Asc_min,max=input$Asc_max,log=F),breaks=qrt,labels=F)
    }
    else if(plotTypeAsc()=="Normal"){
      datAsc<-data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
      datAsc$ypos<-dtruncnorm(x=datAsc$xpos,a=0,b=1,mean=input$Asc_mean,sd=input$Asc_sd)
      datAsc$qt  <- cut(ptruncnorm(datAsc$xpos,a=0,b=1,mean=input$Asc_mean,sd=input$Asc_sd),breaks=qrt,labels=F)
    }
    else if(plotTypeAsc()=="Skewed"){
      #then make these into gamma or beta distribution parameters
      AscShape<-(input$Asc_means*input$Asc_means)/input$Asc_var
      AscScale<-input$Asc_var/input$Asc_means
      AscAlpha<-input$Asc_means*(((input$Asc_means*(1-input$Asc_means))/input$Asc_var)-1)
      AscBeta<-(1-input$Asc_means)*(((input$Asc_means*(1-input$Asc_means))/input$Asc_var)-1)
      datAsc<-data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
      datAsc$ypos<-dgamma(datAsc$xpos,shape=AscShape,scale=AscScale,log=F)/pgamma(1,shape=AscShape,scale=AscScale)
      datAsc$qt  <- cut(pgamma(datAsc$xpos,shape=AscShape,scale=AscScale,log=F)/pgamma(1,shape=AscShape,scale=AscScale),breaks=qrt,labels=F)
    }
    
    ggplot(datAsc,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="Proportion of cases ascertained",y="pdf",color="Percentile",title="Probability density of case ascertainment proportion")+
      theme_gray()+theme(legend.position ="none")
    
  }
  )
  
  output$Ascconf<-renderText({
    if(plotTypeAsc()=="Uniform"){
      lower50<-qunif(0.25,input$Asc_min,input$Asc_max)
      upper50<-qunif(0.75,input$Asc_min,input$Asc_max)
      lower95<-qunif(0.025,input$Asc_min,input$Asc_max)
      upper95<-qunif(0.975,input$Asc_min,input$Asc_max)
    }
    else if(plotTypeAsc()=="Normal"){
      lower50<-qtruncnorm(p=0.25,a=0,b=1,mean=input$Asc_mean,sd=input$Asc_sd)
      upper50<-qtruncnorm(p=0.75,a=0,b=1,mean=input$Asc_mean,sd=input$Asc_sd)
      lower95<-qtruncnorm(p=0.025,a=0,b=1,mean=input$Asc_mean,sd=input$Asc_sd)
      upper95<-qtruncnorm(p=0.975,a=0,b=1,mean=input$Asc_mean,sd=input$Asc_sd)
    }
    else if(plotTypeAsc()=="Skewed"){
      AscAlpha<-input$Asc_means*(((input$Asc_means*(1-input$Asc_means))/input$Asc_var)-1)
      AscBeta<-(1-input$Asc_means)*(((input$Asc_means*(1-input$Asc_means))/input$Asc_var)-1)
      AscShape<-(input$Asc_means*input$Asc_means)/input$Asc_var
      AscScale<-input$Asc_var/input$Asc_means
      lower50<-qgamma(p=0.25*pgamma(1,shape=AscShape,scale=AscScale),shape=AscShape,scale=AscScale)
      upper50<-qgamma(p=0.75*pgamma(1,shape=AscShape,scale=AscScale),shape=AscShape,scale=AscScale)
      lower95<-qgamma(p=0.025*pgamma(1,shape=AscShape,scale=AscScale),shape=AscShape,scale=AscScale)
      upper95<-qgamma(p=0.975*pgamma(1,shape=AscShape,scale=AscScale),shape=AscShape,scale=AscScale)
    }
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$Ascmedian<-renderText({
    if(plotTypeAsc()=="Uniform"){
      median<-qunif(0.5,input$Asc_min,input$Asc_max)
    }
    else if(plotTypeAsc()=="Normal"){
      median<-qtruncnorm(p=0.5,a=0,b=1,mean=input$Asc_mean,sd=input$Asc_sd)
    }
    else if(plotTypeAsc()=="Skewed"){
      AscAlpha<-input$Asc_means*(((input$Asc_means*(1-input$Asc_means))/input$Asc_var)-1)
      AscBeta<-(1-input$Asc_means)*(((input$Asc_means*(1-input$Asc_means))/input$Asc_var)-1)
      AscShape<-(input$Asc_means*input$Asc_means)/input$Asc_var
      AscScale<-input$Asc_var/input$Asc_means
      median<-qgamma(p=0.5*pgamma(1,shape=AscShape,scale=AscScale),shape=AscShape,scale=AscScale)
                     # ,shape1=AscAlpha,shape2=AscBeta)
    }
    paste("Your median value for case ascertainment is:",round(median,digits=2))
  })
  
  observeEvent(input$previousAsc,{
    updateNavbarPage(session=session,"mainpage",selected="Reproduction number")
  })
  
  observeEvent(input$nextAsc,{
    updateNavbarPage(session=session,"mainpage",selected="Contact tracing")
  })
  
  observeEvent(input$previousCT,{
    updateNavbarPage(session=session,"mainpage",selected="Case ascertainment")
  })
  
  observeEvent(input$nextCT,{
    updateNavbarPage(session=session,"mainpage",selected="Vaccination")
  })
  
  observeEvent(input$previousVax,{
    updateNavbarPage(session=session,"mainpage",selected="Contact tracing")
  })
  
  observeEvent(input$nextVax,{
    updateNavbarPage(session=session,"mainpage",selected="Submit")
  })
  
  observeEvent(input$previousSubmit,{
    updateNavbarPage(session=session,"mainpage",selected="Vaccination")
  })
  
  # observeEvent(input$submit,{
  #   updateNavbarPage(session=session,"mainpage",selected="Contact tracing")
  # })

}

# Run the application 
shinyApp(ui = ui, server = server)
