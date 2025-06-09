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
library(tidyverse)
library(bslib)
library(ggplot2)
# library(devtools)
# install_github("olafmersmann/truncnorm")
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
  
  theme=bs_theme(version=5,bootswatch="cerulean"),
  
  title="Expert elicitation for Ebola epidemiological parameters",
  id="mainpage",
  
  #overview
  
  bslib::nav_panel(title="Overview",
      
    p(tags$h3("Rationale")),
    
    p("Mathematical modelling studies to inform Ebola virus disease (EVD) outbreak response policy are highly reliant on input parameters, which are 
      often derived from epidemiological data. Whilst there is reasonable empirical evidence underpinning some parameters (see Nash et al, 2024), 
      other parameters often cannot be fully observed or are not routinely reported, and therefore do not appear in published literature. However, 
      many of these parameters, related to both transmission and outbreak response, have important implications for policy decisions."),
    
    p("This survey aims to fill these gaps in the literature using the subjective judgement of experts with experience working in EVD outbreak response.
       We are specifically interested in parameters pertaining to: reproduction nunber, case ascertainment, contact tracing and vaccination. We would welcome your 
       input on any or all of these parameters; please feel free to skip any parameters for which you don't feel confident making a judgement about."),
    
    p(tags$h3("Explanatory notes")),
    
    p("The first page of the survey asks for some basic information on your experience with EVD outbreak responses. The remaining pages focus 
      on parameter areas of interest. For each parameter, you can select a distribution and use sliders to vary the shape of your judgement. 
      Visualisations and summary statistics are provided to help inform your intuition. There is also a dropdown box that enables you to specify 
      how confident you are in your judgement and a free text field to provide extra context or sources related to your judgement. 
      Please consider the following when making judgements about your confidence:"),
    
    p(tags$b("Very:"),"I have firsthand experience working on or analysing data related to this parameter and I can cite sources supporting my intuition, even
      though they may not be publicly available."),
    
    p(tags$b("Somewhat:"),"I have firsthand experience working on or analysing data related to this parameter but sources supporting my intuition are not available",),
    
    p(tags$b("Slightly:"),"I don't have firsthand experience working on or analysing data related to this parameter but I can cite sources supporting my intuition, even
      though they may not be publicly available."),
    
    p(tags$b("Not very:"),"I don't have firsthand experience working on or analysing data related to this parameter. I have some intuition about this value
      based on conversations with colleague, but there are no sources available to support this intuition."),
    
    p("For more information on this survey, please contact: Anne Cori (anne.cori@imperial.ac.uk), Katharina Hauck (k.hauck@imperial.ac.uk) or 
      Gemma Nedjati-Gilani (g.nedjati-gilani@imperial.ac.uk)"),
    
  
    actionButton("nextOverview","Next",class="btn-primary")
  ),
  
  #experience
  
  nav_panel(title="Your experience",
            p(tags$h3("Your experience")),
            
            p("We'd like to start the survey by asking about your experience during EVD outbreak responses:"),
            
            radioButtons(inputId="ExpEpi",label="Do you have experience analysing epidemiological data? E.g. calculating case fatality ratios, hospitalisation rates or reproduction number etc.",choices=c("No","Yes"),width="100%"),
            conditionalPanel(condition="input.ExpEpi=='Yes'",
                             numericInput("ExpEpi_length","If yes, how many years experience do you have?",min=0,max=20,value=0,step=0.5,width="80%")
                             ),
            radioButtons(inputId="ExpCase",label="Do you have experience with case investigation and ascertainment? E.g. conducting Viral Haemorrhagic Fever (VHF) questionnaires, assessing case alerts etc.",choices=c("No","Yes"),width="100%"),
            conditionalPanel(condition="input.ExpCase=='Yes'",
                             numericInput("ExpCase_length","If yes, how many years experience do you have?",min=0,max=20,value=0,step=0.5,width="80%")
            ),
            radioButtons(inputId="ExpCT",label="Do you have experience with contact tracing? E.g. as a contact tracer or contact tracing supervisor etc.",choices=c("No","Yes"),width="100%"),
            conditionalPanel(condition="input.ExpCT=='Yes'",
                             numericInput("ExpCT_length","If yes, how many years experience do you have?",min=0,max=20,value=0,step=0.5,width="80%")
            ),
            radioButtons(inputId="ExpVacc",label="Do you have experience with reactive vaccination campaigns? E.g. as a vaccinator or vaccination co-ordinator etc.",choices=c("No","Yes"),width="100%"),
            conditionalPanel(condition="input.ExpVacc=='Yes'",
                             numericInput("ExpVacc_length","If yes, how many years experience do you have?",min=0,max=20,value=0,step=0.5,width="80%")
            ),
            
            layout_column_wrap(1/2,
              actionButton("previousExp","Previous"),
              actionButton("nextExp","Next",class="btn-primary")
              )
            ),
  
  #reproduction number
  
  nav_panel(title="Reproduction number",
            
            p(tags$h3("Basic reproduction number")),
            
            p("The basic reproduction number of a pathogen (R0) is the number of infections caused by an infectious individual in an otherwise completely susceptible population.
            If you feel able to share your intuition for R0, please use the options in the sidebar to calibrate your judgement. If you don't feel able to provide your intuition for R0, please continue to the next section"),
            
            
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

              selectInput("conf_R0","How confident are you about the shape of the distribution?",
                            c("Very","Somewhat","Slightly","Not very"),width="50%"),

              textAreaInput("source_R","Please provide any context or sources that have guided your intuition:",width="80%"
              )
            ),
            
            layout_column_wrap(1/2,
              actionButton("previousR0","Previous"),
              actionButton("nextR0","Next",class="btn-primary")
            )
          )
        )
            
  ),
  
  bslib::nav_panel(title="Case ascertainment",
                   
                   p(tags$h3("Case ascertainment")),
                   
                   p("Case ascertainment refers to the proportion of all cases that are identified and added to the case line list (this could be as confirmed, probable or suscept cases.) In an ideal outbreak response, case ascertainment would be 1, i.e. all cases would be recorded.
                   In practice, there are many reasons why this is not possible, e.g. due to limited resources or stigma attached to being identified as an EVD case. We are interested in understanding the proportion of cases that are ascertained as it directly impacts the success of the policies and interventions
                   that we model, such as contact tracing and targeted vaccination.
                     If you feel able to share your intuition regarding case ascertainment, please use the options in the sidebar to calibrate your judgement. If you don't feel able to provide your intuition for case ascertainment, please continue to the next section"),
                   
                   card(
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
                                           sliderInput("Asc_min","What do you think the minimum value of case ascertainment is?",min=0,max=1,value=0.5,step=0.05,round=-2)
                                         ),
                                         
                                         conditionalPanel(
                                           condition="input.Asc_shape=='Uniform'",
                                           sliderInput("Asc_max","What do you think the maximum value of case ascertainment is?",min=0.5,max=1,value=0.7,step=0.05,round=-2)
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
                                           sliderInput("Asc_means","What do you think the mean value of case ascertainment is?",min=0.1,max=1,value=0.5,step=0.05,round=-2)
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
                         
                         selectInput("conf_Asc","How confident are you about the shape of the distribution?",
                                     c("Very","Somewhat","Slightly","Not very"),width="80%"),
                         
                         selectInput("is_corr_Asc_R0","Do you think there is any correlation between case ascertainment and reproduction number? i.e if the reproduction number is higher, case ascertainment is also higher.",c("Not sure", "Yes","No"),selected=NULL,width="80%"),
                         
                         conditionalPanel(condition="input.is_corr_Asc_R0=='Yes'",
                                          selectInput("corr_Asc_R0","Do you think the correlation is positive (i.e. when reproduction number is high, case ascertainment is high and vice versa) or negative (i.e. when reproduction number is low, case ascertainment is high and vice versa)?",c("Positive","Negative"),selected=NULL,width="80%")
                           
                         ),
                         
                         textAreaInput("source_Asc","Please provide any context or sources that have guided your intuition:",width="80%"
                         )
                       ),
                       layout_column_wrap(1/2,
                                          actionButton("previousAsc","Previous"),
                                          actionButton("nextAsc","Next",class="btn-primary")
                       )
                     )
                   )
                   
  ),
  
  bslib::nav_panel(title="Contact tracing",
                   
                   p(tags$h3("Contact tracing")),
                   
                   p("Once a case has been ascertained, contact tracing teams compile lists of close contacts, who are at high risk of infection, to undergo a 21 day follow-up period.
                     If they develop symptoms during this time, they are quickly tested, isolate and treated to prevent further chains of transmission. To understand whether contact tracing 
                     is likely to be effecttive, we are interested in understanding: a) what proportion of contacts are traced, and b) what proportion of contacts complete follow-up. 
                     If you feel able to share your intuition regarding contact tracing, please use the options in the sidebar to calibrate your judgement. If you don't feel able to provide your intuition for contact tracing, please continue to the next section"),
                   
                   card(
                     accordion(id="CT",
                       accordion_panel("Proportion of contacts traced",
                                       value="CTprop",
                                       layout_sidebar(
                                         sidebar=sidebar(title=tags$h4("Proportion traced"),
                                                        width=300,
                                                         p("Based on your knowledge and experience of recent Ebola outbreaks:"),
                                                         
                                                         selectInput("answerCTprop","Can you provide your intuition about the distribution of the proportion of contacts who are traced?",
                                                                     c("No","Yes")),
                                                         
                                                         conditionalPanel(
                                                           condition="input.answerCTprop=='Yes'",
                                                           selectInput("CTprop_shape","What do you think the shape of the distribution of the proportion of contacts who are traced?",
                                                                       c("Uniform","Normal","Skewed")),
                                                           
                                                           conditionalPanel(
                                                             condition="input.CTprop_shape=='Uniform'",
                                                             sliderInput("CTprop_min","What do you think the minimum value of the proportion of contacts who are traced is?",min=0,max=1,value=0.5,step=0.05,round=-2)
                                                           ),
                                                           
                                                           conditionalPanel(
                                                             condition="input.CTprop_shape=='Uniform'",
                                                             sliderInput("CTprop_max","What do you think the maximum value of the proportion of contacts who are traced is?",min=0.5,max=1,value=0.7,step=0.05,round=-2)
                                                           ),
                                                           
                                                           conditionalPanel(
                                                             condition="input.CTprop_shape=='Normal'",
                                                             sliderInput("CTprop_mean","What do you think the mean value of the proportion of contacts who are traced is?",min=0,max=1,value=0.5,step=0.05,round=-2)
                                                           ),
                                                           
                                                           conditionalPanel(
                                                             condition="input.CTprop_shape=='Normal'",
                                                             sliderInput("CTprop_sd","What do you think the standard deviation of the proportion of contacts who are traced is?",min=0.1,max=1,value=0.5,step=0.01,round=-2)
                                                           ),
                                                           
                                                           conditionalPanel(
                                                             condition="input.CTprop_shape=='Skewed'",
                                                             sliderInput("CTprop_means","What do you think the mean value of the proportion of contacts who are traced is?",min=0.1,max=1,value=0.5,step=0.05,round=-2)
                                                           ),
                                                           
                                                           conditionalPanel(
                                                             condition="input.CTprop_shape=='Skewed'",
                                                             sliderInput("CTprop_var","What do you think the variance of the proportion of contacts who are traced is?",min=0.01,max=0.25,value=0.1,step=0.001,round=-3)
                                                           )
                                                         )
                                                         
                                         ),
                                         
                                         conditionalPanel(
                                           condition="input.answerCTprop=='Yes'",
                                           plotOutput("plotCTprop",width="100%",height='500px'),
                                           textOutput("CTpropmedian"),
                                           textOutput("CTpropconf"),
                                           
                                           selectInput("conf_CTprop","How confident are you about the shape of the distribution?",
                                                       c("Very","Somewhat","Slightly","Not very"),width="80%"),
                                           
                                           selectInput("is_corr_CTprop_R0","Do you think there is any correlation between the proportion of contacts traced and reproduction number? i.e if the reproduction number is higher, the proportion of contacts traced is also higher.",c("Not sure", "Yes","No"),selected=NULL,width="80%"),
                                           
                                           conditionalPanel(condition="input.is_corr_CTprop_R0=='Yes'",
                                                            selectInput("corr_CTprop_R0","Do you think the correlation is positive (i.e. when reproduction number is high, the proportion of contacts traced is high and vice versa) or negative (i.e. when reproduction number is low, the proportion of contacts traced is high and vice versa)?",c("Positive","Negative"),selected=NULL,width="80%")
                                                            
                                           ),
                                           
                                           selectInput("is_corr_CTprop_Asc","Do you think there is any correlation between the proportion of contacts traced and case ascertainment? i.e if case ascertainment is higher, the proportion of contacts traced is also higher.",c("Not sure", "Yes","No"),selected=NULL,width="80%"),
                                           
                                           conditionalPanel(condition="input.is_corr_CTprop_Asc=='Yes'",
                                                            selectInput("corr_CTprop_Asc","Do you think the correlation is positive (i.e. when case ascertainment is high, the proportion of contacts traced is high and vice versa) or negative (i.e. when case ascertainment is low, the proportion of contacts traced is high and vice versa)?",c("Positive","Negative"),selected=NULL,width="80%")
                                                            
                                           ),
                                           
                                           textAreaInput("source_CTprop","Please provide any context or sources that have guided your intuition:",width="80%"
                                           )
                                         ),
                                         
                                         layout_column_wrap(1/2,
                                                            actionButton("previousCTprop","Previous"),
                                                            actionButton("nextCTprop","Next",class="btn-primary")
                                         )
                                       )
                       ),
                       accordion_panel("Proportion of contacts who complete follow-up",
                                       value="CTfollow",
                                       layout_sidebar(
                                         sidebar=sidebar(title=tags$h4("Proportion followed-up"),
                                                         width=300,
                                                         p(""),
                                                         p("Based on your knowledge and experience of recent Ebola outbreaks:"),
                                                         
                                                         selectInput("answerCTfoll","Can you provide your intuition about the distribution of the proportion of contacts who complete follow-up?",
                                                                     c("No","Yes")),
                                                         
                                                         conditionalPanel(
                                                           condition="input.answerCTfoll=='Yes'",
                                                           selectInput("CTfoll_shape","What do you think the shape of the distribution of the proportion of contacts who complete follow-up?",
                                                                       c("Uniform","Normal","Skewed")),
                                                           
                                                           conditionalPanel(
                                                             condition="input.CTfoll_shape=='Uniform'",
                                                             sliderInput("CTfoll_min","What do you think the minimum value of the proportion of contacts who complete follow-up is?",min=0,max=1,value=0.5,step=0.05,round=-2)
                                                           ),
                                                           
                                                           conditionalPanel(
                                                             condition="input.CTfoll_shape=='Uniform'",
                                                             sliderInput("CTfoll_max","What do you think the maximum value of the proportion of contacts who complete follow-up is?",min=0.5,max=1,value=0.7,step=0.05,round=-2)
                                                           ),
                                                           
                                                           conditionalPanel(
                                                             condition="input.CTfoll_shape=='Normal'",
                                                             sliderInput("CTfoll_mean","What do you think the mean value of the proportion of contacts who complete follow-up is?",min=0,max=1,value=0.5,step=0.05,round=-2)
                                                           ),
                                                           
                                                           conditionalPanel(
                                                             condition="input.CTfoll_shape=='Normal'",
                                                             sliderInput("CTfoll_sd","What do you think the standard deviation of the proportion of contacts who complete follow-up is?",min=0.1,max=1,value=0.5,step=0.01,round=-2)
                                                           ),
                                                           
                                                           conditionalPanel(
                                                             condition="input.CTfoll_shape=='Skewed'",
                                                             sliderInput("CTfoll_means","What do you think the mean value of the proportion of contacts who complete follow-up is?",min=0.1,max=1,value=0.5,step=0.05,round=-2)
                                                           ),
                                                           
                                                           conditionalPanel(
                                                             condition="input.CTfoll_shape=='Skewed'",
                                                             sliderInput("CTfoll_var","What do you think the variance of the proportion of contacts who complete follow-up is?",min=0.01,max=0.25,value=0.1,step=0.001,round=-3)
                                                           )
                                                         )
                                                         
                                         ),
                                         
                                         conditionalPanel(
                                           condition="input.answerCTfoll=='Yes'",
                                           plotOutput("plotCTfoll",width="100%",height='500px'),
                                           textOutput("CTfollmedian"),
                                           textOutput("CTfollconf"),
                                           
                                           selectInput("conf_CTfoll","How confident are you about the shape of the distribution?",
                                                       c("Very","Somewhat","Slightly","Not very"),width="80%"),
                                           
                                           selectInput("is_corr_CTfoll_R0","Do you think there is any correlation between the proportion of contacts who complete follow-up? and reproduction number? i.e if the reproduction number is higher, the proportion of contacts traced is also higher.",c("Not sure", "Yes","No"),selected=NULL,width="80%"),
                                           
                                           conditionalPanel(condition="input.is_corr_CTfoll_R0=='Yes'",
                                                            selectInput("corr_CTfoll_R0","Do you think the correlation is positive (i.e. when reproduction number is high, the proportion of contacts who complete follow-up is high and vice versa) or negative (i.e. when reproduction number is low, the proportion of contacts who complete follow-up is high and vice versa)?",c("Positive","Negative"),selected=NULL,width="80%")
                                                            
                                           ),
                                           
                                           selectInput("is_corr_CTfoll_Asc","Do you think there is any correlation between the proportion of contacts who complete follow-up and case ascertainment? i.e if case ascertainment is higher, the proportion of contacts who complete follow-up is also higher.",c("Not sure", "Yes","No"),selected=NULL,width="80%"),
                                           
                                           conditionalPanel(condition="input.is_corr_CTfoll_Asc=='Yes'",
                                                            selectInput("corr_CTfoll_Asc","Do you think the correlation is positive (i.e. when case ascertainment is high, the proportion of contacts who complete follow-up is high and vice versa) or negative (i.e. when case ascertainment is low, the proportion of contacts who complete follow-up is high and vice versa)?",c("Positive","Negative"),selected=NULL,width="80%")
                                                            
                                           ),
                                           
                                           textAreaInput("source_CTfoll","Please provide any context or sources that have guided your intuition:",width="80%"
                                           )
                                         ),
                                         
                                         layout_column_wrap(1/2,
                                                            actionButton("previousCTfollow","Previous"),
                                                            actionButton("nextCTfollow","Next",class="btn-primary")
                                         )
                                         
                                       )
                       )
                     )
                     
                   ),
  ),
  
  bslib::nav_panel(title="Vaccination",
                   
                   p(tags$h3("Vaccination")),
                   
                   p("Information about vaccination"),
                   
                   card(
                     layout_sidebar(
                       sidebar=sidebar(title="Vaccination",
                                       width=400,
                                       p(""),
                                       
                       ),
                       
                       layout_column_wrap(1/2,
                                          actionButton("previousVax","Previous"),
                                          actionButton("nextVax","Next",class="btn-primary")
                       )
                     )
                   )
                   
  ),
  
  bslib::nav_panel(title="Submit",
                   layout_column_wrap(1/2,
                                     actionButton("previousSubmit","Previous"),
                                     actionButton("submit","Submit",class="btn-success")
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
  
  #reproduction number r0
  
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
  
  #case ascertainment
  
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
  
  #proportion of contacts traced
  
  observeEvent(input$CTprop_min,{
    updateSliderInput(session,"CTprop_max",min=input$CTprop_min+0.1)
  })
  
  plotTypeCTprop <- reactive({input$CTprop_shape
  })
  
  output$plotCTprop <- renderPlot({
    if(plotTypeCTprop()=="Uniform"){
      datCTprop<-data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
      datCTprop$ypos<-dunif(datCTprop$xpos,min=input$CTprop_min,max=input$CTprop_max,log=F)
      datCTprop$qt  <- cut(punif(datCTprop$xpos,min=input$CTprop_min,max=input$CTprop_max,log=F),breaks=qrt,labels=F)
    }
    else if(plotTypeCTprop()=="Normal"){
      datCTprop<-data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
      datCTprop$ypos<-dtruncnorm(x=datCTprop$xpos,a=0,b=1,mean=input$CTprop_mean,sd=input$CTprop_sd)
      datCTprop$qt  <- cut(ptruncnorm(datCTprop$xpos,a=0,b=1,mean=input$CTprop_mean,sd=input$CTprop_sd),breaks=qrt,labels=F)
    }
    else if(plotTypeCTprop()=="Skewed"){
      #then make these into gamma or beta distribution parameters
      CTpropShape<-(input$CTprop_means*input$CTprop_means)/input$CTprop_var
      CTpropScale<-input$CTprop_var/input$CTprop_means
      datCTprop<-data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
      datCTprop$ypos<-dgamma(datCTprop$xpos,shape=CTpropShape,scale=CTpropScale,log=F)/pgamma(1,shape=CTpropShape,scale=CTpropScale)
      datCTprop$qt  <- cut(pgamma(datCTprop$xpos,shape=CTpropShape,scale=CTpropScale,log=F)/pgamma(1,shape=CTpropShape,scale=CTpropScale),breaks=qrt,labels=F)
    }
    
    ggplot(datCTprop,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="Proportion of cases ascertained",y="pdf",color="Percentile",title="Probability density of case ascertainment proportion")+
      theme_gray()+theme(legend.position ="none")
    
  }
  )
  
  output$CTpropconf<-renderText({
    if(plotTypeCTprop()=="Uniform"){
      lower50<-qunif(0.25,input$CTprop_min,input$CTprop_max)
      upper50<-qunif(0.75,input$CTprop_min,input$CTprop_max)
      lower95<-qunif(0.025,input$CTprop_min,input$CTprop_max)
      upper95<-qunif(0.975,input$CTprop_min,input$CTprop_max)
    }
    else if(plotTypeCTprop()=="Normal"){
      lower50<-qtruncnorm(p=0.25,a=0,b=1,mean=input$CTprop_mean,sd=input$CTprop_sd)
      upper50<-qtruncnorm(p=0.75,a=0,b=1,mean=input$CTprop_mean,sd=input$CTprop_sd)
      lower95<-qtruncnorm(p=0.025,a=0,b=1,mean=input$CTprop_mean,sd=input$CTprop_sd)
      upper95<-qtruncnorm(p=0.975,a=0,b=1,mean=input$CTprop_mean,sd=input$CTprop_sd)
    }
    else if(plotTypeCTProp()=="Skewed"){
      CTpropShape<-(input$CTprop_means*input$CTprop_means)/input$CTprop_var
      CTpropScale<-input$CTprop_var/input$CTprop_means
      lower50<-qgamma(p=0.25*pgamma(1,shape=CTpropShape,scale=CTpropScale),shape=CTpropShape,scale=CTpropScale)
      upper50<-qgamma(p=0.75*pgamma(1,shape=CTpropShape,scale=CTpropScale),shape=CTpropShape,scale=CTpropScale)
      lower95<-qgamma(p=0.025*pgamma(1,shape=CTpropShape,scale=CTpropScale),shape=CTpropShape,scale=CTpropScale)
      upper95<-qgamma(p=0.975*pgamma(1,shape=CTpropShape,scale=CTpropScale),shape=CTpropShape,scale=CTpropScale)
    }
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$CTpropmedian<-renderText({
    if(plotTypeCTprop()=="Uniform"){
      median<-qunif(0.5,input$CTprop_min,input$CTprop_max)
    }
    else if(plotTypeCTprop()=="Normal"){
      median<-qtruncnorm(p=0.5,a=0,b=1,mean=input$CTprop_mean,sd=input$CTprop_sd)
    }
    else if(plotTypeCTprop()=="Skewed"){
      CTpropShape<-(input$CTprop_means*input$CTprop_means)/input$CTprop_var
      CTpropScale<-input$CTprop_var/input$CTprop_means
      median<-qgamma(p=0.5*pgamma(1,shape=CTpropShape,scale=CTpropScale),shape=CTpropShape,scale=CTpropScale)
    }
    paste("Your median value for the proportion of contacts traced is:",round(median,digits=2))
  })
  
  observeEvent(input$previousCTprop,{
    updateNavbarPage(session=session,"mainpage",selected="Case ascertainment")
  })
  
  observeEvent(input$nextCTprop,{
    accordion_panel_close(session=session,id="CT",values = "CTprop")
    accordion_panel_open(session=session,id="CT",values="CTfollow")
  })
  
  #proportion of contacts followed up
  
  observeEvent(input$CTfoll_min,{
    updateSliderInput(session,"CTfoll_max",min=input$CTfoll_min+0.1)
  })
  
  plotTypeCTfoll <- reactive({input$CTfoll_shape
  })
  
  output$plotCTfoll <- renderPlot({
    if(plotTypeCTfoll()=="Uniform"){
      datCTfoll<-data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
      datCTfoll$ypos<-dunif(datCTfoll$xpos,min=input$CTfoll_min,max=input$CTfoll_max,log=F)
      datCTfoll$qt  <- cut(punif(datCTfoll$xpos,min=input$CTfoll_min,max=input$CTfoll_max,log=F),breaks=qrt,labels=F)
    }
    else if(plotTypeCTfoll()=="Normal"){
      datCTfoll<-data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
      datCTfoll$ypos<-dtruncnorm(x=datCTfoll$xpos,a=0,b=1,mean=input$CTfoll_mean,sd=input$CTfoll_sd)
      datCTfoll$qt  <- cut(ptruncnorm(datCTfoll$xpos,a=0,b=1,mean=input$CTfoll_mean,sd=input$CTfoll_sd),breaks=qrt,labels=F)
    }
    else if(plotTypeCTfoll()=="Skewed"){
      #then make these into gamma or beta distribution parameters
      CTfollShape<-(input$CTfoll_means*input$CTfoll_means)/input$CTfoll_var
      CTfollScale<-input$CTfoll_var/input$CTfoll_means
      datCTfoll<-data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
      datCTfoll$ypos<-dgamma(datCTfoll$xpos,shape=CTfollShape,scale=CTfollScale,log=F)/pgamma(1,shape=CTfollShape,scale=CTfollScale)
      datCTfoll$qt  <- cut(pgamma(datCTfoll$xpos,shape=CTfollShape,scale=CTfollScale,log=F)/pgamma(1,shape=CTfollShape,scale=CTfollScale),breaks=qrt,labels=F)
    }
    
    ggplot(datCTfoll,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="Proportion of cases ascertained",y="pdf",color="Percentile",title="Probability density of case ascertainment proportion")+
      theme_gray()+theme(legend.position ="none")
    
  }
  )
  
  output$CTfollconf<-renderText({
    if(plotTypeCTfoll()=="Uniform"){
      lower50<-qunif(0.25,input$CTfoll_min,input$CTfoll_max)
      upper50<-qunif(0.75,input$CTfoll_min,input$CTfoll_max)
      lower95<-qunif(0.025,input$CTfoll_min,input$CTfoll_max)
      upper95<-qunif(0.975,input$CTfoll_min,input$CTfoll_max)
    }
    else if(plotTypeCTfoll()=="Normal"){
      lower50<-qtruncnorm(p=0.25,a=0,b=1,mean=input$CTfoll_mean,sd=input$CTfoll_sd)
      upper50<-qtruncnorm(p=0.75,a=0,b=1,mean=input$CTfoll_mean,sd=input$CTfoll_sd)
      lower95<-qtruncnorm(p=0.025,a=0,b=1,mean=input$CTfoll_mean,sd=input$CTfoll_sd)
      upper95<-qtruncnorm(p=0.975,a=0,b=1,mean=input$CTfoll_mean,sd=input$CTfoll_sd)
    }
    else if(plotTypeCTfoll()=="Skewed"){
      CTfollShape<-(input$CTfoll_means*input$CTfoll_means)/input$CTfoll_var
      CTfollScale<-input$CTfoll_var/input$CTfoll_means
      lower50<-qgamma(p=0.25*pgamma(1,shape=CTfollShape,scale=CTfollScale),shape=CTfollShape,scale=CTfollScale)
      upper50<-qgamma(p=0.75*pgamma(1,shape=CTfollShape,scale=CTfollScale),shape=CTfollShape,scale=CTfollScale)
      lower95<-qgamma(p=0.025*pgamma(1,shape=CTfollShape,scale=CTfollScale),shape=CTfollShape,scale=CTfollScale)
      upper95<-qgamma(p=0.975*pgamma(1,shape=CTfollShape,scale=CTfollScale),shape=CTfollShape,scale=CTfollScale)
    }
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$CTfollmedian<-renderText({
    if(plotTypeCTfoll()=="Uniform"){
      median<-qunif(0.5,input$CTfoll_min,input$CTfoll_max)
    }
    else if(plotTypeCTfoll()=="Normal"){
      median<-qtruncnorm(p=0.5,a=0,b=1,mean=input$CTfoll_mean,sd=input$CTfoll_sd)
    }
    else if(plotTypeCTfoll()=="Skewed"){
      CTfollShape<-(input$CTfoll_means*input$CTfoll_means)/input$CTfoll_var
      CTfollScale<-input$CTfoll_var/input$CTfoll_means
      median<-qgamma(p=0.5*pgamma(1,shape=CTfollShape,scale=CTfollScale),shape=CTfollShape,scale=CTfollScale)
    }
    paste("Your median value for the proportion of contacts traced is:",round(median,digits=2))
  })
  
  
  observeEvent(input$previousCTfollow,{
    accordion_panel_close(session=session,id="CT",values="CTfollow")
    accordion_panel_open(session=session,id="CT",values="CTprop")
  })
  
  observeEvent(input$nextCTfollow,{
    updateNavbarPage(session=session,"mainpage",selected="Vaccination")
  })
  
  #vaccination
  
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
