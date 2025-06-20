#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load libraries
# distr has a conflict with shiny; both have a function called "p". Below, I have included distr's function p using "distr::p". 
# To be able to load the library in the normal way, we would need to replace every "p" with "shiny::p"; I think this works. <3
library(distr)
library(xlsx)
library(shiny)
library(tidyverse)
library(bslib)
library(ggplot2)
# library(devtools)
# install_github("olafmersmann/truncnorm")
library(truncnorm)
# library("DT")

#Data <- gs4_create("Ebola_data")

# Define UI for application that draws a histogram
ui <- page_navbar(
  
  theme=bs_theme(version=5,bootswatch="cerulean"),
  
  title="Expert elicitation for Ebola epidemiological parameters",
  id="mainpage",
  
  #overview
  
  bslib::nav_panel(title="Overview",
      
    shiny::p(tags$h3("Rationale")),
    
    shiny::p("Mathematical modelling studies to inform Ebola virus disease (EVD) outbreak response policy are highly reliant on input parameters, which are 
      often derived from epidemiological data. Whilst there is reasonable empirical evidence underpinning some parameters (see Nash et al, 2024), 
      other parameters often cannot be fully observed or are not routinely reported, and therefore do not appear in published literature. However, 
      many of these parameters, related to both transmission and outbreak response, have important implications for policy decisions."),
    
    shiny::p("This survey aims to fill these gaps in the literature using the subjective judgement of experts with experience working in EVD outbreak response.
       We are specifically interested in parameters pertaining to: reproduction number, case ascertainment, contact tracing and vaccination. We would welcome your 
       input on any or all of these parameters; please feel free to skip any parameters about which you don't feel confident making a judgement."),
    
    shiny::p(tags$h3("Explanatory notes")),
    
    shiny::p("The first page of the survey asks for some basic information on your experience with EVD outbreak responses. The remaining pages focus 
      on parameter areas of interest. For each parameter, you can select a distribution and use sliders to vary the shape of your judgement. 
      Visualisations and summary statistics are provided to help inform your intuition. There is also a dropdown box that enables you to specify 
      how confident you are in your judgement and a free text field to provide extra context or sources related to your judgement. 
      Please consider the following when making judgements about your confidence:"),
    
    shiny::p(tags$b("Very:"),"I have firsthand experience working on or analysing data related to this parameter and I can cite sources supporting my intuition, even
      though they may not be publicly available."),
    
    shiny::p(tags$b("Somewhat:"),"I have firsthand experience working on or analysing data related to this parameter but sources supporting my intuition are not available",),
    
    shiny::p(tags$b("Slightly:"),"I don't have firsthand experience working on or analysing data related to this parameter but I can cite sources supporting my intuition, even
      though they may not be publicly available."),
    
    shiny::p(tags$b("Not very:"),"I don't have firsthand experience working on or analysing data related to this parameter. I have some intuition about this value
      based on conversations with colleague, but there are no sources available to support this intuition."),
    
    shiny::p("For more information on this survey, please contact: Anne Cori (anne.cori@imperial.ac.uk), Katharina Hauck (k.hauck@imperial.ac.uk) or 
      Gemma Nedjati-Gilani (g.nedjati-gilani@imperial.ac.uk)"),
    
  
    actionButton("nextOverview","Next",class="btn-primary")
  ),
  
  #experience
  
  nav_panel(title="Your experience",
            shiny::p(tags$h3("Your experience")),
            
            shiny::p("We'd like to start the survey by asking about your experience during EVD outbreak responses:"),
            
            radioButtons(inputId="ExpEpi",label="Do you have experience analysing epidemiological data? E.g. calculating case fatality ratios, hospitalisation rates or reproduction number etc.",choices=c("No","Yes"),width="100%"),
            conditionalPanel(condition="input.ExpEpi=='Yes'",
                             selectInput("ExpEpi_length","If yes, how many years experience do you have?",c("0-2","3-5","6-9","10+"),width="80%",selected=NULL)
                             ),
            radioButtons(inputId="ExpCase",label="Do you have experience with case investigation and ascertainment? E.g. conducting Viral Haemorrhagic Fever (VHF) questionnaires, assessing case alerts etc.",choices=c("No","Yes"),width="100%"),
            conditionalPanel(condition="input.ExpCase=='Yes'",
                             selectInput("ExpCase_length","If yes, how many years experience do you have?",c("0-2","3-5","6-9","10+"),width="80%")
            ),
            radioButtons(inputId="ExpCT",label="Do you have experience with contact tracing? E.g. as a contact tracer or contact tracing supervisor etc.",choices=c("No","Yes"),width="100%"),
            conditionalPanel(condition="input.ExpCT=='Yes'",
                             selectInput("ExpCT_length","If yes, how many years experience do you have?",c("0-2","3-5","6-9","10+"),width="80%")
            ),
            radioButtons(inputId="ExpVacc",label="Do you have experience with reactive vaccination campaigns? E.g. as a vaccinator or vaccination co-ordinator etc.",choices=c("No","Yes"),width="100%"),
            conditionalPanel(condition="input.ExpVacc=='Yes'",
                             selectInput("ExpVacc_length","If yes, how many years experience do you have?",c("0-2","3-5","6-9","10+"),width="80%")
            ),
            selectInput(inputId="ExpSetting",label="What kind of settings do you have most experience in?",choices=c("Rural","Urban","Both","Remote, e.g. at headquarters or with a collaborating centre"),width="100%",selected = NULL),
            
            selectInput(inputId="ExpOutbreaks",label="Which of the following recent outbreaks have you responded to? Please select all that apply",choices=c("West Africa, 2014-2016","Equateur, DRC, 2018", "North Kivu, DRC, 2018-2020", "Guinea, 2022","Other"),multiple=TRUE,width="100%",selected = NULL),
            conditionalPanel(condition="input.ExpOutbreaks.indexOf('Other')>-1",
                             textAreaInput("ExpOutbreaksOther","Please specify here:",width="80%")
            ),
            textInput("ExpWorkplace","Where do you currently work?",width="100%",placeholder="e.g. WHO"),
            
            textInput("ExpDept","What division or department do you currently work in?",width="100%",placeholder="e.g. Immunization, Health Economics, Surveillance Systems"),
            
            layout_column_wrap(1/2,
              actionButton("previousExp","Previous"),
              actionButton("nextExp","Next",class="btn-primary")
              )
            ),
  
  #reproduction number
  
  nav_panel(title="Reproduction number",
            
            shiny::p(tags$h3("Basic reproduction number")),
            
            shiny::p("The basic reproduction number of a pathogen (R0) is the mean number of infections caused by an infectious individual in an otherwise completely susceptible population.
            If you feel able to share your intuition for R0, please use the options in the sidebar to calibrate your judgement. If you don't feel able to provide your intuition for R0, please continue to the next section"),
            
            
            card(
            layout_sidebar(
              
              sidebar=sidebar(title="Reproduction number (R0)",
                              width=300,
              shiny::p("Based on your knowledge and experience of recent Ebola outbreaks:"),

              selectInput("answerR0","Can you provide your intuition about the distribution of R0 over multiple outbreaks?",
                          c("No","Yes")),

              conditionalPanel(
                condition="input.answerR0=='Yes'",
                selectInput("R0_shape","What do you think the shape of the distribution of R0 is?",
                            c("Uniform","Normal","Skewed")),

                conditionalPanel(
                  condition="input.R0_shape=='Uniform'",
                  sliderInput("R0_min","What do you think the minimum value of R0 is?",min=0,max=10,value=0,step=0.1,round=-1)
                ),

                conditionalPanel(
                  condition="input.R0_shape=='Uniform'",
                  sliderInput("R0_max","What do you think the maximum value of R0 is?",min=0,max=10,value=10,step=0.1,round=-1)
                ),

                conditionalPanel(
                  condition="input.R0_shape=='Normal'",
                  sliderInput("R0_mean","What do you think the mean value of R0 is?",min=1,max=5,value=2,step=0.1,round=-1)
                ),

                conditionalPanel(
                  condition="input.R0_shape=='Normal'",
                  sliderInput("R0_sd","What do you think the standard deviation of R0 is?",min=0.1,max=2,value=0.5,step=0.01,round=-2)
                ),
                
                conditionalPanel(
                  condition="input.R0_shape=='Normal'",
                  sliderInput("R0_min_norm","What do you think minimum value of R0 is?",min=0,max=3,value=1,step=0.1,round=-1)
                ),
                
                conditionalPanel(
                  condition="input.R0_shape=='Normal'",
                  sliderInput("R0_max_norm","What do you think maximum value of R0 is?",min=3,max=10,value=10,step=0.1,round=-1)
                ),

                conditionalPanel(
                  condition="input.R0_shape=='Skewed'",
                  sliderInput("R0_means","What do you think the mean value of R0 is?",min=1,max=5,value=1.5,step=0.1,round=-1)
                ),

                conditionalPanel(
                  condition="input.R0_shape=='Skewed'",
                  sliderInput("R0_var","What do you think the variance of R0 is?",min=0.01,max=2,value=0.5,step=0.01,round=-2)
                ),
                
                conditionalPanel(
                  condition="input.R0_shape=='Skewed'",
                  sliderInput("R0_min_skew","What do you think minimum value of R0 is?",min=0,max=3,value=1,step=0.1,round=-1)
                ),
                
                conditionalPanel(
                  condition="input.R0_shape=='Skewed'",
                  sliderInput("R0_max_skew","What do you think maximum value of R0 is?",min=3,max=10,value=10,step=0.1,round=-1)
                ),
              )
            ),

            conditionalPanel(
              condition="input.answerR0=='Yes'",
              plotOutput("plotR0",width="100%",height='500px'),
              textOutput("R0median"),
              textOutput("R0conf"),

              selectInput("conf_R0","How confident are you about the shape of the distribution?",
                            c("Very","Somewhat","Slightly","Not very"),width="50%", selected = "Not very"),

              textAreaInput("source_R0","Please provide any context or sources that have guided your intuition:",width="80%"
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
                   
                   shiny::p(tags$h3("Case ascertainment")),
                   
                   shiny::p("Case ascertainment refers to the proportion of all cases that are identified and added to the case line list (this could be as confirmed, probable or suspect cases). In an ideal outbreak response, case ascertainment would be 1, i.e. all cases would be recorded.
                   In practice, this is not possible due to limited resources or stigma attached to being identified as an EVD case. We are interested in the proportion of cases that are ascertained as it impacts the success of the policies and interventions
                   that we model, such as contact tracing and targeted vaccination.
                     If you feel able to share your intuition regarding case ascertainment, please use the options in the sidebar to calibrate your judgement. If you don't feel able to provide your intuition for case ascertainment, please continue to the next section"),
                   
                   card(
                     layout_sidebar(
                       sidebar=sidebar(title="Case ascertainment",
                                       width=300,
                                       shiny::p("Based on your knowledge and experience of recent Ebola outbreaks:"),
                                       
                                       selectInput("answerAsc","Can you provide your intuition about the distribution of case ascertainment?",
                                                   c("No","Yes")),
                                       
                                       conditionalPanel(
                                         condition="input.answerAsc=='Yes'",
                                         selectInput("Asc_shape","What do you think the shape of the distribution of case ascertainment is?",
                                                     c("Uniform","Normal","Beta")),
                                         
                                         conditionalPanel(
                                           condition="input.Asc_shape=='Uniform'",
                                           sliderInput("Asc_min","What do you think the minimum value of case ascertainment is?",min=0,max=1,value=0,step=0.05,round=-2)
                                         ),
                                         
                                         conditionalPanel(
                                           condition="input.Asc_shape=='Uniform'",
                                           sliderInput("Asc_max","What do you think the maximum value of case ascertainment is?",min=0,max=1,value=1,step=0.05,round=-2)
                                         ),
                                         
                                         conditionalPanel(
                                           condition="input.Asc_shape=='Normal'",
                                           sliderInput("Asc_mean","What do you think the mean value of case ascertainment is?",min=0,max=1,value=0.5,step=0.05,round=-2)
                                         ),
                                         
                                         conditionalPanel(
                                           condition="input.Asc_shape=='Normal'",
                                           sliderInput("Asc_sd","What do you think the standard deviation of case ascertainment is?",min=0.1,max=1,value=0.5,step=0.01,round=-2)
                                         ),
                                         
                                         # conditionalPanel(
                                         #   condition="input.Asc_shape=='Skewed'",
                                         #   sliderInput("Asc_means","What do you think the mean value of case ascertainment is?",min=0.1,max=1,value=0.5,step=0.05,round=-2)
                                         # ),
                                         # 
                                         # conditionalPanel(
                                         #   condition="input.Asc_shape=='Skewed'",
                                         #   sliderInput("Asc_var","What do you think the variance of case ascertainment is?",min=0.01,max=0.25,value=0.1,step=0.001,round=-3)
                                         # ),
                                         
                                         conditionalPanel(
                                           condition="input.Asc_shape=='Beta'",
                                           sliderInput("Asc_means","What do you think the mean value of case ascertainment is?",min=0.01,max=0.99,value=0.5,step=0.05,round=-2)
                                         ),
                                         
                                         conditionalPanel(
                                           condition="input.Asc_shape=='Beta'",
                                           sliderInput("Asc_betasd","What do you think the standard deviation of case ascertainment is?",min=0.01,max=round(sqrt(1/12),2),value=sqrt(1/12),step=0.001,round=-3)
                                         )
                                       )
                       ),
                       
                       conditionalPanel(
                         condition="input.answerAsc=='Yes'",
                         plotOutput("plotAsc",width="100%",height="100%"),
                         textOutput("Ascmedian"),
                         textOutput("Ascconf"),
                         
                         selectInput("conf_Asc","How confident are you about the shape of the distribution?",
                                     c("Very","Somewhat","Slightly","Not very"),width="80%", selected = "Not very"),
                         
                         selectInput("is_corr_Asc_R0","Do you think there is any correlation between case ascertainment and reproduction number? e.g. if the reproduction number is higher, case ascertainment is also higher.",c("Not sure", "Yes","No"),selected=NULL,width="80%"),
                         
                         conditionalPanel(condition="input.is_corr_Asc_R0=='Yes'",
                                          selectInput("corr_Asc_R0","Do you think the correlation is positive (i.e. when reproduction number is high, case ascertainment is high and when reproduction number is low, case ascertainment is low) or negative (i.e. when reproduction number is low, case ascertainment is high and vice versa)?",c("Positive","Negative"),selected=NULL,width="80%")
                           
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
                   
                   shiny::p(tags$h3("Contact tracing")),
                   
                   shiny::p("Once a case has been ascertained, contact tracing teams compile lists of close contacts, who are at high risk of infection, to undergo a 21 day follow-up.
                     If they develop symptoms during this time, they are quickly tested, isolated and treated to prevent further chains of transmission. To understand whether contact tracing 
                     is likely to be effective, we are interested in understanding: a) what proportion of contacts are traced, and b) what proportion of contacts complete follow-up. 
                     If you feel able to share your intuition regarding contact tracing, please use the options in the sidebar to calibrate your judgement. If you don't feel able to provide your intuition for contact tracing, please continue to the next section"),
                   
                   card(
                     accordion(id="CT",
                       accordion_panel("Proportion of contacts traced",
                                       value="CTprop",
                                       layout_sidebar(
                                         sidebar=sidebar(title=tags$h4("Proportion traced"),
                                                        width=300,
                                                         shiny::p("Based on your knowledge and experience of recent Ebola outbreaks:"),
                                                         
                                                         selectInput("answerCTprop","Can you provide your intuition about the distribution of the proportion of contacts who are traced?",
                                                                     c("No","Yes")),
                                                         
                                                         conditionalPanel(
                                                           condition="input.answerCTprop=='Yes'",
                                                           selectInput("CTprop_shape","What do you think the shape of the distribution of the proportion of contacts who are traced is?",
                                                                       c("Uniform","Normal","Skewed")),
                                                           
                                                           conditionalPanel(
                                                             condition="input.CTprop_shape=='Uniform'",
                                                             sliderInput("CTprop_min","What do you think the minimum value of the proportion of contacts who are traced is?",min=0,max=1,value=0,step=0.05,round=-2)
                                                           ),
                                                           
                                                           conditionalPanel(
                                                             condition="input.CTprop_shape=='Uniform'",
                                                             sliderInput("CTprop_max","What do you think the maximum value of the proportion of contacts who are traced is?",min=0,max=1,value=1,step=0.05,round=-2)
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
                                                       c("Very","Somewhat","Slightly","Not very"),width="80%", selected = "Not very"),
                                           
                                           selectInput("is_corr_CTprop_R0","Do you think there is any correlation between the proportion of contacts traced and the reproduction number? e.g. if the reproduction number is higher, the proportion of contacts traced is also higher.",c("Not sure", "Yes","No"),selected=NULL,width="80%"),
                                           
                                           conditionalPanel(condition="input.is_corr_CTprop_R0=='Yes'",
                                                            selectInput("corr_CTprop_R0","Do you think the correlation is positive (i.e. when reproduction number is high, the proportion of contacts traced is high and when reproduction number is low, the proportion of contacts traced is low) or negative (i.e. when reproduction number is low, the proportion of contacts traced is high and vice versa)?",c("Positive","Negative"),selected=NULL,width="80%")
                                                            
                                           ),
                                           
                                           selectInput("is_corr_CTprop_Asc","Do you think there is any correlation between the proportion of contacts traced and case ascertainment? e.g. if case ascertainment is higher, the proportion of contacts traced is also higher.",c("Not sure", "Yes","No"),selected=NULL,width="80%"),
                                           
                                           conditionalPanel(condition="input.is_corr_CTprop_Asc=='Yes'",
                                                            selectInput("corr_CTprop_Asc","Do you think the correlation is positive (i.e. when case ascertainment is high, the proportion of contacts traced is high and when case ascertainment is low, the proportion of contacts traced is low) or negative (i.e. when case ascertainment is low, the proportion of contacts traced is high and vice versa)?",c("Positive","Negative"),selected=NULL,width="80%")
                                                            
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
                                                         shiny::p(""),
                                                         shiny::p("Based on your knowledge and experience of recent Ebola outbreaks:"),
                                                         
                                                         selectInput("answerCTfoll","Can you provide your intuition about the distribution of the proportion of contacts who complete follow-up?",
                                                                     c("No","Yes")),
                                                         
                                                         conditionalPanel(
                                                           condition="input.answerCTfoll=='Yes'",
                                                           selectInput("CTfoll_shape","What do you think the shape of the distribution of the proportion of contacts who complete follow-up is?",
                                                                       c("Uniform","Normal","Skewed")),
                                                           
                                                           conditionalPanel(
                                                             condition="input.CTfoll_shape=='Uniform'",
                                                             sliderInput("CTfoll_min","What do you think the minimum value of the proportion of contacts who complete follow-up is?",min=0,max=1,value=0,step=0.05,round=-2)
                                                           ),
                                                           
                                                           conditionalPanel(
                                                             condition="input.CTfoll_shape=='Uniform'",
                                                             sliderInput("CTfoll_max","What do you think the maximum value of the proportion of contacts who complete follow-up is?",min=0,max=1,value=1,step=0.05,round=-2)
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
                                                       c("Very","Somewhat","Slightly","Not very"),width="80%", selected = "Not very"),
                                           
                                           selectInput("is_corr_CTfoll_R0","Do you think there is any correlation between the proportion of contacts who complete follow-up and reproduction number? e.g. if the reproduction number is higher, the proportion of contacts traced is also higher.",c("Not sure", "Yes","No"),selected=NULL,width="80%"),
                                           
                                           conditionalPanel(condition="input.is_corr_CTfoll_R0=='Yes'",
                                                            selectInput("corr_CTfoll_R0","Do you think the correlation is positive (i.e. when reproduction number is high, the proportion of contacts who complete follow-up is high and when reproduction number is low, the proportion of contacts who complete follow-up is low) or negative (i.e. when reproduction number is low, the proportion of contacts who complete follow-up is high and vice versa)?",c("Positive","Negative"),selected=NULL,width="80%")
                                                            
                                           ),
                                           
                                           selectInput("is_corr_CTfoll_Asc","Do you think there is any correlation between the proportion of contacts who complete follow-up and case ascertainment? e.g. if case ascertainment is higher, the proportion of contacts who complete follow-up is also higher.",c("Not sure", "Yes","No"),selected=NULL,width="80%"),
                                           
                                           conditionalPanel(condition="input.is_corr_CTfoll_Asc=='Yes'",
                                                            selectInput("corr_CTfoll_Asc","Do you think the correlation is positive (i.e. when case ascertainment is high, the proportion of contacts who complete follow-up is high and when case ascertainment is low, the proportion of contacts who complete follow-up is low) or negative (i.e. when case ascertainment is low, the proportion of contacts who complete follow-up is high and vice versa)?",c("Positive","Negative"),selected=NULL,width="80%")
                                                            
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
                   
                   shiny::p(tags$h3("Vaccination")),
                   
                   shiny::p("Reactive vaccination campaigns carried out during EVD outbreaks target both healthcare workers (HCWs) and frontline workers (FLWs), and at-risk contacts of cases. The
                     latter is typically triggered by the ascertainment of a case and can be carried out using ring vaccination or geographically targeted vaccination. We are interested in 
                     vaccine uptake for these different strategies, as well as the time taken to initiate vaccination following ascertainment of a case."),
                   
                   card(
                     accordion(id="vacc",
                               accordion_panel("HCW/FLW vaccination",
                                               value="Vacc_HCW",
                                               layout_sidebar(
                                                 sidebar=sidebar(title="HCW/FLW vaccination",
                                                                 width=300,
                                                                 shiny::p(""),
                                                                 shiny::p("Based on your knowledge and experience of recent Ebola outbreaks:"),
                                                                 
                                                                 selectInput("answerVaccHCW","Can you provide your intuition about the distribution of the proportion of HCWs/FLWs who accept vaccination?",
                                                                             c("No","Yes")),
                                                                 
                                                                 conditionalPanel(
                                                                   condition="input.answerVaccHCW=='Yes'",
                                                                   selectInput("VaccHCW_shape","What do you think the shape of the distribution of the proportion of HCWs/FLWs who accept vaccination is?",
                                                                               c("Uniform","Normal","Skewed")),
                                                                   
                                                                   conditionalPanel(
                                                                     condition="input.VaccHCW_shape=='Uniform'",
                                                                     sliderInput("VaccHCW_min","What do you think the minimum value of the proportion of HCWs/FLWs who accept vaccination is?",min=0,max=1,value=0,step=0.05,round=-2)
                                                                   ),
                                                                   
                                                                   conditionalPanel(
                                                                     condition="input.VaccHCW_shape=='Uniform'",
                                                                     sliderInput("VaccHCW_max","What do you think the maximum value of the proportion of HCWs/FLWs who accept vaccination is?",min=0,max=1,value=1,step=0.05,round=-2)
                                                                   ),
                                                                   
                                                                   conditionalPanel(
                                                                     condition="input.VaccHCW_shape=='Normal'",
                                                                     sliderInput("VaccHCW_mean","What do you think the mean value of the proportion of HCWs/FLWs who accept vaccination is?",min=0,max=1,value=0.5,step=0.05,round=-2)
                                                                   ),
                                                                   
                                                                   conditionalPanel(
                                                                     condition="input.VaccHCW_shape=='Normal'",
                                                                     sliderInput("VaccHCW_sd","What do you think the standard deviation of the proportion of HCWs/FLWs who accept vaccination is?",min=0.1,max=1,value=0.5,step=0.01,round=-2)
                                                                   ),
                                                                   
                                                                   conditionalPanel(
                                                                     condition="input.VaccHCW_shape=='Skewed'",
                                                                     sliderInput("VaccHCW_means","What do you think the mean value of the proportion of HCWs/FLWs who accept vaccination is?",min=0.1,max=1,value=0.5,step=0.05,round=-2)
                                                                   ),
                                                                   
                                                                   conditionalPanel(
                                                                     condition="input.VaccHCW_shape=='Skewed'",
                                                                     sliderInput("VaccHCW_var","What do you think the variance of the proportion of HCWs/FLWs who accept vaccination is?",min=0.01,max=0.25,value=0.1,step=0.001,round=-3)
                                                                   )
                                                                 )
                                                                 
                                                 ),
                                                 
                                                 conditionalPanel(
                                                   condition="input.answerVaccHCW=='Yes'",
                                                   plotOutput("plotVaccHCW",width="100%",height='500px'),
                                                   textOutput("VaccHCWmedian"),
                                                   textOutput("VaccHCWconf"),
                                                   
                                                   selectInput("conf_VaccHCW","How confident are you about the shape of the distribution?",
                                                               c("Very","Somewhat","Slightly","Not very"),width="80%", selected = "Not very"),
                                                   
                                                   selectInput("is_corr_VaccHCW_R0","Do you think there is any correlation between the proportion of HCWs/FLWs who accept vaccination and reproduction number? e.g. if the reproduction number is higher, the proportion who accept is also higher.",c("Not sure", "Yes","No"),selected=NULL,width="80%"),
                                                   
                                                   conditionalPanel(condition="input.is_corr_VaccHCW_R0=='Yes'",
                                                                    selectInput("corr_VaccHCW_R0","Do you think the correlation is positive (i.e. when reproduction number is high, the proportion who accept vaccination is high and when reproduction number is low, the proportion who accept is low) or negative (i.e. when reproduction number is low, the proportion of contacts who accept is high and vice versa)?",c("Positive","Negative"),selected=NULL,width="80%")
                                                                    
                                                   ),
                                                   
                                                   selectInput("is_corr_VaccHCW_Asc","Do you think there is any correlation between the proportion of contacts who complete follow-up and case ascertainment? e.g. if case ascertainment is higher, the proportion of contacts who complete follow-up is also higher.",c("Not sure", "Yes","No"),selected=NULL,width="80%"),
                                                   
                                                   conditionalPanel(condition="input.is_corr_CTfoll_Asc=='Yes'",
                                                                    selectInput("corr_CTfoll_Asc","Do you think the correlation is positive (i.e. when case ascertainment is high, the proportion of contacts who complete follow-up is high and when case ascertainment is low, the proportion of contacts who complete follow-up is low) or negative (i.e. when case ascertainment is low, the proportion of contacts who complete follow-up is high and vice versa)?",c("Positive","Negative"),selected=NULL,width="80%")
                                                                    
                                                   ),
                                                   
                                                   textAreaInput("source_CTfoll","Please provide any context or sources that have guided your intuition:",width="80%"
                                                   )
                                                 ),
                                                 
                                                 
                                                 layout_column_wrap(1/2,
                                                                    actionButton("previousVax_HCW","Previous"),
                                                                    actionButton("nextVax_HCW","Next",class="btn-primary")
                                                 )
                                               )
                                 
                               ),
                               accordion_panel("Ring vaccination",
                                               value="Vacc_Ring",
                                               layout_sidebar(
                                                 sidebar=sidebar(title="Ring vaccination",
                                                                 width=300,
                                                                 shiny::p(""),
                                                                 shiny::p("Based on your knowledge and experience of recent Ebola outbreaks:"),
                                                                 
                                                 ),
                                                 
                                                 layout_column_wrap(1/2,
                                                                    actionButton("previousVax_Ring","Previous"),
                                                                    actionButton("nextVax_Ring","Next",class="btn-primary")
                                                 )
                                               )
                                               
                               ),
                               accordion_panel("HCW/FLW vaccination",
                                               value="Vacc_Geo",
                                               layout_sidebar(
                                                 sidebar=sidebar(title="Geographically targeted vaccination",
                                                                 width=300,
                                                                 shiny::p(""),
                                                                 shiny::p("Based on your knowledge and experience of recent Ebola outbreaks:"),
                                                                 
                                                 ),
                                                 
                                                 layout_column_wrap(1/2,
                                                                    actionButton("previousVax_Geo","Previous"),
                                                                    actionButton("nextVax_Geo","Next",class="btn-primary")
                                                 )
                                               )
                                               
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
  text_size = 20
  breaks10 = 0:xmax
  breaksunit = seq(0,1,length=11)
  get_beta_parameters <- function(betamean, betasd){
    # parameters alpha and beta
    # mean = alpha /(alpha + beta)
    # variance = alpha*beta / ((alpha + beta)^2 * (alpha + beta + 1))
    # alpha = (mean^2 *(1 - mean) - mean*variance)/variance
    # beta = alpha *(1 - mean)/mean
    # always check for -ve parameters, as we get an error otherwise
    var <- betasd^2
    if(betamean * (1-betamean) < var)
      var <- (betamean * (1-betamean))*.99
    # print(var)
    Alpha <- (betamean^2 * (1-betamean) - betamean*var) / var
    Beta <- Alpha * (1 - betamean) / betamean
    # print(c(Alpha,Beta))
    c(Alpha, Beta)
  }
  v <- reactiveValues(asc_dist = NULL, 
                      AscSlider=NULL)
  
  observeEvent(input$nextOverview,{
    updateNavbarPage(session=session,"mainpage",selected="Your experience")
  })
  
  observeEvent(input$previousExp,{
    updateNavbarPage(session=session,"mainpage",selected="Overview")
  })
  
  observeEvent(input$nextExp,{
    updateNavbarPage(session=session,"mainpage",selected="Reproduction number")
  })
  
  ## reproduction number R0 ########################################################
  
  plotTypeR0 <- reactive({input$R0_shape
  })
  
  observeEvent(input$R0_min,{
    updateSliderInput(session,"R0_max",min=input$R0_min+0.1)
  })
  
  observeEvent(input$R0_min_norm,{
    updateSliderInput(session,"R0_max_norm",min=input$R0_min_norm+0.1)
  })
  
  observeEvent(input$R0_min_skew,{
    updateSliderInput(session,"R0_max_skew",min=input$R0_min_skew+0.1)
  })

  output$plotR0 <- renderPlot({
    if(plotTypeR0()=="Uniform"){
      dat<-data.frame(xpos=seq(xmin,xmax,by=0.01))
      dat$ypos<-dunif(dat$xpos,min=input$R0_min,max=input$R0_max,log=F)
      dat$qt  <- cut(punif(dat$xpos,min=input$R0_min,max=input$R0_max,log=F),breaks=qrt,labels=F)
    }
    else if(plotTypeR0()=="Normal"){
      dat<-data.frame(xpos=seq(xmin,xmax,by=0.01))
      dat$ypos<-dtruncnorm(x=dat$xpos,a=input$R0_min_norm,b=input$R0_max_norm,mean=input$R0_mean,sd=input$R0_sd)
      dat$qt  <- cut(ptruncnorm(dat$xpos,a=input$R0_min_norm,b=input$R0_max_norm,mean=input$R0_mean,sd=input$R0_sd),breaks=qrt,labels=F)
    }
    else if(plotTypeR0()=="Skewed"){
      #then make these into gamma distribution parameters
      R0scale<-input$R0_var/input$R0_means
      R0sh<-(input$R0_means*input$R0_means)/input$R0_var
      dat<-data.frame(xpos=seq(xmin,xmax,by=0.01))
      dat$ypos<-dgamma(dat$xpos,shape=R0sh,scale=R0scale,log=F)
      dat$qt  <- cut(pgamma(dat$xpos,shape=R0sh,scale=R0scale,log=F),breaks=qrt,labels=F)
    }
    
    ggplot(dat,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="R0",y="pdf",color="Percentile",title="Probability density of R0")+
      theme_gray(base_size = text_size)+theme(legend.position ="none") + 
      scale_x_continuous(breaks=breaks10)
    
  }
  )
  
  output$R0conf<-renderText({
    if(plotTypeR0()=="Uniform"){
      lower50<-qunif(0.25,input$R0_min,input$R0_max)
      upper50<-qunif(0.75,input$R0_min,input$R0_max)
      lower95<-qunif(0.025,input$R0_min,input$R0_max)
      upper95<-qunif(0.975,input$R0_min,input$R0_max)
    }
    else if(plotTypeR0()=="Normal"){
      lower50<-qtruncnorm(p=0.25,a=input$R0_min_norm,b=input$R0_max_norm,mean=input$R0_mean,sd=input$R0_sd)
      upper50<-qtruncnorm(p=0.75,a=input$R0_min_norm,b=input$R0_max_norm,mean=input$R0_mean,sd=input$R0_sd)
      lower95<-qtruncnorm(p=0.025,a=input$R0_min_norm,b=input$R0_max_norm,mean=input$R0_mean,sd=input$R0_sd)
      upper95<-qtruncnorm(p=0.975,a=input$R0_min_norm,b=input$R0_max_norm,mean=input$R0_mean,sd=input$R0_sd)
    }
    else if(plotTypeR0()=="Skewed"){
      lower50<-qgamma(0.25,scale=input$R0_var/input$R0_means,shape=(input$R0_means*input$R0_means)/input$R0_var)
      upper50<-qgamma(0.75,scale=input$R0_var/input$R0_means,shape=(input$R0_means*input$R0_means)/input$R0_var)
      lower95<-qgamma(p=0.025,scale=input$R0_var/input$R0_means,shape=(input$R0_means*input$R0_means)/input$R0_var)
      upper95<-qgamma(p=0.975,scale=input$R0_var/input$R0_means,shape=(input$R0_means*input$R0_means)/input$R0_var)
    }
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$R0median<-renderText({
    if(plotTypeR0()=="Uniform"){
      median<-qunif(0.5,input$R0_min,input$R0_max)
    }
    else if(plotTypeR0()=="Normal"){
      median<-qtruncnorm(p=0.5,a=input$R0_min_norm,b=input$R0_max_norm,mean=input$R0_mean,sd=input$R0_sd)
    }
    else if(plotTypeR0()=="Skewed"){
      median<-qgamma(p=0.5,scale=input$R0_var/input$R0_means,shape=((input$R0_means*input$R0_means)/input$R0_var))
    }
    paste("Your median value for R0 is:",round(median,digits=2))
  })
  
  observeEvent(input$previousR0,{
    updateNavbarPage(session=session,"mainpage",selected="Your experience")
  })
  
  observeEvent(input$nextR0,{
    updateNavbarPage(session=session,"mainpage",selected="Case ascertainment")
  })
  
  ## case ascertainment ###################################################################################
  
  observeEvent(input$Asc_min,{
    updateSliderInput(session,"Asc_max",min=input$Asc_min+0.1)
  })
  
  plotTypeAsc <- reactive({input$Asc_shape
  })
  
  observeEvent(input$Asc_means, {
    # If the beta mean changes, compute the new implied standard deviation
    Asc_var <- input$Asc_betasd^2
    # by definition we require
    # mean*(1 - mean) > variance
    if(input$Asc_means * (1-input$Asc_means) < Asc_var){
      Asc_var <- input$Asc_means * (1-input$Asc_means)
      max_sd = round(sqrt(Asc_var), 3)
      updateSliderInput(session, "Asc_betasd", value = max_sd)
    }
    beta_pars <- get_beta_parameters(input$Asc_means, round(sqrt(Asc_var), 3))
    AscAlpha <- beta_pars[1]
    AscBeta <- beta_pars[2]
    # normalise alpha and beta so that both are at least 1
    if(min(AscAlpha,AscBeta)<1){
      minab <- min(AscAlpha,AscBeta)
      AscAlpha <- AscAlpha/minab
      AscBeta <- AscBeta/minab
      implied_sd = round(sqrt(AscAlpha*AscBeta / ((AscAlpha + AscBeta)^2 * (AscAlpha + AscBeta + 1))), 3)
      updateSliderInput(session, "Asc_betasd", value = implied_sd)
    }
  })
  
  output$plotAsc <- renderPlot({
    datAsc <- data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
    if(plotTypeAsc()=="Uniform"){
      asc_dist = distr::Unif(Min=input$Asc_min,Max=input$Asc_max)
    }
    else if(plotTypeAsc()=="Normal"){
      asc_dist = distr::Truncate(distr::Norm(mean=input$Asc_mean,sd=input$Asc_sd),lower=0,upper=1)
    }
    else if(plotTypeAsc()=="Skewed"){
      #then make these into gamma or beta distribution parameters
      AscShape<-(input$Asc_means*input$Asc_means)/input$Asc_var
      AscScale<-input$Asc_var/input$Asc_means
      AscAlpha<-input$Asc_means*(((input$Asc_means*(1-input$Asc_means))/input$Asc_var)-1)
      AscBeta<-(1-input$Asc_means)*(((input$Asc_means*(1-input$Asc_means))/input$Asc_var)-1)
      datAsc<-data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
      asc_dist = distr::Gammad(shape1 = AscAlpha, shape2 = AscBeta)
    }
    else if(plotTypeAsc()=="Beta"){
      #then make these into beta distribution parameters
      beta_pars <- get_beta_parameters(input$Asc_means, input$Asc_betasd)
      datAsc <- subset(datAsc,xpos*(1-xpos)!=0)
      # print(c(14, input$Asc_means, input$Asc_betasd, beta_pars))
      asc_dist = distr::Beta(shape1 = beta_pars[1], shape2 = beta_pars[2])
      # if(sum(beta_pars<.99)>0) return(NULL) # cut if parameters have not been updated
    }
    v$asc_dist = asc_dist
    datAsc$ypos <- distr::d(asc_dist)(datAsc$xpos)
    datAsc$qt  <- cut(distr::p(asc_dist)(datAsc$xpos),breaks=qrt,labels=F) #cut(pbeta(datAsc$xpos,alpha=AscShape,beta=AscScale,log=F),breaks=qrt,labels=F)
    
    ggplot(datAsc,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="Proportion of cases ascertained",y="pdf",color="Percentile",title="Probability density of case ascertainment proportion")+
      theme_gray(base_size = text_size)+theme(legend.position ="none") + 
      scale_x_continuous(breaks=breaksunit)
  }
  )
  
  output$Ascconf<-renderText({
    asc_dist = v$asc_dist
    lower50 <- distr::q(asc_dist)(0.25)
    upper50 <- distr::q(asc_dist)(0.75) # qbeta(p=0.75*pbeta(1,shape=AscShape,scale=AscScale),shape=AscShape,scale=AscScale)
    lower95 <- distr::q(asc_dist)(0.025) # qbeta(p=0.025*pbeta(1,shape=AscShape,scale=AscScale),shape=AscShape,scale=AscScale)
    upper95 <- distr::q(asc_dist)(0.975) # qbeta(p=0.975*pbeta(1,shape=AscShape,scale=AscScale),shape=AscShape,scale=AscScale)
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$Ascmedian<-renderText({
    asc_dist = v$asc_dist
    median <- distr::q(asc_dist)(0.5)
    paste("Your median value for case ascertainment is:",round(median,digits=2))
  })
  
  observeEvent(input$previousAsc,{
    updateNavbarPage(session=session,"mainpage",selected="Reproduction number")
  })
  
  observeEvent(input$nextAsc,{
    updateNavbarPage(session=session,"mainpage",selected="Contact tracing")
  })
  
  ## proportion of contacts traced ##############################################################
  
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
      labs(x="Proportion of contacts traced",y="pdf",color="Percentile",title="Probability density of proportion of contacts traced")+
      theme_gray(base_size = text_size)+theme(legend.position ="none") + 
      scale_x_continuous(breaks=breaksunit)
    
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
    else if(plotTypeCTprop()=="Skewed"){
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
  
  ## proportion of contacts followed up #############################################################
  
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
      labs(x="Proportion of contacts followed up",y="pdf",color="Percentile",title="Probability density of proportion of contacts followed up")+
      theme_gray(base_size = text_size)+theme(legend.position ="none") + 
      scale_x_continuous(breaks=breaksunit)
    
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
    paste("Your median value for the proportion contacts followed up is:",round(median,digits=2))
  })
  
  
  observeEvent(input$previousCTfollow,{
    accordion_panel_close(session=session,id="CT",values="CTfollow")
    accordion_panel_open(session=session,id="CT",values="CTprop")
  })
  
  observeEvent(input$nextCTfollow,{
    updateNavbarPage(session=session,"mainpage",selected="Vaccination")
  })
  
  ## vaccination #############################################################
  
  observeEvent(input$previousVax,{
    updateNavbarPage(session=session,"mainpage",selected="Contact tracing")
  })
  
  observeEvent(input$nextVax,{
    updateNavbarPage(session=session,"mainpage",selected="Submit")
  })
  
  ## submission #############################################################
  
  observeEvent(input$previousSubmit,{
    updateNavbarPage(session=session,"mainpage",selected="Vaccination")
  })
  
  observeEvent(input$submit,{
    # tabulate answers
    # these are all the parameter prefixes for which values could be reported
    categories <- c('R0','Asc','CTprop','CTfoll')
    answers <- c()
    for(ct in categories){
      if(!is.null(input[[paste0(ct,'_shape')]])){
        thisdist = input[[paste0(ct,'_shape')]]
        answers = rbind(answers, c(paste0(ct,' distribution'), thisdist))
        if(thisdist=='Uniform') {
          minlab = paste0(ct,'_min')
          maxlab = paste0(ct,'_max')
          answers = rbind(answers, rbind(c(minlab, input[[minlab]]),c(maxlab, input[[maxlab]])))
        }
        if(thisdist=='Normal') {
          meanlab = paste0(ct,'_mean')
          sclab = paste0(ct,'_sd')
          answers = rbind(answers, rbind(c(meanlab, input[[meanlab]]),c(sclab, input[[sclab]])))
          minlab = paste0(ct,'_min_norm')
          maxlab = paste0(ct,'_max_norm')
          if(!is.null(input[[minlab]]))
            answers = rbind(answers, rbind(c(minlab, input[[minlab]]),c(maxlab, input[[maxlab]])))
        }
        if(thisdist=='Beta') {
          meanlab = paste0(ct,'_means')
          sclab = paste0(ct,'_betasd')
          answers = rbind(answers, rbind(c(meanlab, input[[meanlab]]),c(sclab, input[[sclab]])))
        }
        if(thisdist=='Skewed') {
          meanlab = paste0(ct,'_means')
          varlab = paste0(ct,'_var')
          answers = rbind(answers, rbind(c(meanlab, input[[meanlab]]),c(varlab, input[[varlab]])))
          minlab = paste0(ct,'_min_skew')
          maxlab = paste0(ct,'_max_skew')
          if(!is.null(input[[minlab]]))
            answers = rbind(answers, rbind(c(minlab, input[[minlab]]),c(maxlab, input[[maxlab]])))
        }
        conflab = paste0('conf_',ct)
        sourcelab = paste0('source_',ct)
        if(!is.null(input[[conflab]])) answers = rbind(answers, c(conflab, input[[conflab]]))
        if(!is.null(input[[sourcelab]])) answers = rbind(answers, c(sourcelab, input[[sourcelab]]))
      }
    }
    
    # tabulate correlations
    cors = unlist(sapply(2:length(categories),function(x) sapply(1:(x-1), function(y) paste0('corr_',categories[x],'_',categories[y]))))
    for(sgn in cors){
      cc <- paste0('is_',sgn)
      if(!is.null(input[[cc]])) {
        answers = rbind(answers, c(cc, input[[cc]]))
        answers = rbind(answers, c(sgn, input[[sgn]]))
      }
    }
    
    # tabulate user info
    expinfo = c()
    expvars = c('ExpCT', 'ExpCT_length', 'ExpCase', 'ExpCase_length', 'ExpEpi', 'ExpEpi_length', 'ExpOutbreaks', 'ExpOutbreaksOther', 'ExpSetting', 'ExpVacc', 'ExpVacc_length', 'ExpWorkplace','ExpDept')
    expvars = as.data.frame(sapply(expvars,function(x)ifelse(is.null(input[[x]]),'',paste0(input[[x]],collapse=', '))))
    answers <- as.data.frame(answers)
    
    # give column names; print; save
    colnames(expvars) <- c('Value')
    colnames(answers) <- c('Variable','Value')
    print(expvars)
    print(answers)
    filename = paste0(format(now(), "%Y%m%d_%H%M%S_"), "data_set.xlsx")
    xlsx::write.xlsx(expvars,file = filename,sheetName='User data', append=F,row.names = T)
    xlsx::write.xlsx(answers,file = filename,sheetName='Parameter data', append=T,row.names = F)
    
    ## maybe create a "thanks" page or restart? i think you need to close and reopen to reset everything for the next user.
    updateNavbarPage(session=session,"mainpage",selected="Overview")
    
    # these are some values stored in input not written out
    # answerAsc, answerCTfoll, answerCTprop, answerR0, 
    # mainpage, nextAsc, nextCTfollow, nextCTprop, nextExp, nextOverview, nextR0, nextVax, 
    # previousAsc, previousCTfollow, previousCTprop, previousExp, previousR0, previousSubmit, previousVax, 
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
