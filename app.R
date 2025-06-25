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


# Overview ----------------------------------------------------------------

  
  bslib::nav_panel(title="Overview",
      
    shiny::p(tags$h3("Rationale")),
    
    shiny::p("Mathematical modelling studies to inform Ebola virus disease (EVD) outbreak response policy are highly reliant on input parameters, which are 
      often derived from epidemiological data. Whilst there is reasonable empirical evidence underpinning some disease natural history parameters (see Nash et al, 2024), 
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
    
    shiny::p(tags$b("Somewhat:"),"I have firsthand experience working on or analysing data related to this parameter but sources supporting my intuition are not available"),
    
    shiny::p(tags$b("Slightly:"),"I don't have firsthand experience working on or analysing data related to this parameter but I can cite sources supporting my intuition, even
      though they may not be publicly available."),
    
    shiny::p(tags$b("Not very:"),"I don't have firsthand experience working on or analysing data related to this parameter. I have some intuition about this value
      based on conversations with colleague, but there are no sources available to support this intuition."),
    
    actionButton("nextOverview","Next",class="btn-primary")
  ),
  

# Experience --------------------------------------------------------------

  
  nav_panel(title="1",
            
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
  

# Reproduction number -----------------------------------------------------

  
  nav_panel(title="2",
            
            shiny::p(tags$h3("Basic reproduction number")),
            
            shiny::p("The basic reproduction number of a pathogen (R0) is the mean number of infections caused by an infectious individual in an otherwise completely susceptible population.
            If you feel able to share your intuition for R0, please use the options in the sidebar to calibrate your judgement. If you don't feel able to provide your intuition for R0 or
                     would prefer to provide intuition for the early growth of EVD epidemics in terms of the doubling time, please continue to the next section"),
            
            
            # card(
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
                  sliderInput("R0_min_skew","What do you think minimum value of R0 is?",min=0,max=3,value=0,step=0.1,round=-1)
                ),
                
                conditionalPanel(
                  condition="input.R0_shape=='Skewed'",
                  sliderInput("R0_max_skew","What do you think maximum value of R0 is?",min=0,max=10,value=10,step=0.1,round=-1)
                )
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
        # )
            
  ),
  

# Doubling time -----------------------------------------------------------

  
  nav_panel(title="3",
            
            shiny::p(tags$h3("Doubling time")),
            
            shiny::p("The doubling time is the time it takes for incidence to double and it is an alternative way to characterise early epidemic growth.
                     f you feel able to share your intuition for R0, please use the options in the sidebar to calibrate your judgement. If you don't feel able to provide your intuition for R0 or
                     would prefer to provide intuition for the early growth of EVD epidemics in terms of the doubling time, please continue to the next section"),
            
            # card(
              layout_sidebar(
                
                sidebar=sidebar(title="Doubling time",
                                width=300,
                                shiny::p("Based on your knowledge and experience of recent Ebola outbreaks:"),
                                
                                selectInput("answerDT","Can you provide your intuition about the distribution of doubling time of EVD cases in the early, exponential growth phase over multiple outbreaks?",
                                            c("No","Yes")),
                                
                                conditionalPanel(
                                  condition="input.answerDT=='Yes'",
                                  selectInput("DT_shape","What do you think the shape of the doubling time distribution is?",
                                              c("Uniform","Normal","Skewed")),
                                  
                                  conditionalPanel(
                                    condition="input.DT_shape=='Uniform'",
                                    sliderInput("DT_min","What do you think the minimum plausible doubling time is is?",min=10,max=30,value=10,step=1,round=0)
                                  ),
                                  
                                  conditionalPanel(
                                    condition="input.DT_shape=='Uniform'",
                                    sliderInput("DT_max","What do you think the maximum plausible doubling time is?",min=10,max=40,value=40,step=1,round=0)
                                  ),
                                  
                                  conditionalPanel(
                                    condition="input.DT_shape=='Normal'",
                                    sliderInput("DT_mean","What do you think the mean doubling time is?",min=10,max=40,value=20,step=1,round=-0)
                                  ),
                                  
                                  conditionalPanel(
                                    condition="input.DT_shape=='Normal'",
                                    sliderInput("DT_sd","What do you think the standard deviation the double time is?",min=1,max=20,value=5,step=0.5,round=-1)
                                  ),
                                  
                                  conditionalPanel(
                                    condition="input.DT_shape=='Normal'",
                                    sliderInput("DT_min_norm","What do you think minimum doubling time is?",min=10,max=30,value=10,step=1,round=0)
                                  ),
                                  
                                  conditionalPanel(
                                    condition="input.DT_shape=='Normal'",
                                    sliderInput("DT_max_norm","What do you think maximum plausible doubling time is?",min=10,max=40,value=40,step=1,round=0)
                                  ),
                                  
                                  conditionalPanel(
                                    condition="input.DT_shape=='Skewed'",
                                    sliderInput("DT_means","What do you think the mean doubling time is?",min=10,max=40,value=20,step=1,round=0)
                                  ),
                                  
                                  conditionalPanel(
                                    condition="input.DT_shape=='Skewed'",
                                    sliderInput("DT_var","What do you think the variance of the doubling time is?",min=1,max=400,value=100,step=0.5,round=-1)
                                  ),
                                  
                                  conditionalPanel(
                                    condition="input.DT_shape=='Skewed'",
                                    sliderInput("DT_min_skew","What do you think minimum plausible doubling time is?",min=10,max=30,value=10,step=1,round=0)
                                  ),
                                  
                                  conditionalPanel(
                                    condition="input.DT_shape=='Skewed'",
                                    sliderInput("DT_max_skew","What do you think maximum plausible doubling time is?",min=10,max=40,value=40,step=1,round=0)
                                  )
                                )
                ),
                
                conditionalPanel(
                  condition="input.answerDT=='Yes'",
                  plotOutput("plotDT",width="100%",height='500px'),
                  textOutput("DTmedian"),
                  textOutput("DTconf"),
                  
                  selectInput("conf_DT","How confident are you about the shape of the distribution?",
                              c("Very","Somewhat","Slightly","Not very"),width="50%", selected = "Not very"),
                  
                  textAreaInput("source_DT","Please provide any context or sources that have guided your intuition:",width="80%"
                  )
                ),
                
                layout_column_wrap(1/2,
                                   actionButton("previousDT","Previous"),
                                   actionButton("nextDT","Next",class="btn-primary")
                )
              )
            # )
            
  ),


# Case ascertainment ------------------------------------------------------

  
  bslib::nav_panel(title="4",
                   
                   shiny::p(tags$h3("Case ascertainment")),
                   
                   shiny::p("Case ascertainment refers to the proportion of all cases that are identified and added to the case line list (this could be as confirmed, probable or suspect cases). In an ideal outbreak response, case ascertainment would be 1, i.e. all cases would be recorded.
                   In practice, this is not possible due to limited resources or stigma attached to being identified as an EVD case. We are interested in the proportion of cases that are ascertained as it impacts the success of the policies and interventions
                   that we model, such as contact tracing and targeted vaccination.
                     If you feel able to share your intuition regarding case ascertainment, please use the options in the sidebar to calibrate your judgement. If you don't feel able to provide your intuition for case ascertainment, please continue to the next section"),
                   
                   # card(
                     accordion(id="CaseAsc",
                       accordion_panel(title="Case ascertainment", value="CaseAsc",
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
                                             
                                             conditionalPanel(
                                               condition="input.Asc_shape=='Normal'",
                                               sliderInput("Asc_min_norm","What do you think the minimum value of case ascertainment is?",min=0,max=1,value=0,step=0.05,round=-2)
                                             ),
                                             
                                             conditionalPanel(
                                               condition="input.Asc_shape=='Normal'",
                                               sliderInput("Asc_max_norm","What do you think the maximum value of case ascertainment is?",min=0,max=1,value=1,step=0.05,round=-2)
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
                                             ),
                                             
                                             conditionalPanel(
                                               condition="input.Asc_shape=='Beta'",
                                               sliderInput("Asc_min_beta","What do you think the minimum value of case ascertainment is?",min=0,max=1,value=0,step=0.05,round=-2)
                                             ),
                                             
                                             conditionalPanel(
                                               condition="input.Asc_shape=='Beta'",
                                               sliderInput("Asc_max_beta","What do you think the maximum value of case ascertainment is?",min=0,max=1,value=1,step=0.05,round=-2)
                                             )
                                           )
                           ),
                           
                           conditionalPanel(
                             condition="input.answerAsc=='Yes'",
                             plotOutput("plotAsc",width="100%"),
                             textOutput("Ascmedian"),
                             textOutput("Ascconf"),
                             
                             selectInput("conf_Asc","How confident are you about the shape of the distribution?",
                                         c("Very","Somewhat","Slightly","Not very"),width="80%", selected = "Not very"),
                             
                             selectInput("is_corr_Asc_R0","Do you think there is any correlation between case ascertainment and reproduction number? e.g. if the reproduction number is higher, case ascertainment is also higher.",c("Not sure", "Yes","No"),selected="Not sure",width="80%"),
                             
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
                     )
                     
                   # )
                   
  ),


# Contact tracing ---------------------------------------------------------


  
  bslib::nav_panel(
    title = "5",
    
    shiny::p(tags$h3("Contact tracing")),
    
    shiny::p(
      "Once a case has been ascertained, contact tracing teams compile lists of close contacts, who are at high risk of infection, to undergo a 21 day follow-up.
                     If they develop symptoms during this time, they are quickly tested, isolated and treated to prevent further chains of transmission. To understand whether contact tracing
                     is likely to be effective, we are interested in understanding: a) what proportion of contacts are traced, and b) what proportion of contacts complete follow-up.
                     If you feel able to share your intuition regarding contact tracing, please use the options in the sidebar to calibrate your judgement. If you don't feel able to provide your intuition for contact tracing, please continue to the next section"
    ),
    
    
    accordion(
       id = "CT",
       accordion_panel(
         "Proportion of contacts traced",
         value = "CTprop",
         layout_sidebar(
           sidebar = sidebar(
             title = tags$h4("Proportion traced"),
             width = 300,
             shiny::p("Based on your knowledge and experience of recent Ebola outbreaks:"),

            selectInput(
              "answerCTprop",
              "Can you provide your intuition about the distribution of the proportion of contacts who are traced?",
              c("No", "Yes")
            ),

            conditionalPanel(
              condition="input.answerCTprop=='Yes'",
              selectInput("CTprop_shape","What do you think the shape of the distribution of the proportion of contacts who are traced is?",
                          c("Uniform","Normal","Beta")),
              
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
                condition="input.CTprop_shape=='Normal'",
                sliderInput("CTprop_min_norm","What do you think the minimum value of the proportion of contacts who are traced is?",min=0,max=1,value=0,step=0.05,round=-2)
              ),
              
              conditionalPanel(
                condition="input.CTprop_shape=='Normal'",
                sliderInput("CTprop_max_norm","What do you think the maximum value of the proportion of contacts who are traced is?",min=0,max=1,value=1,step=0.05,round=-2)
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
                condition="input.CTprop_shape=='Beta'",
                sliderInput("CTprop_means","What do you think the mean value of the proportion of contacts who are traced is?",min=0.01,max=0.99,value=0.5,step=0.05,round=-2)
              ),
              
              conditionalPanel(
                condition="input.CTprop_shape=='Beta'",
                sliderInput("CTprop_betasd","What do you think the standard deviation of the proportion of contacts who are traced is?",min=0.01,max=round(sqrt(1/12),2),value=sqrt(1/12),step=0.001,round=-3)
              ),
              
              conditionalPanel(
                condition="input.CTprop_shape=='Beta'",
                sliderInput("CTprop_min_beta","What do you think the minimum value of the proportion of contacts who are traced is?",min=0,max=1,value=0,step=0.05,round=-2)
              ),
              
              conditionalPanel(
                condition="input.CTprop_shape=='Beta'",
                sliderInput("CTprop_max_beta","What do you think the maximum value of the proportion of contacts who are traced is?",min=0,max=1,value=1,step=0.05,round=-2)
              )
            )
           ),



          conditionalPanel(
            condition = "input.answerCTprop=='Yes'",
            plotOutput("plotCTprop", width =
                         "100%", height = '500px'),
            textOutput("CTpropmedian"),
            textOutput("CTpropconf"),

            selectInput(
              "conf_CTprop",
              "How confident are you about the shape of the distribution?",
              c("Very", "Somewhat", "Slightly", "Not very"),
              width = "80%",
              selected = "Not very"
            ),

            selectInput(
              "is_corr_CTprop_R0",
              "Do you think there is any correlation between the proportion of contacts traced and the reproduction number? e.g. if the reproduction number is higher, the proportion of contacts traced is also higher.",
              c("Not sure", "Yes", "No"),
              selected = "Not sure",
              width = "80%"
            ),

            conditionalPanel(
              condition = "input.is_corr_CTprop_R0=='Yes'",
              selectInput(
                "corr_CTprop_R0",
                "Do you think the correlation is positive (i.e. when reproduction number is high, the proportion of contacts traced is high and when reproduction number is low, the proportion of contacts traced is low) or negative (i.e. when reproduction number is low, the proportion of contacts traced is high and vice versa)?",
                c("Positive", "Negative"),
                selected = NULL,
                width = "80%"
              )

            ),

            selectInput(
              "is_corr_CTprop_Asc",
              "Do you think there is any correlation between the proportion of contacts traced and case ascertainment? e.g. if case ascertainment is higher, the proportion of contacts traced is also higher.",
              c("Not sure", "Yes", "No"),
              selected = "Not sure",
              width = "80%"
            ),

            conditionalPanel(
              condition = "input.is_corr_CTprop_Asc=='Yes'",
              selectInput(
                "corr_CTprop_Asc",
                "Do you think the correlation is positive (i.e. when case ascertainment is high, the proportion of contacts traced is high and when case ascertainment is low, the proportion of contacts traced is low) or negative (i.e. when case ascertainment is low, the proportion of contacts traced is high and vice versa)?",
                c("Positive", "Negative"),
                selected = NULL,
                width = "80%"
              )

            ),

            textAreaInput(
              "source_CTprop",
              "Please provide any context or sources that have guided your intuition:",
              width = "80%"
            )
          ),

          layout_column_wrap(
            1 / 2,
            actionButton("previousCTprop", "Previous"),
            actionButton("nextCTprop", "Next", class =
                           "btn-primary")
          )
         )
       ),
       accordion_panel(
         "Proportion of contacts who complete follow up",
         value = "CTfoll",
         layout_sidebar(
           sidebar = sidebar(
             title = tags$h4("Proportion followed up"),
             width = 300,
             shiny::p("Based on your knowledge and experience of recent Ebola outbreaks:"),
             
             selectInput(
               "answerCTfoll",
               "Can you provide your intuition about the distribution of the proportion of contacts who complete follow up?",
               c("No", "Yes")
             ),
             
             conditionalPanel(
               condition="input.answerCTfoll=='Yes'",
               selectInput("CTfoll_shape","What do you think the shape of the distribution of the proportion of contacts who complete follow up is?",
                           c("Uniform","Normal","Beta")),
               
               conditionalPanel(
                 condition="input.CTfoll_shape=='Uniform'",
                 sliderInput("CTfoll_min","What do you think the minimum value of the proportion of contacts who complete follow up is?",min=0,max=1,value=0,step=0.05,round=-2)
               ),
               
               conditionalPanel(
                 condition="input.CTfoll_shape=='Uniform'",
                 sliderInput("CTfoll_max","What do you think the maximum value of the proportion of contacts who complete follow up is?",min=0,max=1,value=1,step=0.05,round=-2)
               ),
               
               conditionalPanel(
                 condition="input.CTfoll_shape=='Normal'",
                 sliderInput("CTfoll_mean","What do you think the mean value of the proportion of contacts who complete follow up is?",min=0,max=1,value=0.5,step=0.05,round=-2)
               ),
               
               conditionalPanel(
                 condition="input.CTfoll_shape=='Normal'",
                 sliderInput("CTfoll_sd","What do you think the standard deviation of the proportion of contacts who complete follow up is?",min=0.1,max=1,value=0.5,step=0.01,round=-2)
               ),
               
               conditionalPanel(
                 condition="input.CTfoll_shape=='Normal'",
                 sliderInput("CTfoll_min_norm","What do you think the minimum value of the proportion of contacts who complete follow up is?",min=0,max=1,value=0,step=0.05,round=-2)
               ),
               
               conditionalPanel(
                 condition="input.CTfoll_shape=='Normal'",
                 sliderInput("CTfoll_max_norm","What do you think the maximum value of the proportion of contacts who complete follow up is?",min=0,max=1,value=1,step=0.05,round=-2)
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
                 condition="input.CTfoll_shape=='Beta'",
                 sliderInput("CTfoll_means","What do you think the mean value of the proportion of contacts who complete follow up is?",min=0.01,max=0.99,value=0.5,step=0.05,round=-2)
               ),
               
               conditionalPanel(
                 condition="input.CTfoll_shape=='Beta'",
                 sliderInput("CTfoll_betasd","What do you think the standard deviation of the proportion of contacts who complete follow up is?",min=0.01,max=round(sqrt(1/12),2),value=sqrt(1/12),step=0.001,round=-3)
               ),
               
               conditionalPanel(
                 condition="input.CTfoll_shape=='Beta'",
                 sliderInput("CTfoll_min_beta","What do you think the minimum value of the proportion of contacts who complete follow up is?",min=0,max=1,value=0,step=0.05,round=-2)
               ),
               
               conditionalPanel(
                 condition="input.CTfoll_shape=='Beta'",
                 sliderInput("CTfoll_max_beta","What do you think the maximum value of the proportion of contacts who complete follow up is?",min=0,max=1,value=1,step=0.05,round=-2)
               )
             )
           ),
           
           
           
           conditionalPanel(
             condition = "input.answerCTfoll=='Yes'",
             plotOutput("plotCTfoll", width =
                          "100%", height = '500px'),
             textOutput("CTfollmedian"),
             textOutput("CTfollconf"),
             
             selectInput(
               "conf_CTfoll",
               "How confident are you about the shape of the distribution?",
               c("Very", "Somewhat", "Slightly", "Not very"),
               width = "80%",
               selected = "Not very"
             ),
             
             selectInput(
               "is_corr_CTfoll_R0",
               "Do you think there is any correlation between the proportion of contacts who complete follow up and the reproduction number? e.g. if the reproduction number is higher, the proportion of contacts who complete follow up is also higher.",
               c("Not sure", "Yes", "No"),
               selected = "Not sure",
               width = "80%"
             ),
             
             conditionalPanel(
               condition = "input.is_corr_CTfoll_R0=='Yes'",
               selectInput(
                 "corr_CTfoll_R0",
                 "Do you think the correlation is positive (i.e. when reproduction number is high, the proportion of contacts who complete follow up is high and when reproduction number is low, the proportion of contactswho complete follow up is low) or negative (i.e. when reproduction number is low, the proportion of contacts who complete follow up is high and vice versa)?",
                 c("Positive", "Negative"),
                 selected = NULL,
                 width = "80%"
               )
               
             ),
             
             selectInput(
               "is_corr_CTfoll_Asc",
               "Do you think there is any correlation between the proportion of contacts who complete follow up and case ascertainment? e.g. if case ascertainment is higher, the proportion of contacts who complete follow up is also higher.",
               c("Not sure", "Yes", "No"),
               selected = "Not sure",
               width = "80%"
             ),
             
             conditionalPanel(
               condition = "input.is_corr_CTfoll_Asc=='Yes'",
               selectInput(
                 "corr_CTfoll_Asc",
                 "Do you think the correlation is positive (i.e. when case ascertainment is high, the proportion of contacts who complete follow up is high and when case ascertainment is low, the proportion of contacts who complete follow up is low) or negative (i.e. when case ascertainment is low, the proportion of contacts who complete follow up is high and vice versa)?",
                 c("Positive", "Negative"),
                 selected = NULL,
                 width = "80%"
               )
               
             ),
             
             textAreaInput(
               "source_CTfoll",
               "Please provide any context or sources that have guided your intuition:",
               width = "80%"
             )
           ),
           
           layout_column_wrap(
             1 / 2,
             actionButton("previousCTfoll", "Previous"),
             actionButton("nextCTfoll", "Next", class =
                            "btn-primary")
           )
         )
       )
     )
  ),


# HCW vaccination ---------------------------------------------------------

  
  bslib::nav_panel(title="6",
                   

                   shiny::p(tags$h3("Healthcare and frontline worker vaccination")), 
                   shiny::p("Healthcare and frontline workers are ..."), 

                   accordion(
                     id = "HCWvacc",
                     accordion_panel(
                       "Preventative vaccination",
                       value = "HCWvacc_prevent",
                       layout_sidebar(
                         sidebar = sidebar(
                           title = tags$h4("Proportion of HCWs and FLWs who accept preventative vaccination"),
                           width = 300,
                           shiny::p("Based on your knowledge and experience of recent preventative vaccination campaigns against EVD:"),
                           
                           selectInput(
                             "answerHCWvacc_prevent",
                             "Can you provide your intuition about the distribution of the proportion of HCWs/FLWs who accept preventative vaccination?",
                             c("No", "Yes")
                           ),
                           
                           conditionalPanel(
                             condition = "input.answerHCWvacc_prevent=='Yes'",
                             selectInput(
                               "HCWvacc_prevent_shape",
                               "What do you think the shape of the distribution of the proportion of HCWs/FLWs who accept preventative vaccination is?",
                               c("Uniform", "Normal", "Beta")
                             ),
                             
                             conditionalPanel(
                               condition = "input.HCWvacc_prevent_shape=='Uniform'",
                               sliderInput(
                                 "HCWvacc_prevent_min",


                                 "What do you think the minimum proportion of HCWs/FLWs who accept preventative vaccination is?",

                                 min = 0,
                                 max = 1,
                                 value = 0,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.HCWvacc_prevent_shape=='Uniform'",
                               sliderInput(
                                 "HCWvacc_prevent_max",

                                 "What do you think the maximum proportion of HCWs/FLWs who accept preventative vaccination is?",

                                 min = 0,
                                 max = 1,
                                 value = 1,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.HCWvacc_prevent_shape=='Normal'",
                               sliderInput(
                                 "HCWvacc_prevent_mean",

                                 "What do you think the mean proportion of HCWs/FLWs who accept preventative vaccination is?",

                                 min = 0,
                                 max = 1,
                                 value = 0.5,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.HCWvacc_prevent_shape=='Normal'",
                               sliderInput(
                                 "HCWvacc_prevent_sd",

                                 "What do you think the standard deviation proportion of HCWs/FLWs who accept preventative vaccination is?",

                                 min = 0.1,
                                 max = 1,
                                 value = 0.5,
                                 step = 0.01,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(

                               condition = "input.HCWvacc_prevent_shape=='Normal'",
                               sliderInput(
                                 "HCWvacc_prevent_min_norm",
                                 "What do you think the minimum proportion of HCWs/FLWs who accept preventative vaccination is?",
                                 min = 0,
                                 max = 1,
                                 value = 0,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.HCWvacc_prevent_shape=='Normal'",
                               sliderInput(
                                 "HCWvacc_prevent_max_norm",
                                 "What do you think the maximum proportion of HCWs/FLWs who accept preventative vaccination is?",
                                 min = 0,
                                 max = 1,
                                 value = 1,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             
                             conditionalPanel(
                               condition = "input.HCWvacc_prevent_shape=='Beta'",
                               sliderInput(
                                 "HCWvacc_prevent_means",

                                 "What do you think the mean proportion of HCWs/FLWs who accept preventative vaccination is",min=0.01,max=0.99,value=0.5,step=0.05,round=-2)
                             ),
                           
                           
                             

                             conditionalPanel(

                               condition = "input.HCWvacc_prevent_shape=='Beta'",
                               sliderInput(
                                 "HCWvacc_prevent_betasd",

                                 "What do you think the standard deviation of the proportion of HCWs/FLWs who accept preventative vaccination is?",min=0.01,max=round(sqrt(1/12),2),value=sqrt(1/12),step=0.001,round=-3)
                             ),
                           
                             conditionalPanel(
                               condition = "input.HCWvacc_prevent_shape=='Beta'",
                               sliderInput(
                                 "HCWvacc_prevent_min_beta",
                                 "What do you think the minimum proportion of HCWs/FLWs who accept preventative vaccination is?",
                                 min = 0,
                                 max = 1,
                                 value = 0,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.HCWvacc_prevent_shape=='Beta'",
                               sliderInput(
                                 "HCWvacc_prevent_max_beta",
                                 "What do you think the maximum proportion of HCWs/FLWs who accept preventative vaccination is?",
                                 min = 0,
                                 max = 1,
                                 value = 1,
                                 step = 0.05,
                                 round = -2
                               )
                             )
                           )
                         ),
                         
                         conditionalPanel(
                           condition = "input.answerHCWvacc_prevent=='Yes'",

                           plotOutput("plotHCWvacc_prevent", width =
                                        "100%", height = '500px'),
                           textOutput("HCWvacc_preventmedian"),
                           textOutput("HCWvacc_preventconf"),

                           
                           selectInput(
                             "conf_HCWvacc_prevent",
                             "How confident are you about the shape of the distribution?",
                             c("Very", "Somewhat", "Slightly", "Not very"),
                             width = "80%",
                             selected = "Not very"
                           ),
                           
                           # selectInput(
                           #   "is_corr_HCWvacc_prevent_R0",
                           #   "Do you think there is any correlation between the proportion of HCWs who accept vaccination and the reproduction number? e.g. if the reproduction number is higher, the proportion of HCWs who accept vaccination is also higher.",
                           #   c("Not sure", "Yes", "No"),
                           #   selected = "Not sure",
                           #   width = "80%"
                           # ),
                           # 
                           # conditionalPanel(
                           #   condition = "input.is_corr_HCWvacc_prevent_R0=='Yes'",
                           #   selectInput(
                           #     "corr_HCWvacc_prevent_R0",
                           #     "Do you think the correlation is positive (i.e. when reproduction number is high, the proportion of HCWs who accept vaccination is high and when reproduction number is low, the proportion of HCWs who accept vaccination is low) or negative (i.e. when reproduction number is low, the proportion of HCWs who accept vaccination is high and vice versa)?",
                           #     c("Positive", "Negative"),
                           #     selected = NULL,
                           #     width = "80%"
                           #   )
                           #   
                           # ),
                           # 
                           # selectInput(
                           #   "is_corr_HCWvacc_prevent_Asc",
                           #   "Do you think there is any correlation between the proportion of HCWs who accept vaccination and case ascertainment? e.g. if case ascertainment is higher, the proportion of HCWs who accept vaccination is also higher.",
                           #   c("Not sure", "Yes", "No"),
                           #   selected = "Not sure",
                           #   width = "80%"
                           # ),
                           # 
                           # conditionalPanel(
                           #   condition = "input.is_corr_HCWvacc_prevent_Asc=='Yes'",
                           #   selectInput(
                           #     "corr_HCWvacc_prevent_Asc",
                           #     "Do you think the correlation is positive (i.e. when case ascertainment is high, the proportion of HCWs who accept vaccination is high and when case ascertainment is low, the proportion of HCWs who accept vaccination is low) or negative (i.e. when case ascertainment is low, the proportion of HCWs who accept vaccination and vice versa)?",
                           #     c("Positive", "Negative"),
                           #     selected = NULL,
                           #     width = "80%"
                           #   )
                           #   
                           # ),

                           
                           textAreaInput(
                             "source_HCWvacc_prevent",
                             "Please provide any context or sources that have guided your intuition:",
                             width = "80%"
                           )
                         ),
                         
                         layout_column_wrap(
                           1 / 2,
                           actionButton("previousHCWvacc_prevent", "Previous"),
                           actionButton("nextHCWvacc_prevent", "Next", class =
                                          "btn-primary")
                         )
                       )
                     ),
                     accordion_panel(

                       "Reactive vaccination",
                       value = "HCWvacc_react",
                       layout_sidebar(
                         sidebar = sidebar(
                           title = tags$h4("Proportion of HCW/FLWs who accept reactive vaccination"),
                           width = 300,
                           shiny::p(""),
                           shiny::p("Based on your knowledge and experience of recent Ebola outbreaks:"),

                           selectInput(
                             "answerHCWvacc_react",

                             "Can you provide your intuition about the proportion of HCW/FLWs who accept reactive vaccination?",

                             c("No", "Yes")
                           ),

                           conditionalPanel(
                             condition = "input.answerHCWvacc_react=='Yes'",
                             selectInput(
                               "HCWvacc_react_shape",

                               "What do you think the shape of the distribution of the proportion of HCW/FLWs who accept reactive vaccination is?",
                               c("Uniform", "Normal", "Beta")

                             ),

                             conditionalPanel(
                               condition = "input.HCWvacc_react_shape=='Uniform'",
                               sliderInput(
                                 "HCWvacc_react_min",

                                 "What do you think the minimum value of the proportion of HCW/FLWs who accept reactive vaccination is?",

                                 min = 0,
                                 max = 1,
                                 value = 0,
                                 step = 0.05,
                                 round = -2
                               )
                             ),

                             conditionalPanel(
                               condition = "input.HCWvacc_react_shape=='Uniform'",
                               sliderInput(
                                 "HCWvacc_react_max",

                                 "What do you think the maximum value of the proportion of HCW/FLWs who accept reactive vaccination is?",

                                 min = 0,
                                 max = 1,
                                 value = 1,
                                 step = 0.05,
                                 round = -2
                               )
                             ),

                             conditionalPanel(
                               condition = "input.HCWvacc_react_shape=='Normal'",
                               sliderInput(
                                 "HCWvacc_react_mean",

                                 "What do you think the mean value of the proportion of HCW/FLWs who accept reactive vaccination is?",

                                 min = 0,
                                 max = 1,
                                 value = 0.5,
                                 step = 0.05,
                                 round = -2
                               )
                             ),

                             conditionalPanel(
                               condition = "input.HCWvacc_react_shape=='Normal'",
                               sliderInput(
                                 "HCWvacc_react_sd",

                                 "What do you think the standard deviation of the proportion of HCW/FLWs who accept reactive vaccination is?",

                                 min = 0.1,
                                 max = 1,
                                 value = 0.5,
                                 step = 0.01,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(

                               condition = "input.HCWvacc_react_shape=='Normal'",
                               sliderInput(
                                 "HCWvacc_react_min_norm",
                                 "What do you think the minimum value of the proportion of HCW/FLWs who accept reactive vaccination is?",
                                 min = 0,
                                 max = 1,
                                 value = 0,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.HCWvacc_react_shape=='Normal'",
                               sliderInput(
                                 "HCWvacc_react_max_norm",
                                 "What do you think the maximum value of the proportion of HCW/FLWs who accept reactive vaccination is?",
                                 min = 0,
                                 max = 1,
                                 value = 1,
                                 step = 0.05,
                                 round = -2
                               )
                             ),

                             conditionalPanel(
                               condition = "input.HCWvacc_react_shape=='Beta'",
                               sliderInput(
                                 "HCWvacc_react_means",
                                 "What do you think the mean value of the proportion of HCW/FLWs who accept reactive vaccination is?",min=0.01,max=0.99,value=0.5,step=0.05,round=-2)
                             ),

                             conditionalPanel(

                               condition = "input.HCWvacc_react_shape=='Beta'",
                               sliderInput(
                                 "HCWvacc_react_betasd",
                                 "What do you think the variance of the proportion of HCW/FLWs who accept reactive vaccination is?",min=0.01,max=round(sqrt(1/12),2),value=sqrt(1/12),step=0.001,round=-3)
                             ),
                             
                             conditionalPanel(
                               condition = "input.HCWvacc_react_shape=='Beta'",
                               sliderInput(
                                 "HCWvacc_react_min_beta",
                                 "What do you think the minimum value of the proportion of HCW/FLWs who accept reactive vaccination is?",
                                 min = 0,
                                 max = 1,
                                 value = 0,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.HCWvacc_react_shape=='Beta'",
                               sliderInput(
                                 "HCWvacc_react_max_beta",
                                 "What do you think the maximum value of the proportion of HCW/FLWs who accept reactive vaccination is?",
                                 min = 0,
                                 max = 1,
                                 value = 1,
                                 step = 0.05,
                                 round = -2
                               )
                             )
                           )
                        ),
                           
                           

                         

                         conditionalPanel(

                           condition = "input.answerHCWvacc_react=='Yes'",
                           plotOutput("plotHCWvacc_react", width =
                                        "100%", height = '500px'),
                           textOutput("HCWvacc_reactmedian"),
                           textOutput("HCWvacc_reactconf"),

                           selectInput(
                             "conf_HCWvacc_react",
                             "How confident are you about the shape of the distribution?",
                             c("Very", "Somewhat", "Slightly", "Not very"),
                             width = "80%",
                             selected = "Not very"
                           ),

                           selectInput(
                             "is_corr_HCWvacc_react_R0",

                             "Do you think there is any correlation between the proportion of HCW/FLWs who accept reactive vaccination and reproduction number? e.g. if the reproduction number is higher, the proportion of HCW/FLWs who accept reactive vaccination is also higher.",

                             c("Not sure", "Yes", "No"),
                             selected = "Not sure",
                             width = "80%"
                           ),

                           conditionalPanel(
                             condition = "input.is_corr_HCWvacc_react_R0=='Yes'",
                             selectInput(
                               "corr_HCWvacc_react_R0",

                               "Do you think the correlation is positive (i.e. when reproduction number is high, the proportion of HCW/FLWs who accept reactive vaccination is high and when reproduction number is low, the proportion of HCW/FLWs who accept reactive vaccination is low) or negative (i.e. when reproduction number is low, the proportion of HCW/FLWs who accept reactive vaccination is high and vice versa)?",
                               c("Positive", "Negative"),
                               selected = NULL,
                               width = "80%"
                             )

                           ),

                           selectInput(
                             "is_corr_HCWvacc_react_Asc",

                             "Do you think there is any correlation between the proportion of HCW/FLWs who accept reactive vaccination and case ascertainment? e.g. if case ascertainment is higher, the proportion of HCW/FLWs who accept reactive vaccination is also higher.",

                             c("Not sure", "Yes", "No"),
                             selected = "Not sure",
                             width = "80%"
                           ),

                           conditionalPanel(
                             condition = "input.is_corr_HCWvacc_react_Asc=='Yes'",
                             selectInput(
                               "corr_HCWvacc_react_Asc",

                               "Do you think the correlation is positive (i.e. when case ascertainment is high, the proportion of HCW/FLWs who accept reactive vaccination is high and when case ascertainment is low, the proportion of HCW/FLWs who accept reactive vaccination is low) or negative (i.e. when case ascertainment is low, the proportion of HCW/FLWs who accept reactive vaccination is high and vice versa)?",

                               c("Positive", "Negative"),
                               selected = NULL,
                               width = "80%"
                             )

                           ),

                           textAreaInput(
                             "source_HCWvacc_react",
                             "Please provide any context or sources that have guided your intuition:",
                             width = "80%"
                           )
                         ),

                         layout_column_wrap(
                           1 / 2,
                           actionButton("previousHCWvacc_react", "Previous"),
                           actionButton("nextHCWvacc_react", "Next", class =
                                          "btn-primary")
                         )
                       )
                     ),
                     accordion_panel(

                       "Delay from outbreak detection to HCW/FLW vaccination",

                       value = "HCWvacc_delay",
                       layout_sidebar(
                         sidebar = sidebar(
                           title = tags$h4("Delay to start HCW/FLW vaccination"),
                           width = 300,
                           shiny::p(""),
                           shiny::p("Based on your knowledge and experience of recent Ebola outbreaks:"),

                           selectInput(
                             "answerHCWvacc_delay",

                             "Can you provide your intuition about the distribution delays to start HCW/FLW vaccination?",

                             c("No", "Yes")
                           ),

                           conditionalPanel(
                             condition = "input.answerHCWvacc_delay=='Yes'",
                             selectInput(
                               "HCWvacc_delay_shape",

                               "What do you think the shape of the distribution of delays to start HCW/FLW vaccination is?",

                               c("Uniform", "Normal", "Skewed")
                             ),

                             conditionalPanel(
                               condition = "input.HCWvacc_delay_shape=='Uniform'",
                               sliderInput(
                                 "HCWvacc_delay_min",

                                 "What do you think the minimum delay to start HCW/FLW vaccination is?",

                                 min = 0,
                                 max = 15,
                                 value = 0,
                                 step = 0.5,
                                 round = -1
                               )
                             ),

                             conditionalPanel(
                               condition = "input.HCWvacc_delay_shape=='Uniform'",
                               sliderInput(
                                 "HCWvacc_delay_max",

                                 "What do you think the maximum delay to start HCW/FLW vaccination is?",

                                 min = 0,
                                 max = 15,
                                 value = 15,
                                 step = 0.5,
                                 round = -1
                               )
                             ),

                             conditionalPanel(
                               condition = "input.HCWvacc_delay_shape=='Normal'",
                               sliderInput(
                                 "HCWvacc_delay_mean",

                                 "What do you think the mean delay to start HCW/FLW vaccination is?",

                                 min = 0,
                                 max = 15,
                                 value = 7.5,
                                 step = 0.5,
                                 round = -1
                               )
                             ),

                             conditionalPanel(
                               condition = "input.HCWvacc_delay_shape=='Normal'",
                               sliderInput(
                                 "HCWvacc_delay_sd",

                                 "What do you think the standard deviation of the delay to start HCW/FLW vaccination is?",

                                 min = 1,
                                 max = 10,
                                 value = 5,
                                 step = 0.1,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.HCWvacc_delay_shape=='Normal'",
                               sliderInput(

                                 "HCWvacc_delay_min_norm",
                                 "What do you think the minimum delay to start HCW/FLW vaccination is?",
                                 min = 0,
                                 max = 15,
                                 value = 0,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.HCWvacc_delay_shape=='Normal'",
                               sliderInput(
                                 "HCWvacc_delay_max_norm",
                                 "What do you think the maximum delay to start HCW/FLW vaccination is?",
                                 min = 0,
                                 max = 15,
                                 value = 15,
                                 step = 0.5,
                                 round = -1
                               )
                             ),

                             conditionalPanel(
                               condition = "input.HCWvacc_delay_shape=='Skewed'",
                               sliderInput(
                                 "HCWvacc_delay_means",
                                 "What do you think the mean delay to start HCW/FLW vaccination is?",

                                 min = 0,
                                 max = 15,
                                 value = 7.5,
                                 step = 0.5,
                                 round = -1
                               )
                             ),

                             conditionalPanel(
                               condition = "input.HCWvacc_delay_shape=='Skewed'",
                               sliderInput(
                                 "HCWvacc_delay_var",
                                 "What do you think the variance of the delay to start HCW/FLW vaccination is?",
                                 min = 1,
                                 max = 50,
                                 value = 25,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.HCWvacc_delay_shape=='Skewed'",
                               sliderInput(
                                 "HCWvacc_delay_min_skewed",
                                 "hat do you think the minimum delay to start HCW/FLW vaccination is?",
                                 min = 0,
                                 max = 15,
                                 value = 0,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.HCWvacc_delay_shape=='Skewed'",
                               sliderInput(
                                 "HCWvacc_delay_max_skewed",
                                 "What do you think the maximum delay to start HCW/FLW vaccination is?",
                                 min = 0,
                                 max = 15,
                                 value = 15,
                                 step = 0.5,
                                 round = -1
                               )
                             )
                           )

                         ),

                         conditionalPanel(
                           condition = "input.answerHCWvacc_delay=='Yes'",
                           plotOutput("plotHCWvacc_delay", width =
                                        "100%", height = '500px'),
                           textOutput("HCWvacc_delay_median"),
                           textOutput("HCWvacc_delay_conf"),

                           selectInput(
                             "conf_HCWvacc_delay",
                             "How confident are you about the shape of the distribution?",
                             c("Very", "Somewhat", "Slightly", "Not very"),
                             width = "80%",
                             selected = "Not very"
                           ),

                           selectInput(
                             "is_corr_HCWvacc_delay_R0",
                             "Do you think there is any correlation between the delay to start HCW/FLW vaccination and reproduction number? e.g. if the reproduction number is higher, the delay to start HCW/FLW vaccination is also higher.",
                             c("Not sure", "Yes", "No"),
                             selected = "Not sure",
                             width = "80%"
                           ),

                           conditionalPanel(
                             condition = "input.is_corr_HCWvacc_delay_R0=='Yes'",
                             selectInput(
                               "corr_HCWvacc_delay_R0",
                               "Do you think the correlation is positive (i.e. when reproduction number is high, the delay to start HCW/FLW vaccination is high and when reproduction number is low, the delay to start HCW/FLW vaccination is low) or negative (i.e. when reproduction number is low, the delay to start HCW/FLW vaccination is high and vice versa)?",
                               c("Positive", "Negative"),
                               selected = NULL,
                               width = "80%"
                             )

                           ),

                           selectInput(
                             "is_corr_HCWvacc_delay_Asc",
                             "Do you think there is any correlation between the delay to start vaccination and case ascertainment? e.g. if case ascertainment is higher, the delay to start HCW/FLW vaccination is also higher.",
                             c("Not sure", "Yes", "No"),
                             selected = "Not sure",
                             width = "80%"
                           ),

                           conditionalPanel(
                             condition = "input.is_corr_HCWvacc_delay_Asc=='Yes'",
                             selectInput(
                               "corr_HCWvacc_delay_Asc",
                               "Do you think the correlation is positive (i.e. when case ascertainment is high, the delay to start HCW/FLW vaccination is high and when case ascertainment is low, the delay to start HCW/FLW vaccination is low) or negative (i.e. when case ascertainment is low, the delay to start HCW vaccination is high and vice versa)?",
                               c("Positive", "Negative"),
                               selected = NULL,
                               width = "80%"
                             )

                           ),

                           textAreaInput(
                             "source_HCWvacc_delay",
                             "Please provide any context or sources that have guided your intuition:",
                             width = "80%"
                           )
                         ),

                         layout_column_wrap(
                           1 / 2,
                           actionButton("previousHCWvacc_delay", "Previous"),
                           actionButton("nextHCWvacc_delay", "Next", class =
                                          "btn-primary")
                         )
                       )
                     )
                   )
                   
                   
  ), 


# Ring vaccination --------------------------------------------------------


  
  bslib::nav_panel(title="7",

                   shiny::p(tags$h3("Ring vaccination")),

                   shiny::p("Reactive vaccination campaigns carried out during EVD outbreaks target both healthcare workers (HCWs) and frontline workers (FLWs), and at-risk contacts of cases. The
                     latter is typically triggered by the ascertainment of a case and can be carried out using ring vaccination or geographically targeted vaccination. We are interested in
                     vaccine uptake for these different strategies, as well as the time taken to initiate vaccination following ascertainment of a case."),
                   
                   
                   accordion(
                     id = "Ringvacc",
                     accordion_panel(
                       "Proportion of cases that have vaccination rings established",
                       value = "Ringvacc_ring",
                       layout_sidebar(
                         sidebar = sidebar(
                           title = tags$h4("Proportion of cases that have vaccination rings established"),
                           width = 300,
                           shiny::p("Based on your knowledge and experience of recent reactive vaccination campaigns against EVD:"),
                           
                           selectInput(
                             "answerRingvacc_ring",
                             "Can you provide your intuition about the distribution of the proportion of cases for which a vaccination ring is established?",
                             c("No", "Yes")
                           ),
                           
                           conditionalPanel(
                             condition = "input.answerRingvacc_ring=='Yes'",
                             selectInput(
                               "Ringvacc_ring_shape",
                               "What do you think the shape of the distribution of the proportion of cases for which a vaccination ring is established is?",
                               c("Uniform", "Normal", "Beta")
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_ring_shape=='Uniform'",
                               sliderInput(
                                 "Ringvacc_ring_min",
                                 
                                 
                                 "What do you think the minimum proportion of cases for which a vaccination ring is established is?",
                                 
                                 min = 0,
                                 max = 1,
                                 value = 0,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_ring_shape=='Uniform'",
                               sliderInput(
                                 "Ringvacc_ring_max",
                                 
                                 "What do you think the maximum proportion of cases for which a vaccination ring is established is?",
                                 
                                 min = 0,
                                 max = 1,
                                 value = 1,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_ring_shape=='Normal'",
                               sliderInput(
                                 "Ringvacc_ring_mean",
                                 
                                 "What do you think the mean proportion of cases for which a vaccination ring is established is?",
                                 
                                 min = 0,
                                 max = 1,
                                 value = 0.5,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_ring_shape=='Normal'",
                               sliderInput(
                                 "Ringvacc_ring_sd",
                                 
                                 "What do you think the standard deviation proportion of cases for which a vaccination ring is established is?",
                                 
                                 min = 0.1,
                                 max = 1,
                                 value = 0.5,
                                 step = 0.01,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               
                               condition = "input.Ringvacc_ring_shape=='Normal'",
                               sliderInput(
                                 "Ringvacc_ring_min_norm",
                                 "What do you think the minimum proportion of cases for which a vaccination ring is established is?",
                                 min = 0,
                                 max = 1,
                                 value = 0,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_ring_shape=='Normal'",
                               sliderInput(
                                 "Ringvacc_ring_max_norm",
                                 "What do you think the maximum proportion of cases for which a vaccination ring is established is?",
                                 min = 0,
                                 max = 1,
                                 value = 1,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_ring_shape=='Beta'",
                               sliderInput(
                                 "Ringvacc_ring_means",
                                 
                                 "What do you think the mean proportion of cases for which a vaccination ring is established is",min=0.01,max=0.99,value=0.5,step=0.05,round=-2)
                             ),
                             
                             
                             
                             
                             conditionalPanel(
                               
                               condition = "input.Ringvacc_ring_shape=='Beta'",
                               sliderInput(
                                 "Ringvacc_ring_betasd",
                                 
                                 "What do you think the standard deviation of the proportion of cases for which a vaccination ring is established is?",min=0.01,max=round(sqrt(1/12),2),value=sqrt(1/12),step=0.001,round=-3)
                             )
                             
                           ),
                           
                           conditionalPanel(
                             condition = "input.Ringvacc_ring_shape=='Beta'",
                             sliderInput(
                               "Ringvacc_ring_min_beta",
                               "What do you think the minimum proportion of cases for which a vaccination ring is established is?",
                               min = 0,
                               max = 1,
                               value = 0,
                               step = 0.05,
                               round = -2
                             )
                           ),
                           
                           conditionalPanel(
                             condition = "input.Ringvacc_ring_shape=='Beta'",
                             sliderInput(
                               "Ringvacc_ring_max_beta",
                               "What do you think the maximum proportion of cases for which a vaccination ring is established is?",
                               min = 0,
                               max = 1,
                               value = 1,
                               step = 0.05,
                               round = -2
                             )
                           ),
                           
                         ),
                         
                         conditionalPanel(
                           condition = "input.answerRingvacc_ring=='Yes'",
                           
                           plotOutput("plotRingvacc_ring", width =
                                        "100%", height = '500px'),
                           textOutput("Ringvacc_ringmedian"),
                           textOutput("Ringvacc_ringconf"),
                           
                           
                           selectInput(
                             "conf_Ringvacc_ring",
                             "How confident are you about the shape of the distribution?",
                             c("Very", "Somewhat", "Slightly", "Not very"),
                             width = "80%",
                             selected = "Not very"
                           ),
                           
                           
                           
                           textAreaInput(
                             "source_Ringvacc_ring",
                             "Please provide any context or sources that have guided your intuition:",
                             width = "80%"
                           )
                         ),
                         
                         layout_column_wrap(
                           1 / 2,
                           actionButton("previousRingvacc_ring", "Previous"),
                           actionButton("nextRingvacc_ring", "Next", class =
                                          "btn-primary")
                         )
                       )
                     ),
                     accordion_panel(
                       
                       "Proportion of ring members who accept the vaccine",
                       value = "Ringvacc_react",
                       layout_sidebar(
                         sidebar = sidebar(
                           title = tags$h4("Proportion members of a ring accept the vaccine"),
                           width = 300,
                           shiny::p(""),
                           shiny::p("Based on your knowledge and experience of recent Ebola outbreaks:"),
                           
                           selectInput(
                             "answerRingvacc_react",
                             
                             "Can you provide your intuition about the proportion of ring members who accept vaccination?",
                             
                             c("No", "Yes")
                           ),
                           
                           conditionalPanel(
                             condition = "input.answerRingvacc_react=='Yes'",
                             selectInput(
                               "Ringvacc_react_shape",
                               
                               "What do you think the shape of the distribution of the proportion of ring members who accept vaccination is?",
                               c("Uniform", "Normal", "Beta")
                               
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_react_shape=='Uniform'",
                               sliderInput(
                                 "Ringvacc_react_min",
                                 
                                 "What do you think the minimum value of the proportion of ring members who accept vaccination is?",
                                 
                                 min = 0,
                                 max = 1,
                                 value = 0,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_react_shape=='Uniform'",
                               sliderInput(
                                 "Ringvacc_react_max",
                                 
                                 "What do you think the maximum value of the proportion of ring members who accept vaccination is?",
                                 
                                 min = 0,
                                 max = 1,
                                 value = 1,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_react_shape=='Normal'",
                               sliderInput(
                                 "Ringvacc_react_mean",
                                 
                                 "What do you think the mean value of the proportion of ring members who accept vaccination is?",
                                 
                                 min = 0,
                                 max = 1,
                                 value = 0.5,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_react_shape=='Normal'",
                               sliderInput(
                                 "Ringvacc_react_sd",
                                 
                                 "What do you think the standard deviation of the proportion of ring members who accept vaccination is?",
                                 
                                 min = 0.1,
                                 max = 1,
                                 value = 0.5,
                                 step = 0.01,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               
                               condition = "input.Ringvacc_react_shape=='Normal'",
                               sliderInput(
                                 "Ringvacc_react_min_norm",
                                 "What do you think the minimum value of the proportion of ring members who accept vaccination is?",
                                 min = 0,
                                 max = 1,
                                 value = 0,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_react_shape=='Normal'",
                               sliderInput(
                                 "Ringvacc_react_max_norm",
                                 "What do you think the maximum value of the proportion of ring members who accept vaccination is?",
                                 min = 0,
                                 max = 1,
                                 value = 1,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_react_shape=='Beta'",
                               sliderInput(
                                 "Ringvacc_react_means",
                                 "What do you think the mean value of the proportion of ring members who accept vaccination is?",min=0.01,max=0.99,value=0.5,step=0.05,round=-2)
                             ),
                             
                             conditionalPanel(
                               
                               condition = "input.Ringvacc_react_shape=='Beta'",
                               sliderInput(
                                 "Ringvacc_react_betasd",
                                 "What do you think the standard deviation of the proportion of ring members who accept vaccination is?",min=0.01,max=round(sqrt(1/12),2),value=sqrt(1/12),step=0.001,round=-3)
                             )
                           ),
                           
                           conditionalPanel(
                             condition = "input.Ringvacc_react_shape=='Beta'",
                             sliderInput(
                               "Ringvacc_react_min_beta",
                               "What do you think the minimum value of the proportion of ring members who accept vaccination is?",
                               min = 0,
                               max = 1,
                               value = 0,
                               step = 0.05,
                               round = -2
                             )
                           ),
                           
                           conditionalPanel(
                             condition = "input.Ringvacc_react_shape=='Beta'",
                             sliderInput(
                               "Ringvacc_react_max_beta",
                               "What do you think the maximum value of the proportion of ring members who accept vaccination is?",
                               min = 0,
                               max = 1,
                               value = 1,
                               step = 0.05,
                               round = -2
                             )
                           )
                         
                         ),
                         
                         conditionalPanel(
                           
                           condition = "input.answerRingvacc_react=='Yes'",
                           plotOutput("plotRingvacc_react", width =
                                        "100%", height = '500px'),
                           textOutput("Ringvacc_reactmedian"),
                           textOutput("Ringvacc_reactconf"),
                           
                           selectInput(
                             "conf_Ringvacc_react",
                             "How confident are you about the shape of the distribution?",
                             c("Very", "Somewhat", "Slightly", "Not very"),
                             width = "80%",
                             selected = "Not very"
                           ),
                           
                           selectInput(
                             "is_corr_Ringvacc_react_R0",
                             
                             "Do you think there is any correlation between the proportion of ring members who accept vaccination and reproduction number? e.g. if the reproduction number is higher, the proportion of ring members who accept vaccination is also higher.",
                             
                             c("Not sure", "Yes", "No"),
                             selected = "Not sure",
                             width = "80%"
                           ),
                           
                           conditionalPanel(
                             condition = "input.is_corr_Ringvacc_react_R0=='Yes'",
                             selectInput(
                               "corr_Ringvacc_react_R0",
                               
                               "Do you think the correlation is positive (i.e. when reproduction number is high, the proportion of ring members who accept vaccination is high and when reproduction number is low, the proportion of ring members who accept vaccination is low) or negative (i.e. when reproduction number is low, the proportion of ring members who accept vaccination is high and vice versa)?",
                               c("Positive", "Negative"),
                               selected = NULL,
                               width = "80%"
                             )
                             
                           ),
                           
                           selectInput(
                             "is_corr_Ringvacc_react_Asc",
                             
                             "Do you think there is any correlation between the proportion of ring members who accept vaccination and case ascertainment? e.g. if case ascertainment is higher, the proportion of ring members who accept vaccination is also higher.",
                             
                             c("Not sure", "Yes", "No"),
                             selected = "Not sure",
                             width = "80%"
                           ),
                           
                           conditionalPanel(
                             condition = "input.is_corr_Ringvacc_react_Asc=='Yes'",
                             selectInput(
                               "corr_Ringvacc_react_Asc",
                               
                               "Do you think the correlation is positive (i.e. when case ascertainment is high, the proportion of ring members who accept vaccination is high and when case ascertainment is low, the proportion of ring members who accept vaccination is low) or negative (i.e. when case ascertainment is low, the proportion of ring members who accept vaccination is high and vice versa)?",
                               
                               c("Positive", "Negative"),
                               selected = NULL,
                               width = "80%"
                             )
                             
                           ),
                           
                           textAreaInput(
                             "source_Ringvacc_react",
                             "Please provide any context or sources that have guided your intuition:",
                             width = "80%"
                           )
                         ),
                         
                         layout_column_wrap(
                           1 / 2,
                           actionButton("previousRingvacc_react", "Previous"),
                           actionButton("nextRingvacc_react", "Next", class =
                                          "btn-primary")
                         )
                       )
                     ),
                     accordion_panel(
                       
                       "Delay from case ascertainment to ring vaccination",
                       
                       value = "Ringvacc_delay",
                       layout_sidebar(
                         sidebar = sidebar(
                           title = tags$h4("Delay from case ascertainment to ring vaccination"),
                           width = 300,
                           shiny::p(""),
                           shiny::p("Based on your knowledge and experience of recent Ebola outbreak vaccination campaigns:"),
                           
                           selectInput(
                             "answerRingvacc_delay",
                             
                             "Can you provide your intuition about the distribution of delays from case ascertainment to the start of ring vaccination?",
                             
                             c("No", "Yes")
                           ),
                           
                           conditionalPanel(
                             condition = "input.answerRingvacc_delay=='Yes'",
                             selectInput(
                               "Ringvacc_delay_shape",
                               
                               "What do you think the shape of the distribution of delays from case ascertainment to the start of ring vaccination is?",
                               
                               c("Uniform", "Normal", "Skewed")
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_delay_shape=='Uniform'",
                               sliderInput(
                                 "Ringvacc_delay_min",
                                 
                                 "What do you think the minimum delay from case ascertainment to the start of ring vaccination is?",
                                 
                                 min = 0,
                                 max = 15,
                                 value = 0,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_delay_shape=='Uniform'",
                               sliderInput(
                                 "Ringvacc_delay_max",
                                 
                                 "What do you think the maximum delay from case ascertainment to the start of ring vaccination is?",
                                 
                                 min = 0,
                                 max = 15,
                                 value = 15,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_delay_shape=='Normal'",
                               sliderInput(
                                 "Ringvacc_delay_mean",
                                 
                                 "What do you think the mean delay from case ascertainment to the start of ring vaccination is?",
                                 
                                 min = 0,
                                 max = 15,
                                 value = 7.5,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_delay_shape=='Normal'",
                               sliderInput(
                                 "Ringvacc_delay_sd",
                                 
                                 "What do you think the standard deviation of the delay from case ascertainment to the start of ring vaccination is?",
                                 
                                 min = 1,
                                 max = 10,
                                 value = 5,
                                 step = 0.1,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_delay_shape=='Normal'",
                               sliderInput(
                                 
                                 "Ringvacc_delay_min_norm",
                                 "What do you think the minimum delay from case ascertainment to the start of ring vaccination is?",
                                 min = 0,
                                 max = 15,
                                 value = 0,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_delay_shape=='Normal'",
                               sliderInput(
                                 "Ringvacc_delay_max_norm",
                                 "What do you think the maximum delay from case ascertainment to the start of ring vaccination is?",
                                 min = 0,
                                 max = 15,
                                 value = 15,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_delay_shape=='Skewed'",
                               sliderInput(
                                 "Ringvacc_delay_means",
                                 "What do you think the mean delay from case ascertainment to the start of ring vaccination is?",
                                 
                                 min = 0,
                                 max = 15,
                                 value = 7.5,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_delay_shape=='Skewed'",
                               sliderInput(
                                 "Ringvacc_delay_var",
                                 "What do you think the variance of the delay from case ascertainment to the start of ring vaccination is?",
                                 min = 1,
                                 max = 50,
                                 value = 25,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_delay_shape=='Skewed'",
                               sliderInput(
                                 "Ringvacc_delay_min_skewed",
                                 "hat do you think the minimum delay from case ascertainment to the start of ring vaccination is?",
                                 min = 0,
                                 max = 15,
                                 value = 0,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Ringvacc_delay_shape=='Skewed'",
                               sliderInput(
                                 "Ringvacc_delay_max_skewed",
                                 "What do you think the maximum delay from case ascertainment to the start of ring vaccination is?",
                                 min = 0,
                                 max = 15,
                                 value = 15,
                                 step = 0.5,
                                 round = -1
                               )
                             )
                           )
                           
                         ),
                         
                         conditionalPanel(
                           condition = "input.answerRingvacc_delay=='Yes'",
                           plotOutput("plotRingvacc_delay", width =
                                        "100%", height = '500px'),
                           textOutput("Ringvacc_delay_median"),
                           textOutput("Ringvacc_delay_conf"),
                           
                           selectInput(
                             "conf_Ringvacc_delay",
                             "How confident are you about the shape of the distribution?",
                             c("Very", "Somewhat", "Slightly", "Not very"),
                             width = "80%",
                             selected = "Not very"
                           ),
                           
                           selectInput(
                             "is_corr_Ringvacc_delay_R0",
                             "Do you think there is any correlation between the delay from case ascertainment to the start of ring vaccination and reproduction number? e.g. if the reproduction number is higher, the delay from case ascertainment to the start of ring vaccination is also higher.",
                             c("Not sure", "Yes", "No"),
                             selected = "Not sure",
                             width = "80%"
                           ),
                           
                           conditionalPanel(
                             condition = "input.is_corr_Ringvacc_delay_R0=='Yes'",
                             selectInput(
                               "corr_Ringvacc_delay_R0",
                               "Do you think the correlation is positive (i.e. when reproduction number is high, the delay from case ascertainment to the start of ring vaccination is high and when reproduction number is low, the delay from case ascertainment to the start of ring vaccination is low) or negative (i.e. when reproduction number is low, the delay from case ascertainment to the start of ring vaccination is high and vice versa)?",
                               c("Positive", "Negative"),
                               selected = NULL,
                               width = "80%"
                             )
                             
                           ),
                           
                           selectInput(
                             "is_corr_Ringvacc_delay_Asc",
                             "Do you think there is any correlation between the delay to start vaccination and case ascertainment? e.g. if case ascertainment is higher, the delay from case ascertainment to the start of ring vaccination is also higher.",
                             c("Not sure", "Yes", "No"),
                             selected = "Not sure",
                             width = "80%"
                           ),
                           
                           conditionalPanel(
                             condition = "input.is_corr_Ringvacc_delay_Asc=='Yes'",
                             selectInput(
                               "corr_Ringvacc_delay_Asc",
                               "Do you think the correlation is positive (i.e. when case ascertainment is high, the delay from case ascertainment to the start of ring vaccination is high and when case ascertainment is low, the delay from case ascertainment to the start of ring vaccination is low) or negative (i.e. when case ascertainment is low, the delay to start HCW vaccination is high and vice versa)?",
                               c("Positive", "Negative"),
                               selected = NULL,
                               width = "80%"
                             )
                             
                           ),
                           
                           textAreaInput(
                             "source_Ringvacc_delay",
                             "Please provide any context or sources that have guided your intuition:",
                             width = "80%"
                           )
                         ),
                         
                         layout_column_wrap(
                           1 / 2,
                           actionButton("previousRingvacc_delay", "Previous"),
                           actionButton("nextRingvacc_delay", "Next", class =
                                          "btn-primary")
                         )
                       )
                     )
                   )


  ),


# Geographically targeted vaccination -------------------------------------


  
  bslib::nav_panel(title="8",

                   shiny::p(tags$h3("Geographically targeted vaccination")),

                   shiny::p("Reactive vaccination campaigns carried out during EVD outbreaks target both healthcare workers (HCWs) and frontline workers (FLWs), and at-risk contacts of cases. The
                     latter is typically triggered by the ascertainment of a case and can be carried out using ring vaccination or geographically targeted vaccination. We are interested in
                     vaccine uptake for these different strategies, as well as the time taken to initiate vaccination following ascertainment of a case."),
                   
                   accordion(
                     id = "Geovacc",
                     
                     accordion_panel(
                       
                       "Proportion of community members who accept geographically targeted vaccination",
                       value = "Geovacc_react",
                       layout_sidebar(
                         sidebar = sidebar(
                           title = tags$h4("Proportion of community members who accept geographically targeted vaccination"),
                           width = 300,
                           shiny::p(""),
                           shiny::p("Based on your knowledge and experience of recent Ebola outbreak vaccination campaigns:"),
                           
                           selectInput(
                             "answerGeovacc_react",
                             
                             "Can you provide your intuition about the proportion of community members who accept geographically targeted vaccination?",
                             
                             c("No", "Yes")
                           ),
                           
                           conditionalPanel(
                             condition = "input.answerGeovacc_react=='Yes'",
                             selectInput(
                               "Geovacc_react_shape",
                               
                               "What do you think the shape of the distribution of the proportion of community members who accept geographically targeted vaccination is?",
                               c("Uniform", "Normal", "Beta")
                               
                             ),
                             
                             conditionalPanel(
                               condition = "input.Geovacc_react_shape=='Uniform'",
                               sliderInput(
                                 "Geovacc_react_min",
                                 
                                 "What do you think the minimum value of the proportion of community members who accept geographically targeted vaccination is?",
                                 
                                 min = 0,
                                 max = 1,
                                 value = 0,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Geovacc_react_shape=='Uniform'",
                               sliderInput(
                                 "Geovacc_react_max",
                                 
                                 "What do you think the maximum value of the proportion of community members who accept geographically targeted vaccination is?",
                                 
                                 min = 0,
                                 max = 1,
                                 value = 1,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Geovacc_react_shape=='Normal'",
                               sliderInput(
                                 "Geovacc_react_mean",
                                 
                                 "What do you think the mean value of the proportion of community members who accept geographically targeted vaccination is?",
                                 
                                 min = 0,
                                 max = 1,
                                 value = 0.5,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Geovacc_react_shape=='Normal'",
                               sliderInput(
                                 "Geovacc_react_sd",
                                 
                                 "What do you think the standard deviation of the proportion of community members who accept geographically targeted vaccination is?",
                                 
                                 min = 0.1,
                                 max = 1,
                                 value = 0.5,
                                 step = 0.01,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               
                               condition = "input.Geovacc_react_shape=='Normal'",
                               sliderInput(
                                 "Geovacc_react_min_norm",
                                 "What do you think the minimum value of the proportion of community members who accept geographically targeted vaccination is?",
                                 min = 0,
                                 max = 1,
                                 value = 0,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Geovacc_react_shape=='Normal'",
                               sliderInput(
                                 "Geovacc_react_max_norm",
                                 "What do you think the maximum value of the proportion of community members who accept geographically targeted vaccination is?",
                                 min = 0,
                                 max = 1,
                                 value = 1,
                                 step = 0.05,
                                 round = -2
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Geovacc_react_shape=='Beta'",
                               sliderInput(
                                 "Geovacc_react_means",
                                 "What do you think the mean value of the proportion of community members who accept geographically targeted vaccination is?",min=0.01,max=0.99,value=0.5,step=0.05,round=-2)
                             ),
                             
                             conditionalPanel(
                               
                               condition = "input.Geovacc_react_shape=='Beta'",
                               sliderInput(
                                 "Geovacc_react_betasd",
                                 "What do you think the variance of the proportion of community members who accept geographically targeted vaccination is?",
                                 
                                 min = 0.01,
                                 max = 0.25,
                                 value = 0.1,
                                 step = 0.001,
                                 round = -3
                               )
                             )
                           ),
                           
                           conditionalPanel(
                             condition = "input.Geovacc_react_shape=='Beta'",
                             sliderInput(
                               "Geovacc_react_min_beta",
                               "What do you think the minimum value of the proportion of community members who accept geographically targeted vaccination is?",
                               min = 0,
                               max = 1,
                               value = 0,
                               step = 0.05,
                               round = -2
                             )
                           ),
                           
                           conditionalPanel(
                             condition = "input.Geovacc_react_shape=='Beta'",
                             sliderInput(
                               "Geovacc_react_max_beta",
                               "What do you think the maximum value of the proportion of community members who accept geographically targeted vaccination is?",
                               min = 0,
                               max = 1,
                               value = 1,
                               step = 0.05,
                               round = -2
                             )
                           )
                           
                         ),
                         
                         conditionalPanel(
                           
                           condition = "input.answerGeovacc_react=='Yes'",
                           plotOutput("plotGeovacc_react", width =
                                        "100%", height = '500px'),
                           textOutput("Geovacc_reactmedian"),
                           textOutput("Geovacc_reactconf"),
                           
                           selectInput(
                             "conf_Geovacc_react",
                             "How confident are you about the shape of the distribution?",
                             c("Very", "Somewhat", "Slightly", "Not very"),
                             width = "80%",
                             selected = "Not very"
                           ),
                           
                           selectInput(
                             "is_corr_Geovacc_react_R0",
                             
                             "Do you think there is any correlation between the proportion of community members who accept geographically targeted vaccination and reproduction number? e.g. if the reproduction number is higher, the proportion of community members who accept geographically targeted vaccination is also higher.",
                             
                             c("Not sure", "Yes", "No"),
                             selected = "Not sure",
                             width = "80%"
                           ),
                           
                           conditionalPanel(
                             condition = "input.is_corr_Geovacc_react_R0=='Yes'",
                             selectInput(
                               "corr_Geovacc_react_R0",
                               
                               "Do you think the correlation is positive (i.e. when reproduction number is high, the proportion of community members who accept geographically targeted vaccination is high and when reproduction number is low, the proportion of community members who accept geographically targeted vaccination is low) or negative (i.e. when reproduction number is low, the proportion of community members who accept geographically targeted vaccination is high and vice versa)?",
                               c("Positive", "Negative"),
                               selected = NULL,
                               width = "80%"
                             )
                             
                           ),
                           
                           selectInput(
                             "is_corr_Geovacc_react_Asc",
                             
                             "Do you think there is any correlation between the proportion of community members who accept geographically targeted vaccination and case ascertainment? e.g. if case ascertainment is higher, the proportion of community members who accept geographically targeted vaccination is also higher.",
                             
                             c("Not sure", "Yes", "No"),
                             selected = "Not sure",
                             width = "80%"
                           ),
                           
                           conditionalPanel(
                             condition = "input.is_corr_Geovacc_react_Asc=='Yes'",
                             selectInput(
                               "corr_Geovacc_react_Asc",
                               
                               "Do you think the correlation is positive (i.e. when case ascertainment is high, the proportion of community members who accept geographically targeted vaccination is high and when case ascertainment is low, the proportion of community members who accept geographically targeted vaccination is low) or negative (i.e. when case ascertainment is low, the proportion of community members who accept geographically targeted vaccination is high and vice versa)?",
                               
                               c("Positive", "Negative"),
                               selected = NULL,
                               width = "80%"
                             )
                             
                           ),
                           
                           textAreaInput(
                             "source_Geovacc_react",
                             "Please provide any context or sources that have guided your intuition:",
                             width = "80%"
                           )
                         ),
                         
                         layout_column_wrap(
                           1 / 2,
                           actionButton("previousGeovacc_react", "Previous"),
                           actionButton("nextGeovacc_react", "Next", class =
                                          "btn-primary")
                         )
                       )
                     ),
                     accordion_panel(
                       
                       "Delay from case ascertainment to geographical vaccination",
                       
                       value = "Geovacc_delay",
                       layout_sidebar(
                         sidebar = sidebar(
                           title = tags$h4("Delay from case ascertainment to the start of geographically targeted vaccination"),
                           width = 300,
                           shiny::p(""),
                           shiny::p("Based on your knowledge and experience of recent Ebola outbreak vaccination campaigns:"),
                           
                           selectInput(
                             "answerGeovacc_delay",
                             
                             "Can you provide your intuition about the distribution of delays from case ascertainment to the start of geographically targeted vaccination?",
                             
                             c("No", "Yes")
                           ),
                           
                           conditionalPanel(
                             condition = "input.answerGeovacc_delay=='Yes'",
                             selectInput(
                               "Geovacc_delay_shape",
                               
                               "What do you think the shape of the distribution of delays from case ascertainment to the start of geographically targeted vaccination is?",
                               
                               c("Uniform", "Normal", "Skewed")
                             ),
                             
                             conditionalPanel(
                               condition = "input.Geovacc_delay_shape=='Uniform'",
                               sliderInput(
                                 "Geovacc_delay_min",
                                 
                                 "What do you think the minimum delay from case ascertainment to the start of geographically targeted vaccination is?",
                                 
                                 min = 0,
                                 max = 15,
                                 value = 0,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Geovacc_delay_shape=='Uniform'",
                               sliderInput(
                                 "Geovacc_delay_max",
                                 
                                 "What do you think the maximum delay from case ascertainment to the start of geographically targeted vaccination is?",
                                 
                                 min = 0,
                                 max = 15,
                                 value = 15,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Geovacc_delay_shape=='Normal'",
                               sliderInput(
                                 "Geovacc_delay_mean",
                                 
                                 "What do you think the mean delay from case ascertainment to the start of geographically targeted vaccination is?",
                                 
                                 min = 0,
                                 max = 15,
                                 value = 7.5,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Geovacc_delay_shape=='Normal'",
                               sliderInput(
                                 "Geovacc_delay_sd",
                                 
                                 "What do you think the standard deviation of the delay from case ascertainment to the start of geographically targeted vaccination is?",
                                 
                                 min = 1,
                                 max = 10,
                                 value = 5,
                                 step = 0.1,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Geovacc_delay_shape=='Normal'",
                               sliderInput(
                                 
                                 "Geovacc_delay_min_norm",
                                 "What do you think the minimum delay from case ascertainment to the start of geographically targeted vaccination is?",
                                 min = 0,
                                 max = 15,
                                 value = 0,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Geovacc_delay_shape=='Normal'",
                               sliderInput(
                                 "Geovacc_delay_max_norm",
                                 "What do you think the maximum delay from case ascertainment to the start of geographically targeted vaccination is?",
                                 min = 0,
                                 max = 15,
                                 value = 15,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Geovacc_delay_shape=='Skewed'",
                               sliderInput(
                                 "Geovacc_delay_means",
                                 "What do you think the mean delay from case ascertainment to the start of geographically targeted vaccination is?",
                                 
                                 min = 0,
                                 max = 15,
                                 value = 7.5,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Geovacc_delay_shape=='Skewed'",
                               sliderInput(
                                 "Geovacc_delay_var",
                                 "What do you think the variance of the delay from case ascertainment to the start of geographically targeted vaccination is?",
                                 min = 1,
                                 max = 50,
                                 value = 25,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Geovacc_delay_shape=='Skewed'",
                               sliderInput(
                                 "Geovacc_delay_min_skewed",
                                 "hat do you think the minimum delay from case ascertainment to the start of geographically targeted vaccination is?",
                                 min = 0,
                                 max = 15,
                                 value = 0,
                                 step = 0.5,
                                 round = -1
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.Geovacc_delay_shape=='Skewed'",
                               sliderInput(
                                 "Geovacc_delay_max_skewed",
                                 "What do you think the maximum delay from case ascertainment to the start of geographically targeted vaccination is?",
                                 min = 0,
                                 max = 15,
                                 value = 15,
                                 step = 0.5,
                                 round = -1
                               )
                             )
                           )
                           
                         ),
                         
                         conditionalPanel(
                           condition = "input.answerGeovacc_delay=='Yes'",
                           plotOutput("plotGeovacc_delay", width =
                                        "100%", height = '500px'),
                           textOutput("Geovacc_delay_median"),
                           textOutput("Geovacc_delay_conf"),
                           
                           selectInput(
                             "conf_Geovacc_delay",
                             "How confident are you about the shape of the distribution?",
                             c("Very", "Somewhat", "Slightly", "Not very"),
                             width = "80%",
                             selected = "Not very"
                           ),
                           
                           selectInput(
                             "is_corr_Geovacc_delay_R0",
                             "Do you think there is any correlation between the delay from case ascertainment to the start of geographically targeted vaccination and reproduction number? e.g. if the reproduction number is higher, the delay from case ascertainment to the start of geographically targeted vaccination is also higher.",
                             c("Not sure", "Yes", "No"),
                             selected = "Not sure",
                             width = "80%"
                           ),
                           
                           conditionalPanel(
                             condition = "input.is_corr_Geovacc_delay_R0=='Yes'",
                             selectInput(
                               "corr_Geovacc_delay_R0",
                               "Do you think the correlation is positive (i.e. when reproduction number is high, the delay from case ascertainment to the start of geographically targeted vaccination is high and when reproduction number is low, the delay from case ascertainment to the start of geographically targeted vaccination is low) or negative (i.e. when reproduction number is low, the delay from case ascertainment to the start of geographically targeted vaccination is high and vice versa)?",
                               c("Positive", "Negative"),
                               selected = NULL,
                               width = "80%"
                             )
                             
                           ),
                           
                           selectInput(
                             "is_corr_Geovacc_delay_Asc",
                             "Do you think there is any correlation between the delay to start vaccination and case ascertainment? e.g. if case ascertainment is higher, the delay from case ascertainment to the start of geographically targeted vaccination is also higher.",
                             c("Not sure", "Yes", "No"),
                             selected = "Not sure",
                             width = "80%"
                           ),
                           
                           conditionalPanel(
                             condition = "input.is_corr_Geovacc_delay_Asc=='Yes'",
                             selectInput(
                               "corr_Geovacc_delay_Asc",
                               "Do you think the correlation is positive (i.e. when case ascertainment is high, the delay from case ascertainment to the start of geographically targeted vaccination is high and when case ascertainment is low, the delay from case ascertainment to the start of geographically targeted vaccination is low) or negative (i.e. when case ascertainment is low, the delay to start HCW vaccination is high and vice versa)?",
                               c("Positive", "Negative"),
                               selected = NULL,
                               width = "80%"
                             )
                             
                           ),
                           
                           textAreaInput(
                             "source_Geovacc_delay",
                             "Please provide any context or sources that have guided your intuition:",
                             width = "80%"
                           )
                         ),
                         
                         layout_column_wrap(
                           1 / 2,
                           actionButton("previousGeovacc_delay", "Previous"),
                           actionButton("nextGeovacc_delay", "Next", class =
                                          "btn-primary")
                         )
                       )
                     )
                   )


  ),


# Stockpile opinions ------------------------------------------------------


  
  bslib::nav_panel(title="9",
                   
                   shiny::p(tags$h3("Your views about EVD outbreaks and the role of the stockpile")),
                   
                   shiny::p("On the final page of the survey, we'd like you to give your views regarding the deployment of vaccines in future EVD outbreaks, the potential worst-case EVD outbreaks that we should
                   be planning for, and the purpose of the stockpile"),
                   
                   textInput( 
                     "vacc_doses",
                     "Before licensing, the Ervebo vaccine had to be administered by GCP-trained vaccinators, potentially limiting the number of vaccination teams and the number of 
                     doses administered by each team. Now that the requirement for GCP-trained vaccinators is no longer in place, what do you think is a reasonable upper bound on the
                     number of doses a vaccination team can adminster per day?",placeholder="e.g. 100",
                     width = "80%"
                   ),
                   
                   textInput( 
                     "vacc_teams",
                     "And what do you think is a reasonable upper bound on the number of vaccination teams that are sent as part of the outbreak response?",placeholder="e.g. 50",
                     width = "80%"
                   ),
                   
                   textAreaInput( 
                     "worst_case",
                     "In your opinion, what is the worst-case outbreak that we should be preparing for? How likely do
                     you think the worst-case outbreak is? Are current response activities sufficient to respond to 
                     such an outbreak, and if not, what other resources or interventions would be needed?",
                     width = "80%"
                    ),
                   
                   textAreaInput(
                     "stockpile",
                     "In your opinion, what do you think the role of the Ervebo vaccine stockpile should be? Should it be 
                     sufficient for the worst-case scenario? Should it be sufficient for 95% of future outbreaks? Should
                     it be sufficient for the whole outbreak length, or just the length of time it takes to ramp up vaccine production?",
                     width = "80%"
                   ),
                   
                   
                   layout_column_wrap(1/2,
                                      actionButton("previousStockpile","Previous"),
                                      actionButton("submit","Submit",class="btn-success")
                   )
                   
                   
  ),
  

# Thank you page ----------------------------------------------------------



  bslib::nav_panel(title="End",
                   shiny::p(tags$h3("Many thanks for your input!")),
                   
                   shiny::p("For more information on this survey, please contact: Anne Cori (anne.cori@imperial.ac.uk), Katharina Hauck (k.hauck@imperial.ac.uk) or 
      Gemma Nedjati-Gilani (g.nedjati-gilani@imperial.ac.uk)"),
                   
                   shiny::p("Survey design: Gemma Nedjati-Gilani and Rob Johnson"),
                   
                   
                   
  ),
  
  nav_spacer(),
  
 
)

# Define server logic required to plot the output
server <- function(input, output, session) {
  
  xmin<-0
  xmax<-10
  xmaxUnit<-1
  xmaxDelay <-15
  xminDT<-10
  xmaxDT<-40
  qrt<-c(0,0.025,0.25,0.5,0.75,0.975,1)
  text_size = 20
  breaks10 = 0:xmax
  breaksunit = seq(0,1,length=11)
  breaksDT=seq(xminDT,xmaxDT,by=2)
  breaksDelay=seq(xmin,xmaxDelay,by=1)
  
  ## functions to handle distributions
  # this function gets beta parameters from the mean and sd
  # note that there is an upper bound to sd given the mean

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
    Alpha <- (betamean^2 * (1-betamean) - betamean*var) / var
    Beta <- Alpha * (1 - betamean) / betamean
    c(Alpha, Beta)
  }
  
  # this function updates the beta distribution sd if the mean changes to a more extreme value
  # it prevents accumulating mass in the tails, which happens when one of the parameters is less than 1
  update_beta_dist <- function(betasd, betamean, sd_to_update){
    input_var <- betasd^2
    # by definition we require
    # mean*(1 - mean) > variance
    if(betamean * (1-betamean) < input_var){
      input_var <- betamean * (1-betamean)
      max_sd = round(sqrt(input_var), 3)
      updateSliderInput(session, sd_to_update, value = max_sd)
    }
    beta_pars <- get_beta_parameters(betamean, round(sqrt(input_var), 3))
    Alpha <- beta_pars[1]
    Beta <- beta_pars[2]
    # normalise alpha and beta so that both are at least 1
    if(min(Alpha,Beta)<1){
      minab <- min(Alpha,Beta)
      Alpha <- Alpha/minab
      Beta <- Beta/minab
      implied_sd = round(sqrt(Alpha*Beta / ((Alpha + Beta)^2 * (Alpha + Beta + 1))), 3)
      updateSliderInput(session, sd_to_update, value = implied_sd)
    }
  }
  
  # this function gets shape and scale parameters from gamma mean and variance
  get_gamma_dist <- function(gammamean, gammavar, min_skew, max_skew){
    Shape <- gammamean^2/gammavar
    Scale <- gammavar/gammamean
    Truncate(Gammad(shape = Shape, scale = Scale), lower=min_skew, upper=max_skew)
  }
  
  ## store distributions
  v <- reactiveValues(R0_dist = NULL, 
                      DT_dist = NULL,
                      asc_dist = NULL, 
                      HCWvacc_prevent_dist = NULL)
  

# Overview - server----------------------------------------------------------------
  
  observeEvent(input$nextOverview,{
    updateNavbarPage(session=session,"mainpage",selected="1")
  })
  

# Experience - server -----------------------------------------------------

  
  observeEvent(input$previousExp,{
    updateNavbarPage(session=session,"mainpage",selected="Overview")
  })
  
  observeEvent(input$nextExp,{
    updateNavbarPage(session=session,"mainpage",selected="2")
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
      R0_dist = distr::Unif(Min=input$R0_min,Max=input$R0_max)
    }
    else if(plotTypeR0()=="Normal"){
      R0_dist = distr::Truncate(distr::Norm(mean=input$R0_mean,sd=input$R0_sd),lower=input$R0_min_norm,upper=input$R0_max_norm)
    }
    else if(plotTypeR0()=="Skewed"){
      #then make these into gamma or beta distribution parameters
      R0_dist <- get_gamma_dist(input$R0_means, input$R0_var, input$R0_min_skew, input$R0_max_skew)
    }
    dat <- data.frame(xpos=seq(xmin,xmax,by=0.01))
    v$R0_dist = R0_dist
    dat$ypos <- distr::d(R0_dist)(dat$xpos)
    dat$qt  <- cut(distr::p(R0_dist)(dat$xpos),breaks=qrt,labels=F) 
    
    ggplot(dat,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="R0",y="pdf",color="Percentile",title="Probability density of R0")+
      theme_gray(base_size = text_size)+theme(legend.position ="none") + 
      scale_x_continuous(breaks=breaks10)
  }
  )
  
  output$R0conf<-renderText({
    R0_dist = v$R0_dist
    lower50 <- distr::q(R0_dist)(0.25)
    upper50 <- distr::q(R0_dist)(0.75) 
    lower95 <- distr::q(R0_dist)(0.025) 
    upper95 <- distr::q(R0_dist)(0.975) 
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$R0median<-renderText({
    R0_dist = v$R0_dist
    median <- distr::q(R0_dist)(0.5)
    paste("Your median value for R0 is:",round(median,digits=2))
  })
  
  observeEvent(input$previousR0,{
    updateNavbarPage(session=session,"mainpage",selected="1")
  })
  
  observeEvent(input$nextR0,{
    updateNavbarPage(session=session,"mainpage",selected="3")
  })
  
  ## doubling time ########################################################
  
  plotTypeDT <- reactive({input$DT_shape
  })
  
  observeEvent(input$DT_min,{
    updateSliderInput(session,"DT_max",min=input$DT_min+0.1)
  })
  
  observeEvent(input$DT_min_norm,{
    updateSliderInput(session,"DT_max_norm",min=input$DT_min_norm+0.1)
  })
  
  observeEvent(input$DT_min_skew,{
    updateSliderInput(session,"DT_max_skew",min=input$DT_min_skew+0.1)
  })
  
  output$plotDT <- renderPlot({
    if(plotTypeDT()=="Uniform"){
      DT_dist<-distr::Unif(Min=input$DT_min,Max=input$DT_max)
    }
    else if(plotTypeDT()=="Normal"){
      DT_dist<-distr::Truncate(distr::Norm(mean=input$DT_mean,sd=input$DT_sd),lower=input$DT_min_norm,upper=input$DT_max_norm)
      }
    else if(plotTypeDT()=="Skewed"){
      #then make these into gamma distribution parameters
      DT_dist <- get_gamma_dist(input$DT_means, input$DT_var, input$DT_min_skew, input$DT_max_skew)
    }
    
    dat <- data.frame(xpos=seq(xminDT,xmaxDT,by=0.01))
    v$DT_dist = DT_dist
    dat$ypos <- distr::d(DT_dist)(dat$xpos)
    dat$qt  <- cut(distr::p(DT_dist)(dat$xpos),breaks=qrt,labels=F) 
    
    ggplot(dat,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="Doubling time (days)",y="pdf",color="Percentile",title="Probability density of doubling time (days)")+
      theme_gray(base_size = text_size)+theme(legend.position ="none") + 
      scale_x_continuous(breaks=breaksDT)

  }
  )
  
  output$DTconf<-renderText({
    DT_dist = v$DT_dist
    lower50 <- distr::q(DT_dist)(0.25)
    upper50 <- distr::q(DT_dist)(0.75) 
    lower95 <- distr::q(DT_dist)(0.025) 
    upper95 <- distr::q(DT_dist)(0.975)
    paste("Your 50% confidence interval is:",round(lower50,digits=1),"-",round(upper50,digits=1), "days and your 95%
          confidence interval is:",round(lower95,digits=1),"-",round(upper95,digits=1),"days")
  })
  
  output$DTmedian<-renderText({
    DT_dist = v$DT_dist
    median <- distr::q(DT_dist)(0.5)
    paste("Your median value for doubling time is:",round(median,digits=1),"days")
  })
  
  
  observeEvent(input$previousDT,{
    updateNavbarPage(session=session,"mainpage",selected="2")
  })
  
  observeEvent(input$nextDT,{
    updateNavbarPage(session=session,"mainpage",selected="4")
  })
  
  ## case ascertainment ###################################################################################
  
  observeEvent(input$Asc_min,{
    updateSliderInput(session,"Asc_max",min=input$Asc_min+0.1)
  })
  
  observeEvent(input$Asc_min_norm,{
    updateSliderInput(session,"Asc_max_norm",min=input$Asc_min_norm+0.1)
  })
  
  observeEvent(input$Asc_min_beta,{
    updateSliderInput(session,"Asc_max_beta",min=input$Asc_min_norm+0.1)
  })
  
  plotTypeAsc <- reactive({input$Asc_shape
  })
  
  observeEvent(input$Asc_means, {
    # If the beta mean changes, compute the new implied standard deviation
    update_beta_dist(input$Asc_betasd, input$Asc_means, "Asc_betasd")
  })
  
  output$plotAsc <- renderPlot({
    datAsc <- data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
    if(plotTypeAsc()=="Uniform"){
      asc_dist = distr::Unif(Min=input$Asc_min,Max=input$Asc_max)
    }
    else if(plotTypeAsc()=="Normal"){
      asc_dist = distr::Truncate(distr::Norm(mean=input$Asc_mean,sd=input$Asc_sd),lower=input$Asc_min_norm,upper=input$Asc_max_norm)
    }
    else if(plotTypeAsc()=="Skewed"){
      asc_dist <- get_gamma_dist(input$Asc_means, input$Asc_var, 0, 1)
    }
    else if(plotTypeAsc()=="Beta"){
      #then make these into beta distribution parameters
      beta_pars <- get_beta_parameters(input$Asc_means, input$Asc_betasd)
      datAsc <- subset(datAsc,xpos*(1-xpos)!=0)
      asc_dist = distr::Truncate(distr::Beta(shape1 = beta_pars[1], shape2 = beta_pars[2]),lower=input$Asc_min_beta,upper=input$Asc_max_beta)
    }
    v$asc_dist = asc_dist
    datAsc$ypos <- distr::d(asc_dist)(datAsc$xpos)
    datAsc$qt  <- cut(distr::p(asc_dist)(datAsc$xpos),breaks=qrt,labels=F) 
    
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
    upper50 <- distr::q(asc_dist)(0.75) 
    lower95 <- distr::q(asc_dist)(0.025) 
    upper95 <- distr::q(asc_dist)(0.975) 
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$Ascmedian<-renderText({
    asc_dist = v$asc_dist
    median <- distr::q(asc_dist)(0.5)
    paste("Your median value for case ascertainment is:",round(median,digits=2))
  })
  
  observeEvent(input$previousAsc,{
    updateNavbarPage(session=session,"mainpage",selected="3")
  })
  
  observeEvent(input$nextAsc,{
    updateNavbarPage(session=session,"mainpage",selected="5")
  })
  
  ## proportion of contacts traced ##############################################################
  
  observeEvent(input$CTprop_min,{
    updateSliderInput(session,"CTprop_max",min=input$CTprop_min+0.1)
  })
  
  observeEvent(input$CTprop_min_norm,{
    updateSliderInput(session,"CTprop_max_norm",min=input$CTprop_min_norm+0.1)
  })
  
  observeEvent(input$CTprop_min_beta,{
    updateSliderInput(session,"CTprop_max_beta",min=input$CTprop_min_norm+0.1)
  })
  
  plotTypeCTprop <- reactive({input$CTprop_shape
  })
  
  observeEvent(input$CTprop_means, {
    # If the beta mean changes, compute the new implied standard deviation
    update_beta_dist(input$CTprop_betasd, input$CTprop_means, "CTprop_betasd")
  })
  
  output$plotCTprop <- renderPlot({
    datCTprop <- data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
    if(plotTypeCTprop()=="Uniform"){
      CTprop_dist = distr::Unif(Min=input$CTprop_min,Max=input$CTprop_max)
    }
    else if(plotTypeCTprop()=="Normal"){
      CTprop_dist = distr::Truncate(distr::Norm(mean=input$CTprop_mean,sd=input$CTprop_sd),lower=input$CTprop_min_norm,upper=input$CTprop_max_norm)
    }
    else if(plotTypeCTprop()=="Skewed"){
      CTprop_dist <- get_gamma_dist(input$CTprop_means, input$CTprop_var, 0, 1)
    }
    else if(plotTypeCTprop()=="Beta"){
      #then make these into beta distribution parameters
      beta_pars <- get_beta_parameters(input$CTprop_means, input$CTprop_betasd)
      datCTprop <- subset(datCTprop,xpos*(1-xpos)!=0)
      CTprop_dist = distr::Truncate(distr::Beta(shape1 = beta_pars[1], shape2 = beta_pars[2]),lower=input$CTprop_min_beta,upper=input$CTprop_max_beta)
    }
    v$CTprop_dist = CTprop_dist
    datCTprop$ypos <- distr::d(CTprop_dist)(datCTprop$xpos)
    datCTprop$qt  <- cut(distr::p(CTprop_dist)(datCTprop$xpos),breaks=qrt,labels=F) 
    
    ggplot(datCTprop,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="Proportion of contacts traced",y="pdf",color="Percentile",title="Probability density of contact tracing proportion")+
      theme_gray(base_size = text_size)+theme(legend.position ="none") + 
      scale_x_continuous(breaks=breaksunit)
  }
  )
  
  output$CTpropconf<-renderText({
    CTprop_dist = v$CTprop_dist
    lower50 <- distr::q(CTprop_dist)(0.25)
    upper50 <- distr::q(CTprop_dist)(0.75) 
    lower95 <- distr::q(CTprop_dist)(0.025) 
    upper95 <- distr::q(CTprop_dist)(0.975) 
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$CTpropmedian<-renderText({
    CTprop_dist = v$CTprop_dist
    median <- distr::q(CTprop_dist)(0.5)
    paste("Your median value for the proportion of contacts traced is:",round(median,digits=2))
  })
  
  observeEvent(input$previousCTprop,{
    updateNavbarPage(session=session,"mainpage",selected="4")
  })
  
  observeEvent(input$nextCTprop,{
    accordion_panel_close(session=session,id="CT",values = "CTprop")
    accordion_panel_open(session=session,id="CT",values="CTfollow")
  })
  
  ## proportion of contacts followed up #############################################################
  
  observeEvent(input$CTfoll_min,{
    updateSliderInput(session,"CTfoll_max",min=input$CTfoll_min+0.1)
  })
  
  observeEvent(input$CTfoll_min_norm,{
    updateSliderInput(session,"CTfoll_max_norm",min=input$CTfoll_min_norm+0.1)
  })
  
  observeEvent(input$CTfoll_min_beta,{
    updateSliderInput(session,"CTfoll_max_beta",min=input$CTfoll_min_norm+0.1)
  })
  
  plotTypeCTfoll <- reactive({input$CTfoll_shape
  })
  
  observeEvent(input$CTfoll_means, {
    # If the beta mean changes, compute the new implied standard deviation
    update_beta_dist(input$CTfoll_betasd, input$CTfoll_means, "CTfoll_betasd")
  })
  
  output$plotCTfoll <- renderPlot({
    datCTfoll <- data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
    if(plotTypeCTfoll()=="Uniform"){
      CTfoll_dist = distr::Unif(Min=input$CTfoll_min,Max=input$CTfoll_max)
    }
    else if(plotTypeCTfoll()=="Normal"){
      CTfoll_dist = distr::Truncate(distr::Norm(mean=input$CTfoll_mean,sd=input$CTfoll_sd),lower=input$CTfoll_min_norm,upper=input$CTfoll_max_norm)
    }
    else if(plotTypeCTfoll()=="Skewed"){
      CTfoll_dist <- get_gamma_dist(input$CTfoll_means, input$CTfoll_var, 0, 1)
    }
    else if(plotTypeCTfoll()=="Beta"){
      #then make these into beta distribution parameters
      beta_pars <- get_beta_parameters(input$CTfoll_means, input$CTfoll_betasd)
      datCTfoll <- subset(datCTfoll,xpos*(1-xpos)!=0)
      CTfoll_dist = distr::Truncate(distr::Beta(shape1 = beta_pars[1], shape2 = beta_pars[2]),lower=input$CTfoll_min_beta,upper=input$CTfoll_max_beta)
    }
    v$CTfoll_dist = CTfoll_dist
    datCTfoll$ypos <- distr::d(CTfoll_dist)(datCTfoll$xpos)
    datCTfoll$qt  <- cut(distr::p(CTfoll_dist)(datCTfoll$xpos),breaks=qrt,labels=F) 
    
    ggplot(datCTfoll,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="Proportion of contacts who complete follow up",y="pdf",color="Percentile",title="Probability density of contact tracing completion")+
      theme_gray(base_size = text_size)+theme(legend.position ="none") + 
      scale_x_continuous(breaks=breaksunit)
  }
  )
  
  output$CTfollconf<-renderText({
    CTfoll_dist = v$CTfoll_dist
    lower50 <- distr::q(CTfoll_dist)(0.25)
    upper50 <- distr::q(CTfoll_dist)(0.75) 
    lower95 <- distr::q(CTfoll_dist)(0.025) 
    upper95 <- distr::q(CTfoll_dist)(0.975) 
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$CTfollmedian<-renderText({
    CTfoll_dist = v$CTfoll_dist
    median <- distr::q(CTfoll_dist)(0.5)
    paste("Your median value for the proportion of contacts who complete follow up is:",round(median,digits=2))
  })
  
  
  observeEvent(input$previousCTfollow,{
    accordion_panel_close(session=session,id="CT",values="CTfollow")
    accordion_panel_open(session=session,id="CT",values="CTprop")
  })
  
  observeEvent(input$nextCTfollow,{
    updateNavbarPage(session=session,"mainpage",selected="6")
  })
  
  ## HCW vaccination #############################################################

  observeEvent(input$HCWvacc_prevent_min,{
    updateSliderInput(session,"HCWvacc_prevent_max",min=input$HCWvacc_prevent_min+0.1)
  })
  
  observeEvent(input$HCWvacc_prevent_min_norm,{
    updateSliderInput(session,"HCWvacc_prevent_max_norm",min=input$HCWvacc_prevent_min_norm+0.1)
  })
  
  observeEvent(input$HCWvacc_prevent_min_beta,{
    updateSliderInput(session,"HCWvacc_prevent_max_beta",min=input$HCWvacc_prevent_min_beta+0.1)
  })
  
  plotTypeHCWvacc_prevent <- reactive({input$HCWvacc_prevent_shape
  })
  
  observeEvent(input$HCWvacc_prevent_means, {
    # If the beta mean changes, compute the new implied standard deviation
    update_beta_dist(input$HCWvacc_prevent_betasd, input$HCWvacc_prevent_means, "HCWvacc_prevent_betasd")
  })
  
  output$plotHCWvacc_prevent <- renderPlot({
    datHCWvacc_prevent <- data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
    if(plotTypeHCWvacc_prevent()=="Uniform"){
      HCWvacc_prevent_dist = distr::Unif(Min=input$HCWvacc_prevent_min,Max=input$HCWvacc_prevent_max)
    }
    else if(plotTypeHCWvacc_prevent()=="Normal"){
      HCWvacc_prevent_dist = distr::Truncate(distr::Norm(mean=input$HCWvacc_prevent_mean,sd=input$HCWvacc_prevent_sd),lower=input$HCWvacc_prevent_min_norm,upper=input$HCWvacc_prevent_max_norm)
    }
    else if(plotTypeHCWvacc_prevent()=="Skewed"){
      #then make these into gamma or beta distribution parameters
      HCWvacc_prevent_dist = get_gamma_dist(input$HCWvacc_prevent_means, input$HCWvacc_prevent_var, input$HCWvacc_prevent_min_skewed, input$HCWvacc_prevent_max_skewed)
    }
    else if(plotTypeHCWvacc_prevent()=="Beta"){
      #then make these into beta distribution parameters
      beta_pars <- get_beta_parameters(input$HCWvacc_prevent_means, input$HCWvacc_prevent_betasd)
      datHCWvacc_prevent <- subset(datHCWvacc_prevent,xpos*(1-xpos)!=0)
      HCWvacc_prevent_dist = distr::Truncate(distr::Beta(shape1 = beta_pars[1], shape2 = beta_pars[2]),input$HCWvacc_prevent_min_beta, input$HCWvacc_prevent_max_beta)
    }
    v$HCWvacc_prevent_dist = HCWvacc_prevent_dist
    datHCWvacc_prevent$ypos <- distr::d(HCWvacc_prevent_dist)(datHCWvacc_prevent$xpos)
    datHCWvacc_prevent$qt  <- cut(distr::p(HCWvacc_prevent_dist)(datHCWvacc_prevent$xpos),breaks=qrt,labels=F) #cut(pbeta(datHCWvacc_prevent$xpos,alpha=HCWvacc_preventShape,beta=HCWvacc_preventScale,log=F),breaks=qrt,labels=F)
    
    ggplot(datHCWvacc_prevent,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="Proportion of HCWs/FLWs accepting preventative vaccination",y="pdf",color="Percentile",title="Probability density of proportion of HCWs/FLWs accepting preventative vaccination")+
      theme_gray(base_size = text_size)+theme(legend.position ="none") + 
      scale_x_continuous(breaks=breaksunit)
  }
  )
  
  output$HCWvacc_prevent_conf<-renderText({
    HCWvacc_prevent_dist = v$HCWvacc_prevent_dist
    lower50 <- distr::q(HCWvacc_prevent_dist)(0.25)
    upper50 <- distr::q(HCWvacc_prevent_dist)(0.75) 
    lower95 <- distr::q(HCWvacc_prevent_dist)(0.025) 
    upper95 <- distr::q(HCWvacc_prevent_dist)(0.975)
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$HCWvacc_prevent_median<-renderText({
    HCWvacc_prevent_dist = v$HCWvacc_prevent_dist
    median <- distr::q(HCWvacc_prevent_dist)(0.5)
    paste("Your median value for the proportion of HCWs/FLWs who accept preventative vaccination is:",round(median,digits=2))
  })
 
  # For preventative vaccination
  observeEvent(input$previousHCWvacc_prevent,{
    updateNavbarPage(session=session,"mainpage",selected="5")
  })
  
  observeEvent(input$nextHCWvacc_prevent,{
    accordion_panel_close(session=session,id="HCWvacc",values="HCWvacc_prevent")
    accordion_panel_open(session=session,id="HCWvacc",values="HCWvacc_react")
  })
  
  observeEvent(input$HCWvacc_react_min_norm,{
    updateSliderInput(session,"HCWvacc_react_max_norm",min=input$HCWvacc_react_min_norm+0.1)
  })
  
  observeEvent(input$HCWvacc_react_min_beta,{
    updateSliderInput(session,"HCWvacc_react_max_beta",min=input$HCWvacc_react_min_beta+0.1)
  })
  
  observeEvent(input$HCWvacc_react_min,{
    updateSliderInput(session,"HCWvacc_react_max",min=input$HCWvacc_react_min+0.1)
  })
  
  plotTypeHCWvacc_react <- reactive({input$HCWvacc_react_shape
  })
  
  
  observeEvent(input$HCWvacc_react_means, {
    # If the beta mean changes, compute the new implied standard deviation
    HCWvacc_react_var <- input$HCWvacc_react_betasd^2
    # by definition we require
    # mean*(1 - mean) > variance
    if(input$HCWvacc_react_means * (1-input$HCWvacc_react_means) < HCWvacc_react_var){
      HCWvacc_react_var <- input$HCWvacc_react_means * (1-input$HCWvacc_react_means)
      max_sd = round(sqrt(HCWvacc_react_var), 3)
      updateSliderInput(session, "HCWvacc_react_betasd", value = max_sd)
    }
    beta_pars <- get_beta_parameters(input$HCWvacc_react_means, round(sqrt(HCWvacc_react_var), 3))
    HCWvacc_reactAlpha <- beta_pars[1]
    HCWvacc_reactBeta <- beta_pars[2]
    # normalise alpha and beta so that both are at least 1
    if(min(HCWvacc_reactAlpha,HCWvacc_reactBeta)<1){
      minab <- min(HCWvacc_reactAlpha,HCWvacc_reactBeta)
      HCWvacc_reactAlpha <- HCWvacc_reactAlpha/minab
      HCWvacc_reactBeta <- HCWvacc_reactBeta/minab
      implied_sd = round(sqrt(HCWvacc_reactAlpha*HCWvacc_reactBeta / ((HCWvacc_reactAlpha + HCWvacc_reactBeta)^2 * (HCWvacc_reactAlpha + HCWvacc_reactBeta + 1))), 3)
      updateSliderInput(session, "HCWvacc_react_betasd", value = implied_sd)
    }
  })
  
  output$plotHCWvacc_react <- renderPlot({
    datHCWvacc_react <- data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
    if(plotTypeHCWvacc_react()=="Uniform"){
      HCWvacc_react_dist = distr::Unif(Min=input$HCWvacc_react_min,Max=input$HCWvacc_react_max)
    }
    else if(plotTypeHCWvacc_react()=="Normal"){
      HCWvacc_react_dist = distr::Truncate(distr::Norm(mean=input$HCWvacc_react_mean,sd=input$HCWvacc_react_sd),lower=input$HCWvacc_react_min_norm,upper=input$HCWvacc_react_max_norm)
    }
    else if(plotTypeHCWvacc_react()=="Skewed"){
      #then make these into gamma or beta distribution parameters
      HCWvacc_reactShape<-(input$HCWvacc_react_means*input$HCWvacc_react_means)/input$HCWvacc_react_var
      HCWvacc_reactScale<-input$HCWvacc_react_var/input$HCWvacc_react_means
      HCWvacc_reactAlpha<-input$HCWvacc_react_means*(((input$HCWvacc_react_means*(1-input$HCWvacc_react_means))/input$HCWvacc_react_var)-1)
      HCWvacc_reactBeta<-(1-input$HCWvacc_react_means)*(((input$HCWvacc_react_means*(1-input$HCWvacc_react_means))/input$HCWvacc_react_var)-1)
      datHCWvacc_react<-data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
      HCWvacc_react_dist = distr::Gammad(shape1 = HCWvacc_reactAlpha, shape2 = HCWvacc_reactBeta)
    }
    else if(plotTypeHCWvacc_react()=="Beta"){
      #then make these into beta distribution parameters
      beta_pars <- get_beta_parameters(input$HCWvacc_react_means, input$HCWvacc_react_betasd)
      datHCWvacc_react <- subset(datHCWvacc_react,xpos*(1-xpos)!=0)
      # print(c(14, input$HCWvacc_react_means, input$HCWvacc_react_betasd, beta_pars))
      HCWvacc_react_dist = distr::Truncate(distr::Beta(shape1 = beta_pars[1], shape2 = beta_pars[2]),lower=input$HCWvacc_react_min_beta,upper=input$HCWvacc_react_max_beta)
      # if(sum(beta_pars<.99)>0) return(NULL) # cut if parameters have not been updated
    }
    v$HCWvacc_react_dist = HCWvacc_react_dist
    datHCWvacc_react$ypos <- distr::d(HCWvacc_react_dist)(datHCWvacc_react$xpos)
    datHCWvacc_react$qt  <- cut(distr::p(HCWvacc_react_dist)(datHCWvacc_react$xpos),breaks=qrt,labels=F) #cut(pbeta(datHCWvacc_react$xpos,alpha=HCWvacc_reactShape,beta=HCWvacc_reactScale,log=F),breaks=qrt,labels=F)
    
    ggplot(datHCWvacc_react,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="Proportion of HCWs/FLWs accepting reactive vaccination",y="pdf",color="Percentile",title="Probability density of proportion of HCWs/FLWs accepting reactive vaccination")+
      theme_gray(base_size = text_size)+theme(legend.position ="none") + 
      scale_x_continuous(breaks=breaksunit)
  }
  )
  
  output$HCWvacc_react_conf<-renderText({
    HCWvacc_react_dist = v$HCWvacc_react_dist
    lower50 <- distr::q(HCWvacc_react_dist)(0.25)
    upper50 <- distr::q(HCWvacc_react_dist)(0.75) # qbeta(p=0.75*pbeta(1,shape=HCWvacc_reactShape,scale=HCWvacc_reactScale),shape=HCWvacc_reactShape,scale=HCWvacc_reactScale)
    lower95 <- distr::q(HCWvacc_react_dist)(0.025) # qbeta(p=0.025*pbeta(1,shape=HCWvacc_reactShape,scale=HCWvacc_reactScale),shape=HCWvacc_reactShape,scale=HCWvacc_reactScale)
    upper95 <- distr::q(HCWvacc_react_dist)(0.975) # qbeta(p=0.975*pbeta(1,shape=HCWvacc_reactShape,scale=HCWvacc_reactScale),shape=HCWvacc_reactShape,scale=HCWvacc_reactScale)
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$HCWvacc_react_median<-renderText({
    HCWvacc_react_dist = v$HCWvacc_react_dist
    median <- distr::q(HCWvacc_react_dist)(0.5)
    paste("Your median value for the proportion of HCWs/FLWs who accept reactive vaccination is:",round(median,digits=2))
  })
  
  # For reactive vaccination
  observeEvent(input$previousHCWvacc_react,{
    accordion_panel_close(session=session,id="HCWvacc",values="HCWvacc_react")
    accordion_panel_open(session=session,id="HCWvacc",values="HCWvacc_react")
  })
  
  observeEvent(input$nextHCWvacc_react,{
    accordion_panel_close(session=session,id="HCWvacc",values="HCWvacc_react")
    accordion_panel_open(session=session,id="HCWvacc",values="HCWvacc_delay")
  })
  
  # For vaccination delay
  
  observeEvent(input$HCWvacc_delay_min,{
    updateSliderInput(session,"HCWvacc_delay_max",min=input$HCWvacc_delay_min+0.1)
  })
  
  observeEvent(input$HCWvacc_delay_min_skewed,{
    updateSliderInput(session,"HCWvacc_delay_max_skewed",min=input$HCWvacc_delay_min_skewed+0.1)
  })
  
  observeEvent(input$HCWvacc_delay_min_norm,{
    updateSliderInput(session,"HCWvacc_delay_max_norm",min=input$HCWvacc_delay_min_norm+0.1)
  })
  
  plotTypeHCWvacc_delay <- reactive({input$HCWvacc_delay_shape
  })
  
  output$plotHCWvacc_delay <- renderPlot({
    datHCWvacc_delay <- data.frame(xpos=seq(xmin,xmaxDelay,by=0.01))
    if(plotTypeHCWvacc_delay()=="Uniform"){
      HCWvacc_delay_dist = distr::Unif(Min=input$HCWvacc_delay_min,Max=input$HCWvacc_delay_max)
    }
    else if(plotTypeHCWvacc_delay()=="Normal"){
      HCWvacc_delay_dist = distr::Truncate(distr::Norm(mean=input$HCWvacc_delay_mean,sd=input$HCWvacc_delay_sd),lower=input$HCWvacc_delay_min_norm,upper=input$HCWvacc_delay_max_norm)
    }
    else if(plotTypeHCWvacc_delay()=="Skewed"){
      #then make these into gamma or beta distribution parameters
      HCWvacc_delayShape<-(input$HCWvacc_delay_means*input$HCWvacc_delay_means)/input$HCWvacc_delay_var
      HCWvacc_delayScale<-input$HCWvacc_delay_var/input$HCWvacc_delay_means
      # HCWvacc_delayAlpha<-input$HCWvacc_delay_means*(((input$HCWvacc_delay_means*(1-input$HCWvacc_delay_means))/input$HCWvacc_delay_var)-1)
      # HCWvacc_delayBeta<-(1-input$HCWvacc_delay_means)*(((input$HCWvacc_delay_means*(1-input$HCWvacc_delay_means))/input$HCWvacc_delay_var)-1)
      # datHCWvacc_delay<-data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
      HCWvacc_delay_dist = Truncate(Gammad(shape = HCWvacc_delayShape, scale=HCWvacc_delayScale),lower=input$HCWvacc_delay_min_skewed,upper=input$HCWvacc_delay_max_skewed)
      
    }
    v$HCWvacc_delay_dist = HCWvacc_delay_dist
    datHCWvacc_delay$ypos <- distr::d(HCWvacc_delay_dist)(datHCWvacc_delay$xpos)
    datHCWvacc_delay$qt  <- cut(distr::p(HCWvacc_delay_dist)(datHCWvacc_delay$xpos),breaks=qrt,labels=F) #cut(pbeta(datHCWvacc_delay$xpos,alpha=HCWvacc_delayShape,beta=HCWvacc_delayScale,log=F),breaks=qrt,labels=F)
    
    ggplot(datHCWvacc_delay,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="Delay to start HCW/FLW vaccination (days)",y="pdf",color="Percentile",title="Probability density of delay to start HCW/FLW vaccination")+
      theme_gray(base_size = text_size)+theme(legend.position ="none") + 
      scale_x_continuous(breaks=breaksDelay)
  }
  )
  
  output$HCWvacc_delay_conf<-renderText({
    HCWvacc_delay_dist = v$HCWvacc_delay_dist
    lower50 <- distr::q(HCWvacc_delay_dist)(0.25)
    upper50 <- distr::q(HCWvacc_delay_dist)(0.75) # qbeta(p=0.75*pbeta(1,shape=HCWvacc_delayShape,scale=HCWvacc_delayScale),shape=HCWvacc_delayShape,scale=HCWvacc_delayScale)
    lower95 <- distr::q(HCWvacc_delay_dist)(0.025) # qbeta(p=0.025*pbeta(1,shape=HCWvacc_delayShape,scale=HCWvacc_delayScale),shape=HCWvacc_delayShape,scale=HCWvacc_delayScale)
    upper95 <- distr::q(HCWvacc_delay_dist)(0.975) # qbeta(p=0.975*pbeta(1,shape=HCWvacc_delayShape,scale=HCWvacc_delayScale),shape=HCWvacc_delayShape,scale=HCWvacc_delayScale)
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$HCWvacc_delay_median<-renderText({
    HCWvacc_delay_dist = v$HCWvacc_delay_dist
    median <- distr::q(HCWvacc_delay_dist)(0.5)
    paste("Your median value for the delay to start HCW/FLW vaccination is:",round(median,digits=2),"days")
  })
  
  observeEvent(input$previousHCWvacc_delay,{
    accordion_panel_close(session=session,id="HCWvacc",values="HCWvacc_delay")
    accordion_panel_open(session=session,id="HCWvacc",values="HCWvacc_react")
  })
  
  observeEvent(input$nextHCWvacc_delay,{
    updateNavbarPage(session=session,"mainpage",selected="7")
    
  })
  
  ## Ring vaccination #############################################################
  
  observeEvent(input$Ringvacc_ring_min,{
    updateSliderInput(session,"Ringvacc_ring_max",min=input$Ringvacc_ring_min+0.1)
  })
  
  observeEvent(input$Ringvacc_ring_min_norm,{
    updateSliderInput(session,"Ringvacc_ring_max_norm",min=input$Ringvacc_ring_min_norm+0.1)
  })
  
  observeEvent(input$Ringvacc_ring_min_norm,{
    updateSliderInput(session,"Ringvacc_ring_max_norm",min=input$Ringvacc_ring_min_norm+0.1)
  })
  
  plotTypeRingvacc_ring <- reactive({input$Ringvacc_ring_shape
  })
  
  observeEvent(input$Ringvacc_ring_means, {
    # If the beta mean changes, compute the new implied standard deviation
    update_beta_dist(input$Ringvacc_ring_betasd, input$Ringvacc_ring_means, "Ringvacc_ring_betasd")
  })
  
  output$plotRingvacc_ring <- renderPlot({
    datRingvacc_ring <- data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
    if(plotTypeRingvacc_ring()=="Uniform"){
      Ringvacc_ring_dist = distr::Unif(Min=input$Ringvacc_ring_min,Max=input$Ringvacc_ring_max)
    }
    else if(plotTypeRingvacc_ring()=="Normal"){
      Ringvacc_ring_dist = distr::Truncate(distr::Norm(mean=input$Ringvacc_ring_mean,sd=input$Ringvacc_ring_sd),lower=input$Ringvacc_ring_min_norm,upper=input$Ringvacc_ring_max_norm)
    }
    else if(plotTypeRingvacc_ring()=="Skewed"){
      #then make these into gamma or beta distribution parameters
      Ringvacc_ring_dist = get_gamma_dist(input$Ringvacc_ring_means, input$Ringvacc_ring_var, input$Ringvacc_ring_min_beta, input$Ringvacc_ring_max_beta)
    }
    else if(plotTypeRingvacc_ring()=="Beta"){
      #then make these into beta distribution parameters
      beta_pars <- get_beta_parameters(input$Ringvacc_ring_means, input$Ringvacc_ring_betasd)
      datRingvacc_ring <- subset(datRingvacc_ring,xpos*(1-xpos)!=0)
      Ringvacc_ring_dist = distr::Truncate(distr::Beta(shape1 = beta_pars[1], shape2 = beta_pars[2]), input$Ringvacc_ring_min_beta, input$Ringvacc_ring_max_beta)
    }
    v$Ringvacc_ring_dist = Ringvacc_ring_dist
    datRingvacc_ring$ypos <- distr::d(Ringvacc_ring_dist)(datRingvacc_ring$xpos)
    datRingvacc_ring$qt  <- cut(distr::p(Ringvacc_ring_dist)(datRingvacc_ring$xpos),breaks=qrt,labels=F) #cut(pbeta(datRingvacc_ring$xpos,alpha=Ringvacc_ringShape,beta=Ringvacc_ringScale,log=F),breaks=qrt,labels=F)
    
    ggplot(datRingvacc_ring,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="Proportion of cases for which vaccination rings are established",y="pdf",color="Percentile",title="Probability density of the proportion of cases for which vaccination rings are established")+
      theme_gray(base_size = text_size)+theme(legend.position ="none") + 
      scale_x_continuous(breaks=breaksunit)
  }
  )
  
  output$Ringvacc_ring_conf<-renderText({
    Ringvacc_ring_dist = v$Ringvacc_ring_dist
    lower50 <- distr::q(Ringvacc_ring_dist)(0.25)
    upper50 <- distr::q(Ringvacc_ring_dist)(0.75) 
    lower95 <- distr::q(Ringvacc_ring_dist)(0.025) 
    upper95 <- distr::q(Ringvacc_ring_dist)(0.975)
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$Ringvacc_ring_median<-renderText({
    Ringvacc_ring_dist = v$Ringvacc_ring_dist
    median <- distr::q(Ringvacc_ring_dist)(0.5)
    paste("Your median value for the proportion of cases around which a ring is established is:",round(median,digits=2))
  })
  
  # For ringative vaccination
  observeEvent(input$previousRingvacc_ring,{
    updateNavbarPage(session=session,"mainpage",selected="6")
  })
  
  observeEvent(input$nextRingvacc_ring,{
    accordion_panel_close(session=session,id="Ringvacc",values="Ringvacc_ring")
    accordion_panel_open(session=session,id="Ringvacc",values="Ringvacc_react")
  })
  
  observeEvent(input$Ringvacc_react_min,{
    updateSliderInput(session,"Ringvacc_react_max",min=input$Ringvacc_react_min+0.1)
  })
  
  observeEvent(input$Ringvacc_react_min_norm,{
    updateSliderInput(session,"Ringvacc_react_max_norm",min=input$Ringvacc_react_min_norm+0.1)
  })
  
  observeEvent(input$Ringvacc_react_min_norm,{
    updateSliderInput(session,"Ringvacc_react_max_norm",min=input$Ringvacc_react_min_norm+0.1)
  })
  
  plotTypeRingvacc_react <- reactive({input$Ringvacc_react_shape
  })
  
  
  observeEvent(input$Ringvacc_react_means, {
    # If the beta mean changes, compute the new implied standard deviation
    Ringvacc_react_var <- input$Ringvacc_react_betasd^2
    # by definition we require
    # mean*(1 - mean) > variance
    if(input$Ringvacc_react_means * (1-input$Ringvacc_react_means) < Ringvacc_react_var){
      Ringvacc_react_var <- input$Ringvacc_react_means * (1-input$Ringvacc_react_means)
      max_sd = round(sqrt(Ringvacc_react_var), 3)
      updateSliderInput(session, "Ringvacc_react_betasd", value = max_sd)
    }
    beta_pars <- get_beta_parameters(input$Ringvacc_react_means, round(sqrt(Ringvacc_react_var), 3))
    Ringvacc_reactAlpha <- beta_pars[1]
    Ringvacc_reactBeta <- beta_pars[2]
    # normalise alpha and beta so that both are at least 1
    if(min(Ringvacc_reactAlpha,Ringvacc_reactBeta)<1){
      minab <- min(Ringvacc_reactAlpha,Ringvacc_reactBeta)
      Ringvacc_reactAlpha <- Ringvacc_reactAlpha/minab
      Ringvacc_reactBeta <- Ringvacc_reactBeta/minab
      implied_sd = round(sqrt(Ringvacc_reactAlpha*Ringvacc_reactBeta / ((Ringvacc_reactAlpha + Ringvacc_reactBeta)^2 * (Ringvacc_reactAlpha + Ringvacc_reactBeta + 1))), 3)
      updateSliderInput(session, "Ringvacc_react_betasd", value = implied_sd)
    }
  })
  
  output$plotRingvacc_react <- renderPlot({
    datRingvacc_react <- data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
    if(plotTypeRingvacc_react()=="Uniform"){
      Ringvacc_react_dist = distr::Unif(Min=input$Ringvacc_react_min,Max=input$Ringvacc_react_max)
    }
    else if(plotTypeRingvacc_react()=="Normal"){
      Ringvacc_react_dist = distr::Truncate(distr::Norm(mean=input$Ringvacc_react_mean,sd=input$Ringvacc_react_sd),lower=input$Ringvacc_react_min_norm,upper=input$Ringvacc_react_max_norm)
    }
    else if(plotTypeRingvacc_react()=="Skewed"){
      #then make these into gamma or beta distribution parameters
      Ringvacc_reactShape<-(input$Ringvacc_react_means*input$Ringvacc_react_means)/input$Ringvacc_react_var
      Ringvacc_reactScale<-input$Ringvacc_react_var/input$Ringvacc_react_means
      Ringvacc_reactAlpha<-input$Ringvacc_react_means*(((input$Ringvacc_react_means*(1-input$Ringvacc_react_means))/input$Ringvacc_react_var)-1)
      Ringvacc_reactBeta<-(1-input$Ringvacc_react_means)*(((input$Ringvacc_react_means*(1-input$Ringvacc_react_means))/input$Ringvacc_react_var)-1)
      datRingvacc_react<-data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
      Ringvacc_react_dist = distr::Gammad(shape1 = Ringvacc_reactAlpha, shape2 = Ringvacc_reactBeta)
    }
    else if(plotTypeRingvacc_react()=="Beta"){
      #then make these into beta distribution parameters
      beta_pars <- get_beta_parameters(input$Ringvacc_react_means, input$Ringvacc_react_betasd)
      datRingvacc_react <- subset(datRingvacc_react,xpos*(1-xpos)!=0)
      # print(c(14, input$Ringvacc_react_means, input$Ringvacc_react_betasd, beta_pars))
      Ringvacc_react_dist = distr::Truncate(distr::Beta(shape1 = beta_pars[1], shape2 = beta_pars[2]),lower=input$Ringvacc_react_min_beta,upper=input$Ringvacc_react_max_beta)
      # if(sum(beta_pars<.99)>0) return(NULL) # cut if parameters have not been updated
    }
    v$Ringvacc_react_dist = Ringvacc_react_dist
    datRingvacc_react$ypos <- distr::d(Ringvacc_react_dist)(datRingvacc_react$xpos)
    datRingvacc_react$qt  <- cut(distr::p(Ringvacc_react_dist)(datRingvacc_react$xpos),breaks=qrt,labels=F) #cut(pbeta(datRingvacc_react$xpos,alpha=Ringvacc_reactShape,beta=Ringvacc_reactScale,log=F),breaks=qrt,labels=F)
    
    ggplot(datRingvacc_react,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="Proportion of ring member who accept vaccination",y="pdf",color="Percentile",title="Probability density of the proportion of ring members who accept vaccination")+
      theme_gray(base_size = text_size)+theme(legend.position ="none") + 
      scale_x_continuous(breaks=breaksunit)
  }
  )
  
  output$Ringvacc_react_conf<-renderText({
    Ringvacc_react_dist = v$Ringvacc_react_dist
    lower50 <- distr::q(Ringvacc_react_dist)(0.25)
    upper50 <- distr::q(Ringvacc_react_dist)(0.75) # qbeta(p=0.75*pbeta(1,shape=Ringvacc_reactShape,scale=Ringvacc_reactScale),shape=Ringvacc_reactShape,scale=Ringvacc_reactScale)
    lower95 <- distr::q(Ringvacc_react_dist)(0.025) # qbeta(p=0.025*pbeta(1,shape=Ringvacc_reactShape,scale=Ringvacc_reactScale),shape=Ringvacc_reactShape,scale=Ringvacc_reactScale)
    upper95 <- distr::q(Ringvacc_react_dist)(0.975) # qbeta(p=0.975*pbeta(1,shape=Ringvacc_reactShape,scale=Ringvacc_reactScale),shape=Ringvacc_reactShape,scale=Ringvacc_reactScale)
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$Ringvacc_react_median<-renderText({
    Ringvacc_react_dist = v$Ringvacc_react_dist
    median <- distr::q(Ringvacc_react_dist)(0.5)
    paste("Your median value for the proportion of ring members who accept vaccination is:",round(median,digits=2))
  })
  
  # For reactive vaccination
  observeEvent(input$previousRingvacc_react,{
    accordion_panel_close(session=session,id="Ringvacc",values="Ringvacc_ring")
    accordion_panel_open(session=session,id="Ringvacc",values="Ringvacc_react")
  })
  
  observeEvent(input$nextRingvacc_react,{
    accordion_panel_close(session=session,id="Ringvacc",values="Ringvacc_react")
    accordion_panel_open(session=session,id="Ringvacc",values="Ringvacc_delay")
  })
  
  # For vaccination delay
  
  observeEvent(input$Ringvacc_delay_min,{
    updateSliderInput(session,"Ringvacc_delay_max",min=input$Ringvacc_delay_min+0.1)
  })
  
  observeEvent(input$Ringvacc_delay_min_skewed,{
    updateSliderInput(session,"Ringvacc_delay_max_skewed",min=input$Ringvacc_delay_min_skewed+0.1)
  })
  
  observeEvent(input$Ringvacc_delay_min_norm,{
    updateSliderInput(session,"Ringvacc_delay_max_norm",min=input$Ringvacc_delay_min_norm+0.1)
  })
  
  plotTypeRingvacc_delay <- reactive({input$Ringvacc_delay_shape
  })
  
  output$plotRingvacc_delay <- renderPlot({
    datRingvacc_delay <- data.frame(xpos=seq(xmin,xmaxDelay,by=0.01))
    if(plotTypeRingvacc_delay()=="Uniform"){
      Ringvacc_delay_dist = distr::Unif(Min=input$Ringvacc_delay_min,Max=input$Ringvacc_delay_max)
    }
    else if(plotTypeRingvacc_delay()=="Normal"){
      Ringvacc_delay_dist = distr::Truncate(distr::Norm(mean=input$Ringvacc_delay_mean,sd=input$Ringvacc_delay_sd),lower=input$Ringvacc_delay_min_norm,upper=input$Ringvacc_delay_max_norm)
    }
    else if(plotTypeRingvacc_delay()=="Skewed"){
      #then make these into gamma or beta distribution parameters
      Ringvacc_delayShape<-(input$Ringvacc_delay_means*input$Ringvacc_delay_means)/input$Ringvacc_delay_var
      Ringvacc_delayScale<-input$Ringvacc_delay_var/input$Ringvacc_delay_means
      # Ringvacc_delayAlpha<-input$Ringvacc_delay_means*(((input$Ringvacc_delay_means*(1-input$Ringvacc_delay_means))/input$Ringvacc_delay_var)-1)
      # Ringvacc_delayBeta<-(1-input$Ringvacc_delay_means)*(((input$Ringvacc_delay_means*(1-input$Ringvacc_delay_means))/input$Ringvacc_delay_var)-1)
      # datRingvacc_delay<-data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
      Ringvacc_delay_dist = Truncate(Gammad(shape = Ringvacc_delayShape, scale=Ringvacc_delayScale),lower=input$Ringvacc_delay_min_skewed,upper=input$Ringvacc_delay_max_skewed)
      
    }
    v$Ringvacc_delay_dist = Ringvacc_delay_dist
    datRingvacc_delay$ypos <- distr::d(Ringvacc_delay_dist)(datRingvacc_delay$xpos)
    datRingvacc_delay$qt  <- cut(distr::p(Ringvacc_delay_dist)(datRingvacc_delay$xpos),breaks=qrt,labels=F) #cut(pbeta(datRingvacc_delay$xpos,alpha=Ringvacc_delayShape,beta=Ringvacc_delayScale,log=F),breaks=qrt,labels=F)
    
    ggplot(datRingvacc_delay,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="Delay from case ascertainment to the start of ring vaccination (days)",y="pdf",color="Percentile",title="Probability density of the delay from case ascertainment to the start of ring vaccination")+
      theme_gray(base_size = text_size)+theme(legend.position ="none") + 
      scale_x_continuous(breaks=breaksDelay)
  }
  )
  
  output$Ringvacc_delay_conf<-renderText({
    Ringvacc_delay_dist = v$Ringvacc_delay_dist
    lower50 <- distr::q(Ringvacc_delay_dist)(0.25)
    upper50 <- distr::q(Ringvacc_delay_dist)(0.75) # qbeta(p=0.75*pbeta(1,shape=Ringvacc_delayShape,scale=Ringvacc_delayScale),shape=Ringvacc_delayShape,scale=Ringvacc_delayScale)
    lower95 <- distr::q(Ringvacc_delay_dist)(0.025) # qbeta(p=0.025*pbeta(1,shape=Ringvacc_delayShape,scale=Ringvacc_delayScale),shape=Ringvacc_delayShape,scale=Ringvacc_delayScale)
    upper95 <- distr::q(Ringvacc_delay_dist)(0.975) # qbeta(p=0.975*pbeta(1,shape=Ringvacc_delayShape,scale=Ringvacc_delayScale),shape=Ringvacc_delayShape,scale=Ringvacc_delayScale)
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$Ringvacc_delay_median<-renderText({
    Ringvacc_delay_dist = v$Ringvacc_delay_dist
    median <- distr::q(Ringvacc_delay_dist)(0.5)
    paste("Your median value for the delay from case ascertainment to the beginning of ring vaccination is:",round(median,digits=2),"days")
  })
  
  observeEvent(input$previousRingvacc_delay,{
    accordion_panel_close(session=session,id="Ringvacc",values="Ringvacc_delay")
    accordion_panel_open(session=session,id="Ringvacc",values="Ringvacc_react")
  })
  
  observeEvent(input$nextRingvacc_delay,{
    updateNavbarPage(session=session,"mainpage",selected="8")
    
  })
  
  ## Geographic vaccination #############################################################
  
  observeEvent(input$Geovacc_react_min,{
    updateSliderInput(session,"Geovacc_react_max",min=input$Geovacc_react_min+0.1)
  })
  
  plotTypeGeovacc_react <- reactive({input$Geovacc_react_shape
  })
  
  
  observeEvent(input$Geovacc_react_means, {
    # If the beta mean changes, compute the new implied standard deviation
    Geovacc_react_var <- input$Geovacc_react_betasd^2
    # by definition we require
    # mean*(1 - mean) > variance
    if(input$Geovacc_react_means * (1-input$Geovacc_react_means) < Geovacc_react_var){
      Geovacc_react_var <- input$Geovacc_react_means * (1-input$Geovacc_react_means)
      max_sd = round(sqrt(Geovacc_react_var), 3)
      updateSliderInput(session, "Geovacc_react_betasd", value = max_sd)
    }
    beta_pars <- get_beta_parameters(input$Geovacc_react_means, round(sqrt(Geovacc_react_var), 3))
    Geovacc_reactAlpha <- beta_pars[1]
    Geovacc_reactBeta <- beta_pars[2]
    # normalise alpha and beta so that both are at least 1
    if(min(Geovacc_reactAlpha,Geovacc_reactBeta)<1){
      minab <- min(Geovacc_reactAlpha,Geovacc_reactBeta)
      Geovacc_reactAlpha <- Geovacc_reactAlpha/minab
      Geovacc_reactBeta <- Geovacc_reactBeta/minab
      implied_sd = round(sqrt(Geovacc_reactAlpha*Geovacc_reactBeta / ((Geovacc_reactAlpha + Geovacc_reactBeta)^2 * (Geovacc_reactAlpha + Geovacc_reactBeta + 1))), 3)
      updateSliderInput(session, "Geovacc_react_betasd", value = implied_sd)
    }
  })
  
  output$plotGeovacc_react <- renderPlot({
    datGeovacc_react <- data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
    if(plotTypeGeovacc_react()=="Uniform"){
      Geovacc_react_dist = distr::Unif(Min=input$Geovacc_react_min,Max=input$Geovacc_react_max)
    }
    else if(plotTypeGeovacc_react()=="Normal"){
      Geovacc_react_dist = distr::Truncate(distr::Norm(mean=input$Geovacc_react_mean,sd=input$Geovacc_react_sd),lower=input$Geovacc_react_min_norm,upper=input$Geovacc_react_max_norm)
    }
    else if(plotTypeGeovacc_react()=="Skewed"){
      #then make these into gamma or beta distribution parameters
      Geovacc_reactShape<-(input$Geovacc_react_means*input$Geovacc_react_means)/input$Geovacc_react_var
      Geovacc_reactScale<-input$Geovacc_react_var/input$Geovacc_react_means
      Geovacc_reactAlpha<-input$Geovacc_react_means*(((input$Geovacc_react_means*(1-input$Geovacc_react_means))/input$Geovacc_react_var)-1)
      Geovacc_reactBeta<-(1-input$Geovacc_react_means)*(((input$Geovacc_react_means*(1-input$Geovacc_react_means))/input$Geovacc_react_var)-1)
      datGeovacc_react<-data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
      Geovacc_react_dist = distr::Gammad(shape1 = Geovacc_reactAlpha, shape2 = Geovacc_reactBeta)
    }
    else if(plotTypeGeovacc_react()=="Beta"){
      #then make these into beta distribution parameters
      beta_pars <- get_beta_parameters(input$Geovacc_react_means, input$Geovacc_react_betasd)
      datGeovacc_react <- subset(datGeovacc_react,xpos*(1-xpos)!=0)
      # print(c(14, input$Geovacc_react_means, input$Geovacc_react_betasd, beta_pars))
      Geovacc_react_dist = distr::Truncate(distr::Beta(shape1 = beta_pars[1], shape2 = beta_pars[2]),lower=input$Geovacc_react_min_beta,upper=input$Geovacc_react_max_beta)
      # if(sum(beta_pars<.99)>0) return(NULL) # cut if parameters have not been updated
    }
    v$Geovacc_react_dist = Geovacc_react_dist
    datGeovacc_react$ypos <- distr::d(Geovacc_react_dist)(datGeovacc_react$xpos)
    datGeovacc_react$qt  <- cut(distr::p(Geovacc_react_dist)(datGeovacc_react$xpos),breaks=qrt,labels=F) #cut(pbeta(datGeovacc_react$xpos,alpha=Geovacc_reactShape,beta=Geovacc_reactScale,log=F),breaks=qrt,labels=F)
    
    ggplot(datGeovacc_react,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="Proportion of community members who accept geographically targeted vaccination",y="pdf",color="Percentile",title="Probability density of the proportion of community members who accept geographically targeted vaccination")+
      theme_gray(base_size = text_size)+theme(legend.position ="none") + 
      scale_x_continuous(breaks=breaksunit)
  }
  )
  
  output$Geovacc_react_conf<-renderText({
    Geovacc_react_dist = v$Geovacc_react_dist
    lower50 <- distr::q(Geovacc_react_dist)(0.25)
    upper50 <- distr::q(Geovacc_react_dist)(0.75) # qbeta(p=0.75*pbeta(1,shape=Geovacc_reactShape,scale=Geovacc_reactScale),shape=Geovacc_reactShape,scale=Geovacc_reactScale)
    lower95 <- distr::q(Geovacc_react_dist)(0.025) # qbeta(p=0.025*pbeta(1,shape=Geovacc_reactShape,scale=Geovacc_reactScale),shape=Geovacc_reactShape,scale=Geovacc_reactScale)
    upper95 <- distr::q(Geovacc_react_dist)(0.975) # qbeta(p=0.975*pbeta(1,shape=Geovacc_reactShape,scale=Geovacc_reactScale),shape=Geovacc_reactShape,scale=Geovacc_reactScale)
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$Geovacc_react_median<-renderText({
    Geovacc_react_dist = v$Geovacc_react_dist
    median <- distr::q(Geovacc_react_dist)(0.5)
    paste("Your median value for the proportion of community members who accept geographically targeted vaccination is:",round(median,digits=2))
  })
  
  # For reactive vaccination
  observeEvent(input$previousGeovacc_react,{
    updateNavbarPage(session=session,"mainpage",selected="8")
  })
  
  observeEvent(input$nextGeovacc_react,{
    accordion_panel_close(session=session,id="Geovacc",values="Geovacc_react")
    accordion_panel_open(session=session,id="Geovacc",values="Geovacc_delay")
  })
  
  # For vaccination delay
  
  observeEvent(input$Geovacc_delay_min,{
    updateSliderInput(session,"Geovacc_delay_max",min=input$Geovacc_delay_min+0.1)
  })
  
  observeEvent(input$Geovacc_delay_min_skewed,{
    updateSliderInput(session,"Geovacc_delay_max_skewed",min=input$Geovacc_delay_min_skewed+0.1)
  })
  
  observeEvent(input$Geovacc_delay_min_norm,{
    updateSliderInput(session,"Geovacc_delay_max_norm",min=input$Geovacc_delay_min_norm+0.1)
  })
  
  plotTypeGeovacc_delay <- reactive({input$Geovacc_delay_shape
  })
  
  output$plotGeovacc_delay <- renderPlot({
    datGeovacc_delay <- data.frame(xpos=seq(xmin,xmaxDelay,by=0.01))
    if(plotTypeGeovacc_delay()=="Uniform"){
      Geovacc_delay_dist = distr::Unif(Min=input$Geovacc_delay_min,Max=input$Geovacc_delay_max)
    }
    else if(plotTypeGeovacc_delay()=="Normal"){
      Geovacc_delay_dist = distr::Truncate(distr::Norm(mean=input$Geovacc_delay_mean,sd=input$Geovacc_delay_sd),lower=input$Geovacc_delay_min_norm,upper=input$Geovacc_delay_max_norm)
    }
    else if(plotTypeGeovacc_delay()=="Skewed"){
      #then make these into gamma or beta distribution parameters
      Geovacc_delayShape<-(input$Geovacc_delay_means*input$Geovacc_delay_means)/input$Geovacc_delay_var
      Geovacc_delayScale<-input$Geovacc_delay_var/input$Geovacc_delay_means
      # Geovacc_delayAlpha<-input$Geovacc_delay_means*(((input$Geovacc_delay_means*(1-input$Geovacc_delay_means))/input$Geovacc_delay_var)-1)
      # Geovacc_delayBeta<-(1-input$Geovacc_delay_means)*(((input$Geovacc_delay_means*(1-input$Geovacc_delay_means))/input$Geovacc_delay_var)-1)
      # datGeovacc_delay<-data.frame(xpos=seq(xmin,xmaxUnit,by=0.001))
      Geovacc_delay_dist = Truncate(Gammad(shape = Geovacc_delayShape, scale=Geovacc_delayScale),lower=input$Geovacc_delay_min_skewed,upper=input$Geovacc_delay_max_skewed)
      
    }
    v$Geovacc_delay_dist = Geovacc_delay_dist
    datGeovacc_delay$ypos <- distr::d(Geovacc_delay_dist)(datGeovacc_delay$xpos)
    datGeovacc_delay$qt  <- cut(distr::p(Geovacc_delay_dist)(datGeovacc_delay$xpos),breaks=qrt,labels=F) #cut(pbeta(datGeovacc_delay$xpos,alpha=Geovacc_delayShape,beta=Geovacc_delayScale,log=F),breaks=qrt,labels=F)
    
    ggplot(datGeovacc_delay,aes(x=xpos,y=ypos))+
      geom_area(aes(x=xpos,y=ypos,group=qt,fill=qt),color="black")+
      labs(x="Delay from case ascertainment to the start of geographically targeted vaccination (days)",y="pdf",color="Percentile",title="Probability density of the delay from case ascertainment to the start of geographically targeted vaccination")+
      theme_gray(base_size = text_size)+theme(legend.position ="none") + 
      scale_x_continuous(breaks=breaksDelay)
  }
  )
  
  output$Geovacc_delay_conf<-renderText({
    Geovacc_delay_dist = v$Geovacc_delay_dist
    lower50 <- distr::q(Geovacc_delay_dist)(0.25)
    upper50 <- distr::q(Geovacc_delay_dist)(0.75) # qbeta(p=0.75*pbeta(1,shape=Geovacc_delayShape,scale=Geovacc_delayScale),shape=Geovacc_delayShape,scale=Geovacc_delayScale)
    lower95 <- distr::q(Geovacc_delay_dist)(0.025) # qbeta(p=0.025*pbeta(1,shape=Geovacc_delayShape,scale=Geovacc_delayScale),shape=Geovacc_delayShape,scale=Geovacc_delayScale)
    upper95 <- distr::q(Geovacc_delay_dist)(0.975) # qbeta(p=0.975*pbeta(1,shape=Geovacc_delayShape,scale=Geovacc_delayScale),shape=Geovacc_delayShape,scale=Geovacc_delayScale)
    paste("Your 50% confidence interval is:",round(lower50,digits=2),"-",round(upper50,digits=2), "and your 95%
          confidence interval is:",round(lower95,digits=2),"-",round(upper95,digits=2))
  })
  
  output$Geovacc_delay_median<-renderText({
    Geovacc_delay_dist = v$Geovacc_delay_dist
    median <- distr::q(Geovacc_delay_dist)(0.5)
    paste("Your median value for the delay from case ascertainment to the beginning of geographically targeted vaccination is:",round(median,digits=2),"days")
  })
  
  observeEvent(input$previousGeovacc_delay,{
    accordion_panel_close(session=session,id="Geovacc",values="Geovacc_delay")
    accordion_panel_open(session=session,id="Geovacc",values="Geovacc_react")
  })
  
  observeEvent(input$nextGeovacc_delay,{
    updateNavbarPage(session=session,"mainpage",selected="9")
    
  })
  
  ## Stockpile views #############################################################
  
  observeEvent(input$previousStockpile,{
    updateNavbarPage(session=session,"mainpage",selected="8")
  })
  
  observeEvent(input$nextStockpile,{
    updateNavbarPage(session=session,"mainpage",selected="Submit")
  })
  
  ## submission #############################################################
  
  observeEvent(input$previousSubmit,{
    updateNavbarPage(session=session,"mainpage",selected="9")
  })
  
  observeEvent(input$submit,{
    # tabulate answers
    # these are all the parameter prefixes for which values could be reported
    categories <- c('R0','DT','Asc','CTprop','CTfoll','HCWvacc_prevent','HCWvacc_react','HCWvacc_delay','Ringvacc_ring','Ringvacc_react','Ringvacc_delay','Geovacc_react','Geovacc_delay')
    answers <- c()
    for(ct in categories){
      answer <- paste0('answer',ct)
      if(input[[answer]]=='Yes'){
      # if(!is.null(input[[paste0(ct,'_shape')]])){
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
    tested_corr = c()
    for(sgn in cors){
      cc <- paste0('is_',sgn)
      tested_corr <- c(tested_corr, sgn)
      if(!is.null(input[[cc]]) && input[[cc]]!='Not sure') {
        answers = rbind(answers, c(cc, input[[cc]]))
        if (input[[cc]]=='Yes') answers = rbind(answers, c(sgn, input[[sgn]]))
      }
    }
    corrnames <- names(input)[grepl('^corr_',names(input))]
    # print in case any correlation terms were missed:
    print(corrnames[!corrnames%in%tested_corr])
    
    
    # tabulate user info
    expinfo = c()
    expvarnames = c('ExpOutbreaks', 'ExpOutbreaksOther', 'ExpSetting', 'ExpWorkplace','ExpDept',"vacc_doses","vacc_teams","worst_case","stockpile")
    expvars = sapply(expvarnames,function(x)ifelse(is.null(input[[x]]),'',paste0(input[[x]],collapse=', ')))
    expvarstab = data.frame(Variable=expvarnames,Value=expvars)
    rownames(expvarstab) <- NULL
    expvars_spec = c('ExpCT', 'ExpCase', 'ExpEpi', 'ExpVacc')
    for(vspec in expvars_spec){
      if(input[[vspec]] != 'No') {
        lenvar <- paste0(vspec,'_length')
        expvarstab <- rbind(expvarstab, c(lenvar, input[[lenvar]]))
      }
    }
    
    # give column names; print; save
    print(expvarstab)
    print(answers)
    filename = paste0(format(now(), "%Y%m%d_%H%M%S_"), "data_set.xlsx")
    xlsx::write.xlsx(expvarstab,file = filename,sheetName='User data', append=F,row.names = F)
    if(length(answers)>0){
      answers <- as.data.frame(answers)
      colnames(answers) <- c('Variable','Value')
      xlsx::write.xlsx(answers,file = filename,sheetName='Parameter data', append=T,row.names = F)
    }
    
    ## maybe create a "thanks" page or restart? i think you need to close and reopen to reset everything for the next user.
    updateNavbarPage(session=session,"mainpage",selected="End")
    
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
