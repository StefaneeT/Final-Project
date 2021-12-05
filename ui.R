library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  
                  
titlePanel("ST 558 Final Project"),
navbarPage("Let's Begin",  
                tabPanel("About",
                         
                         fluidRow(column(tags$img(src='Students.png',width="200px",height="130px"),width = 2)),
                         
                         h3("1. Purpose of the App", style="text-align:center;color:black"),
                         p("The purpose of this app is to explore the Student's Performance data to see if school setting, classroom, teaching method,
              number of students in the classroom, gender, lunch qualification, and pretest scores have any influence on the response variable, posttest scores.
              In this app, users will be able to observe the modeling for this data set and the best fit model. Users will also be able to observe prediction models for this dataset. ",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                         
                         
                         br(),
                         h3("2. Data and it's source", style="text-align:center;color:black"),
                    
                         p("The dataset used in this application is available at", em("Kaggle.com"), " 
            The data contains information about a test written by some students. It include features such as: School setting, School type, gender, pretetest scores among other. The ultimate goal of this 
              data set is to predict the scores of the students. ",style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                        
                          p("For more information about the dataset, check out ",em("Kaggle.com"),
                           br(),
                           a(href="https://www.kaggle.com/kwadwoofosu/predict-test-scores-of-students", "Here",target="_blank"),style="text-align:center;color:black"),

                         br(),
                         h3("3. Purpose of the tabs", style="text-align:center;color:black"),
                         
                         p("In this app we have 6 different tabs.
                         The first tab is The About tab.
                         This About tab describes the purpose of the app.
                         Briefly discusses the data, describes the purpose of each data and displays a picture.The second tab us the Data Exploration tab. This will allow the user to create numerical and graphical summaries. 
                         Change plot type and type of summary reported. The user will also be allow to change the variables and filter the row to change the data in the plots/summaries.
                         The third tab is the Modeling tab. Here we will fit three supervised learning models. In this tab we have a Modeling Info tab that will explain the three modeling approaches and the pros and cons of each.
                         There is also a Model Fitting tab that will split data into training and testing set.  Users should have the functionality for choose model settings for each model.
    The fourth tab is the Prediction tab. This tab will give users a way to use one of the models for prediction. Lastly, there is a Data page that allows users to scroll, subset, and save the data.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                         
                
                         
                         ),#end of first About Tab
                         

                
                
                
              
                
                
                tabPanel("Data Exploration",  #start of data Exploration Tab
                         
                         
                         
                         #Start of Numerical Summaries
                         tabsetPanel(
                            

                         h3(p(strong('Model Numerical Summary',style="color:salmon")),align="center"),
                    
                         
                         p("Observe the contigency tables that observe each predictor variable for Posttest grade levels in order: A, B, C, D, F",style="color:black;text-align:justify"),
                         
                         hr(),
                         #Contingency Tables
                         tabPanel("Lunch Qualifications",
                                  fluidRow(column(tableOutput("numsum1"),width = 7),
                                           column(width=1),
                                           column(br(),
                                                  width = 4,style="background-color:lavender")),
                                  
                                  br()),
                         
                         
                         tabPanel("School Setting",
                                  fluidRow(column(tableOutput("numsum2"),width = 7),
                                           column(width=1),
                                           column(br(),
                                                  width = 4,style="background-color:lavender")),
                                  
                                  br()),
                         
                         tabPanel("School Type",
                                  fluidRow(column(tableOutput("numsum3"),width = 7),
                                           column(width=1),
                                           column(br(),
                                                  width = 4,style="background-color:lavender")),
                                  
                                  br()),
                         
                         tabPanel("Teaching Methods",
                                  fluidRow(column(tableOutput("numsum4"),width = 7),
                                           column(width=1),
                                           column(br(),
                                                  width = 4,style="background-color:lavender")),
                                  
                                  br()),
                         
                         
                         tabPanel("Gender",
                                  fluidRow(column(tableOutput("numsum5"),width = 7),
                                           column(width=1),
                                           column(br(),
                                                  width = 4,style="background-color:lavender")),
                                  
                                  br()),
                         
                         tabPanel("Pretest",
                                  fluidRow(column(tableOutput("numsum6"),width = 7),
                                           column(width=1),
                                           column(br(),
                                                  width = 4,style="background-color:lavender")),
                                  
                                  br()),

                          ),#End of Numerical Summaries
                         
                         
                         
                         
                          
                   
                          #Begin Tabs for Graphs                         
                         tabsetPanel(
                             h3(p(strong('Model Graphical Summary',style="color:salmon")),align="center"),
                             
                           #1st Graph
                       tabPanel("Number of Students",
                                
                               
                                    
                                    br(),
                                    br(),
                                    sidebarLayout(
                                      
                                      sidebarPanel(
                                        p("Observe the relationship between response variable and the independent variable",style="color:black;text-align:justify"),
                                        
                                        tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}")),
                                        tags$style(HTML(".js-irs-1 .irs-max, .js-irs-1 .irs-min {background:papayawhip}")),
                                        
                                      
                                        br(),
                                        column(
                                          br(),
                                          tags$head(tags$style("#














acion1{color: black;
                                                                        font-size: 15px;
                                                                        text-align: center;
                                                                        }")),
                                          textOutput("correlacion1"),
                                          br(),
                                          p("This plot shows the relationship between the variable post test and number of students.
                                            It can be observed that more students recieved F's and fewer students recieved A's as their final grades."),
                                          br(),width = 12,style="background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                                        
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br()
                                      ),
                                      mainPanel(column(plotOutput("Plot1"),width = 12,style="border:1px solid black"))),
                                    br()),
                    
                       
                       
                       #Start of 2nd Graph
                       tabPanel("Lunch Qualifications",
                                
                                br(),
                                br(),
                                sidebarLayout(
                                  
                                  sidebarPanel(
                                    p("Observe the relationship between response variable and the independent variable",style="color:black;text-align:justify"),
                                    
                                    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}")),
                                    tags$style(HTML(".js-irs-1 .irs-max, .js-irs-1 .irs-min {background:papayawhip}")),
                                    
                                    
                                    br(),
                                    column(
                                      br(),
                                      tags$head(tags$style("#correlacion2{color: black;
                                                                        font-size: 15px;
                                                                        text-align: center;
                                                                        }")),
                                      textOutput("correlacion2"),
                                      br(),
                                      p("This plot shows the relationship between the variable post test and number of students.
                                            It can be observed that their were more F's for the students that qualified for reduced lunch and better grades for students that do not qualify for free lunch."),
                                      br(),width = 12,style="background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                                    
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br()
                                  ),
                                  mainPanel(column(plotOutput("Plot2"),width = 12,style="border:1px solid black"))),
                                br()
                       ),                 
                       
                       
                       
#Start of 3rd Graph
                       tabPanel("School Setting",
                                
                                br(),
                                br(),
                                sidebarLayout(
                                  
                                  sidebarPanel(
                                    p("Observe the relationship between response variable and the independent variable",style="color:black;text-align:justify"),
                                    
                                    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}")),
                                    tags$style(HTML(".js-irs-1 .irs-max, .js-irs-1 .irs-min {background:papayawhip}")),
                                    
                                    
                                    br(),
                                    column(
                                      br(),
                                      tags$head(tags$style("#correlacion3{color: black;
                                                                        font-size: 15px;
                                                                        text-align: center;
                                                                        }")),
                                      textOutput("correlacion3"),
                                      br(),
                                      p("This plot shows the relationship between the variable post test and Typer of school setting.students that lived in Rural and Suburban Areas had better grades."),
                                      br(),width = 12,style="background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                                    
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br()
                                  ),
                                  mainPanel(column(plotOutput("Plot3"),width = 12,style="border:1px solid black"))),
                                br()
                       ), #End of Graph 3                       
                       
                        
#Start of 4th Graph
tabPanel("School Type",
         
         br(),
         br(),
         sidebarLayout(
           
           sidebarPanel(
             p("Observe the relationship between response variable and the independent variable",style="color:black;text-align:justify"),
             
             tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}")),
             tags$style(HTML(".js-irs-1 .irs-max, .js-irs-1 .irs-min {background:papayawhip}")),
             
             
             br(),
             column(
               br(),
               tags$head(tags$style("#correlacion4{color: black;
                                                                        font-size: 15px;
                                                                        text-align: center;
                                                                        }")),
               textOutput("correlacion4"),
               br(),
               p("This plot shows the relationship between the variable posttest and Type of school. The school types are: Non-Public and Public. It can be observed that students in Public Schools received lower grades and students in Non-public schools received better grades."),
               br(),width = 12,style="background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
             
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br()
           ),
           mainPanel(column(plotOutput("Plot4"),width = 12,style="border:1px solid black"))),
         br()
), #End of Graph 4                      


#Start of 5th Graph
tabPanel("Teaching Method",
         
         br(),
         br(),
         sidebarLayout(
           
           sidebarPanel(
             p("Observe the relationship between response variable and the independent variable",style="color:black;text-align:justify"),
             
             tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}")),
             tags$style(HTML(".js-irs-1 .irs-max, .js-irs-1 .irs-min {background:papayawhip}")),
             
             
             br(),
             column(
               br(),
               tags$head(tags$style("#correlacion5{color: black;
                                                                        font-size: 15px;
                                                                        text-align: center;
                                                                        }")),
               textOutput("correlacion5"),
               br(),
               p("This plot shows the relationship between the variable posttest and Teaching Methods. The Teaching Methods are: Experimental and Standard. It can be observed that students that had the Standard teaching method received lower grades and that students that recieved the Experimental Method recieved better grades.
                 It can also be observed that less students received the Experimental Teaching Method."),
               br(),width = 12,style="background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
             
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br()
           ),
           mainPanel(column(plotOutput("Plot5"),width = 12,style="border:1px solid black"))),
         br()
), #End of Graph 5    
                       
   
#Start of 6th Graph
tabPanel("Gender",
         
         br(),
         br(),
         sidebarLayout(
           
           sidebarPanel(
             p("Observe the relationship between response variable and the independent variable",style="color:black;text-align:justify"),
             
             tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}")),
             tags$style(HTML(".js-irs-1 .irs-max, .js-irs-1 .irs-min {background:papayawhip}")),
             
             
             br(),
             column(
               br(),
               tags$head(tags$style("#correlacion6{color: black;
                                                                        font-size: 15px;
                                                                        text-align: center;
                                                                        }")),
               textOutput("correlacion6"),
               br(),
               p("This plot shows the relationship between the variable posttest and gender. It can be observed that female students received more F's and A's than male students. But it also appears to be mre female students than males in this dataset. "),
               br(),width = 12,style="background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
             
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br()
           ),
           mainPanel(column(plotOutput("Plot6"),width = 12,style="border:1px solid black"))),
         br()
), #End of Graph 6                 
                       
             


          
#Start of Dynamic
tabPanel("Dynamic UI",
         
         br(),
         br(),
         sidebarLayout(
             
             sidebarPanel(
                 
                 selectInput("var", "Select the variable", choices = c("n_student" = 4, "pretest" =7, "posttest" =8)),
                 sliderInput("bins", "Select Bins for Histogram:",
                             min = 5, max = 100, value = 10, step = 10),
             
                 p("Observe the Histograms",style="color:black;text-align:justify"),
                 
                 tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}")),
                 tags$style(HTML(".js-irs-1 .irs-max, .js-irs-1 .irs-min {background:papayawhip}")),
                 
                 
                 br(),
                 column(
                     br(),
                     tags$head(tags$style("#correlacion7{color: black;
                                                                        font-size: 15px;
                                                                        text-align: center;
                                                                        }")),
                     textOutput("correlacion7"),
                     br(),
                     p("Here we can observe the frequency histogram for the three numeric variables in this dataset: number of students, pretest scores and posttest scores. Here we can observe that for both pretest
                       and posttest scores the frequencies are higher towards the median performance scores and lower towards higher and low performance scores."),
                     br(),width = 12,style="background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                 
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br()
             ),
             mainPanel(column(plotOutput("Plot7"),width = 12,style="border:1px solid black"))),
         br()
), #End of Graph 6                        
                       
                       
                       
                       
                       
                       
                       
                        ), #End of Graph Tabset
                      
hr(),

tags$style(".fa-database {color:#E87722}"),
h3(p(em("Dataset "),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
fluidRow(column(DT::dataTableOutput("RawData"),
                width = 12)),        

            
            
            
                      
                      
   
                      
                      ),#End of Data Exploration Tab



##########################
##########################
#Start Modeling Tab
navbarMenu("Modeling", icon = icon("info-circle"),
##########################
##########################

#Start of SubTab Modeling Information
                tabPanel("Modeling Info",
                         sidebarPanel(width=1),
                         mainPanel(width=8,
                                   h3(p(strong("Modeling Information", style = "color:black;"))),
                                   p("Here we will be fitting three different models: a linear Regression Model, a Random Forest Model, and
                                   a regression tree model.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   
                                   br(), 
                                   h4("1. Linear Modeling Approach", style = "color:black;"),
                                   p("A linear regression model is a model that displays the relationship between two or more variables.
                                    An advantage of a linear regression model is how simple it is to build a model with predictors variables and a set response variable 
                                     and a disadvantage here is that these variables may not be exactly 'linear'.",style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px")
                                   ,
                                   
                                uiOutput('fitmathjax'),
                                   br(), 
                                   h4("2. Random Forest Model Approach", style = "color:black;"),
                                   p("A Random Forest is a machine learning technique that is used to solve regression and classification problems. The Random Forest Technique extends the idea of bagging and is generally better than bagging.
                                     The Random Forest will create multiple trees from bootstrap samples and average the results.After using the Cross Validation Method we are able to observe the
                                     RMSE and R^2 values for the predictor variables with posttest as the response. A few benefits of the Random Forest Model include are  
                                      there is a lower risk of overfitting, and it runs efficiently on a large dataset. Random Forest typically have better accuracy than other classification algorithms. A drawback for the Random Forest
                                     is that it runs slow on the training set and it can be biases when dealing the categorical variables.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")
                                   ,
                                   
                                   br(), 
                                   h4("3. Boosted Tree Model", style = "color:black;"),
                                   p("The Boosted Tree Model is an algorithm model that helps in reducing variance and bias. With the Boosted Tree Model
                                   each tree attempts to minimize the errors of previous tree. Trees in boosting are weak learners but adding many trees in series and each focusing
                                   on the errors from previous one make boosting a highly efficient and accurate model. A few advantages are: they are highly efficient on both classification and regression tasks and
they are more accurate predictions compared to random forests. A few drawbacks are users may overfit if too many trees are used and they are sensitive to outliers.",style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px")
                                   ,
                                   
                                   
                                   )),#End of Modeling Info SubTab

#Splitting Data
tabPanel("Model Fitting",
         
        ###
        column(4, "Modeling Choices for All Models",
               sliderInput("Slider1",
                           label = h3("Train Split %"),
                           min = 0,
                           max = 100,
                           value = 75),
               selectizeInput("PredVars", "Select Predictors:", choices = names(data), multiple = TRUE),
               numericInput("folds", "Set Folds:", min = 2, max = 10, value = 5)),
        
        
        
        column(4, "Parameters for Random Forest",
               numericInput("mtry", "Select Number of mtry:", 1, min = 1, max = 8),
        ),
        
        
        column(4, "Parameters for Boosted Tree",
               selectInput("ntrees", "Number of Trees",
                           c(25, 50, 75, 100, 125, 150, 175, 200, 250, 300, 350, 400, 450, 500),
                           multiple = TRUE, selectize = FALSE),
               selectInput("intdeph", "Interaction Depth", c(1:10), multiple = TRUE, 
                           selectize = FALSE),
               selectInput("shrink", "Choosing Shrinkage", c(.001, .01, .1), multiple = TRUE, 
                           selectize = FALSE),
               selectInput("nodes", "Nodes", c(100, 500, 750, 1000), multiple = TRUE, 
                           selectize = FALSE),
        ),
        ###
         
         
         
             actionButton("action", label = "Fit All Models"),
             
             hr(),
        
        
        mainPanel(
            h4("Linear Regression Model"), verbatimTextOutput("lmsummary1"),
            br(),
            
            h4("Random Forest Model Accuracy"),  dataTableOutput("rfaccuracy"),
            br(),
            
            h4("Random Forest Variable of Important"),  plotOutput("rfVarImp"),
            br(),
            
            h4("Boosted Tree Model"),  verbatimTextOutput("btreeaccuracy"),
            br(),
            
            h4("Compare RMSE Test Set"),  verbatimTextOutput("testsetcompare"),
            br(),
            
            
        ),
        
             
        ),#End Model Fitting Tab
         
 
        
        
#End Model Fitting SubTab
tabPanel("Prediction",
         
        
         h3(strong("Prediction", style="text-align:center;color:black")),
p("Here users can use the Linear Regression Model for Prediction of the response variable with chosen predictor variables",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
         
selectizeInput(
    "PredVars", "Select Predictors:", choices = names(data), multiple = TRUE),  

actionButton("action2", label = "Fit All Models"),

mainPanel(
    tableOutput("Pred"),
    uiOutput("select")),
        

 )#Closes Prediction Tab

),#Closes out Whole Modeling Tab

tabPanel("Data Page",
         sidebarLayout(
             
             # Sidebar panel for inputs ----
             sidebarPanel(
                 
                 # Input: Choose dataset ----
                 selectInput("dataset", "Choose a dataset:",
                             choices = c("data", "intdata")),
                 
                 downloadButton("downloadData", "Download")
                 
             ),
             
             # Main panel for displaying outputs ----
             mainPanel(tableOutput("table"))
             
         )
         
         
         ),#Closes Data Page
         
        

                   
),#Closes Navbar

textOutput("cntTrain"),
textOutput("cntTest"),)#Closes Fluid Pages



)#End of UI
