library(shiny)
library(shinydashboard)
library(maps)
library(dplyr)
library(leaflet)
library(shinycssloaders)
library(shinythemes)
library(datadigest)
library(rio)
library(DT)
library(stargazer)
library(ggplot2)
library(tree) 
library(caret)



# Define server logic 
shinyServer(function(input, output, session) {
   
  output$fitmathjax <- renderUI({
    withMathJax(helpText('The Linear Regression Model Formula is:  $$\\beta_0 + \\beta_1x  + \\beta_2x + ... + \\beta_kx + e$$'))
  })
     
###############   
#Model Summary
###############
  
#1. Posttest vs. lunch
output$numsum1 <- renderTable({
    
  contin1<- table(data$posttest, data$lunch)
  as.data.frame.matrix(contin1)
}, rownames = TRUE)



#2. Posttest vs. School Setting
output$numsum2 <- renderTable({
  
  contin2<- table(data$posttest, data$school_setting)
  as.data.frame.matrix(contin2)
  
}, rownames = TRUE)


#3.Posttest vs. School Type
output$numsum3 <- renderTable({
  contin3<- table(data$posttest, data$school_type)
  as.data.frame.matrix(contin3)
}, rownames = TRUE)
  
#4.Posttest vs. Teaching Method
output$numsum4 <- renderTable({
contin4<- table(data$posttest, data$teaching_method)
as.data.frame.matrix(contin4)
}, rownames = TRUE)


#5.Posttest vs. Gender
output$numsum5 <- renderTable({
contin5<- table(data$posttest, data$gender)
as.data.frame.matrix(contin5)
}, rownames = TRUE)

#6.Posttest vs. Pretest
output$numsum6 <- renderTable({
contin6<- table(data$posttest, data$pretest)
}, rownames = TRUE)


#End of Model Summary
    

#############
#Graphs
#############

#Posttest vs. Number of Students Plot        
        output$Plot1 <- renderPlot({
            data$posttest<- factor(data$posttest, levels = c("A", "B", "C", "D", "F"))
            plot1 <- ggplot(data = data, aes(x= posttest))
            plot1 + geom_bar(aes(fill= n_student), position = "dodge") + labs(x = "posttest", y = "Count") 
}) #End of Plot 1
        
        
#Posttest vs. Lunch
        output$Plot2 <- renderPlot({
            data$posttest<- factor(data$posttest, levels = c("A", "B", "C", "D", "F"))
            plot1 <- ggplot(data = data, aes(x= posttest))
            plot1 + geom_bar(aes(fill= lunch), position = "dodge") + labs(x = "posttest", y = "Count") 
}) #End of Plot 2
        
        
        #Posttest vs. School Setting
        output$Plot3 <- renderPlot({
            data$posttest<- factor(data$posttest, levels = c("A", "B", "C", "D", "F"))
            plot1 <- ggplot(data = data, aes(x= posttest))
            plot1 + geom_bar(aes(fill= school_setting), position = "dodge") + labs(x = "posttest", y = "Count") 
        }) #End of Plot 3
        
        
        #Posttest vs. School Type
        output$Plot4 <- renderPlot({
            data$posttest<- factor(data$posttest, levels = c("A", "B", "C", "D", "F"))
            plot1 <- ggplot(data = data, aes(x= posttest))
            plot1 + geom_bar(aes(fill= school_type), position = "dodge") + labs(x = "posttest", y = "Count") 
        }) #End of Plot 4
        
        #Posttest vs. Teaching Method
        output$Plot5 <- renderPlot({
            data$posttest<- factor(data$posttest, levels = c("A", "B", "C", "D", "F"))
            plot1 <- ggplot(data = data, aes(x= posttest))
            plot1 + geom_bar(aes(fill= teaching_method), position = "dodge") + labs(x = "posttest", y = "Count") 
        }) #End of Plot 5
        
        #Posttest vs. Gender
        output$Plot6 <- renderPlot({
            data$posttest<- factor(data$posttest, levels = c("A", "B", "C", "D", "F"))
            plot1 <- ggplot(data = data, aes(x= posttest))
            plot1 + geom_bar(aes(fill= gender), position = "dodge") + labs(x = "posttest", y = "Count") 
        }) #End of Plot 6
        
        
    
        
        #Dynamic
        output$Plot7 <- renderPlot({
         # hist(rnorm(input$n))
         # hist(data, breaks = seq(0, max(data, l= input$bins +1)))
          
        
       colm<- as.numeric(input$var)
       hist(intdata[,colm], breaks=seq(0, max(intdata[,colm]), l=input$bins), main = "Histogram of Student Scores Dataset", xlab=names(intdata[colm]))   
        })
        
       
    
###############End of Graphs
     
        
        
####################################################      
#Datatable        
####################################################
        
        output$RawData<- DT::renderDataTable(
            DT::datatable({
                data
            },
            options = list(lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                           initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                               "}"),
                           columnDefs=list(list(className='dt-center',targets="_all"))
            ),
            filter = "top",
            selection = 'multiple',
            style = 'bootstrap',
            class = 'cell-border stripe',
            rownames = FALSE,
            colnames = c("school_setting","school_type","teaching_method","n_student","gender","lunch","pretest","posttest")
            ))#End of Data Table
        
###################################################      
##########Modeling Try
###################################################  

        
#Modeling Choices for All Models
numcv<- eventReactive(input$action, {
  input$folds
  })        
        
split<- eventReactive(input$action, {
          input$Slider1/100
        })   

predtrain<- eventReactive(input$action, {
  set.seed(123)
  trainIndex<- createDataPartition(data$pretest, p= split(), list = FALSE)
  #Train
  data_train<- intdata[dt,]
  
  req(input$PredVars)
  predtrain<- data %>% select(all_of(input$PredVars))
  predtrain
})

predtest<- eventReactive(input$action, {
  set.seed(123)
  #trainIndex<- createDataPartition(data$posttest,times= 1, p= split(), list = FALSE)
  #Train
  data_test<- intdata[-dt,]
  
  req(input$PredVars)
  predtest<- data %>% select(all_of(input$PredVars))
  predtest
})


#Create Progress Object
progress <- Progress$new()

#Closes on Reactive
on.exit(progress$close())

progress$set(message = "Models loading", value = 0)



##1. Linear Model
#Reaction
lmfit<- eventReactive(input$action, {
  lmfit<- train(posttest~ ., data = train,
                method = "lm", trControl = trainControl(method = "cv", number = input$folds))
})

#Visualize
output$lmsummary1<- renderPrint({
  lmmodel <- lm(posttest~., data=train, trControl = trainControl(method = "cv", number = 5))
  summary(lmmodel)
})
progress$inc(0.3, detail = "Linear Regression Model")



##2. Random Forest
#Reaction
#rfmodel<- eventReactive(input$action, {
  
  #rfmodel <- train(posttest ~ school_setting + school_type + teaching_method + n_student + gender + lunch + pretest, data = data_train,
                   #method = "rf", trControl = trainControl(method= "cv", number = 5),
                   #preProcess = c("center", "scale"), tuneGrid = data.frame(mtry = 1:8))

#})

#Visualize
#output$rfaccuracy<- renderPrint({
 #print(rfmodel)
#})

#2. Random Forest Accuracy
output$rfaccuracy<- renderDataTable(({
  rfmodel<- train(posttest ~ school_setting + school_type + teaching_method + n_student + gender + lunch + pretest, data = train,
                  method = "rf", trControl = trainControl(method= "cv", number = 5),
                  preProcess = c("center", "scale"), tuneGrid = data.frame(mtry = 1:8))
  print(rfmodel)
}))


#3. Random Forest Variable of Important
output$rfVarImp<- renderPlot({
  rfplot<- ggplot(varImp(object = rfmodel)) + ggtitle("Variable of Importance")
  rfplot
})

progress$inc(0.5, detail = "Random Forest Model")

#3. Boosted Tree
gbmGrid <-  expand.grid(interaction.depth = 1:4,
                        n.trees = c(25, 50, 100, 150, 200),
                        shrinkage = 0.1,
                        n.minobsinnode = 10)
nrow(gbmGrid)
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
boostFit <- train(posttest ~ school_setting + school_type + teaching_method + n_student + gender + lunch + pretest,
                  data = train,
                  preProcess = c("center", "scale"),
                  trControl = fitControl,
                  method = "gbm",
                  tuneGrid = gbmGrid)              

#Reaction
ntreesnum<- eventReactive(input$action, {
  input$ntrees
})
#Visualize


output$btreeaccuracy<- renderPrint(({
  
  
  print(boostFit)
  
}))
progress$inc(1, detail = "Models Complete")

#5. Compared on Test Set
output$testsetcompare<- renderPrint(({
  
  
  print(boostRMSE)
  print(rfRSME)
  print(lmRSME)
}))
 
#############
#Prediction
#############
repred<- eventReactive(input$action, {
  input$PredVars
})        

pred.fit1 <- predict(lmmodel, newdata = test)
print(pred.fit1)
output$Pred <- renderTable(pred.fit1) 







###################################################
#Data Page
###################################################
# Reactive value for selected dataset ----
datasetInput <- reactive({
  switch(input$dataset,
         "data" = data,
         "intdata" = intdata)
})

#Table of selected dataset ----
output$table <- renderTable({
datasetInput()
})



# Downloadable csv of selected dataset ----
output$downloadData <- downloadHandler(
  filename = function() {
    paste(input$dataset, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(datasetInput(), file, row.names = FALSE)
  }
)










    })#End of Shiny Server Function
    

    
