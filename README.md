Final-Project
=================

# Brief Description of the app and it's purpose
The purpose of this app is to explore the Student's Performance data to see if school setting, classroom, teaching method, number of students in the classroom, gender, lunch qualification, and pretest scores have any influence on the response variable, posttest scores. In this app, users will be able to observe the modeling for this data set and predict it using a train and test set. This dataset is available at Kaggle.com. The data contains information about a test written by some students. It include features such as: School setting, School type, gender, pretetest scores among other. The ultimate goal of this data set is to predict the scores of the students. 


# A list of packages needed to run the app.

• `dplyr()`

• `caret()`

• `shiny()`

• `tree()`

• `stargazer()`

• `tidyverse()`

• `shinycssloaders()`

# A line of code to easily install the packages used

 install. packages(c("dplyr", "caret", "shiny", "tree", "stargazer", "tidyverse", "shinycssloaders"))

# The shiny::runGitHub() code that we can copy and paste into Rstudio to run your app. 
shiny::runGitHub("Final-Project", "StefaneeT", ref="main")
