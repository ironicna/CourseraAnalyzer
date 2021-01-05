courses <- read.csv("Coursera_courses_catalog.csv", stringsAsFactors = TRUE)
head(courses)
str(courses)
summary(courses)

# dropping variables not needed in the prediction
courses$X<-NULL
courses$course_name<-NULL
courses$course_link<-NULL
courses$university_logo<-NULL
courses$course_subtitles<-NULL
courses$time_required<-NULL
courses$course_skills<-NULL
courses$university_name<-NULL
courses$course_rating<-NULL
courses$course_level<-NULL

#creating a randomly ordered data frame
set.seed(12345)
c_rand <- courses[order(runif(5164)), ]

#checking the number of values
summary(courses$course_type)
summary(c_rand$course_type)

# split into 90% training and 10% testing data
c_train <-c_rand[1:4648, ]
c_test <- c_rand[4649:5164, ]

#creating model
library(C50)
c_model <- C5.0(c_train[-1], as.factor(c_train$course_type))
c_model
summary(c_model)
plot(c_model)

#accuracy
(3959+348)/4648
#sensitivity 
3959/(3959+0)
#specificity
348/(348+341)

#creating predictor
c_predict <- predict(c_model,c_test)
library(gmodels)
CrossTable(c_test$course_type, c_predict,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual category', 'predicted category'))

#accuracy
(425+46)/516
#sensitivity 
425/(425+0)
#specificity
46/(46+45)
