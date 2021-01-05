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

#convert factors to integers
courses$course_language<-as.integer(courses$course_language)
courses$category<-as.integer(courses$category)
courses$sub_category<-as.integer(courses$sub_category)

summary(courses)

#creating training and testing set
c1<-courses[2:4]
c_train <- c1[1:4131, ]
c_test <- c1[4132:5164, ]

#storing target variable in vectors
c_train_label <- courses[1:4131, 1]
c_test_label <- courses[4132:5164, 1]

#building the classifier
library(class)
c_predict <- knn(train = c_train,test = c_test, cl=c_train_label, k=32)
summary(c_predict)

#evaluation
library(gmodels)
CrossTable(x=c_test_label,y=c_predict, prop.chisq = FALSE)

#accuracy for k=32
(892+37)/1033
#sensitivity 
892/(892+65)
#specificity
37/(39+37)

library(class)
c_predict <- knn(train = c_train,test = c_test, cl=c_train_label, k=2)
summary(c_predict)

library(gmodels)
CrossTable(x=c_test_label,y=c_predict, prop.chisq = FALSE)

#accuracy for k=2
(955+39)/1033
#sensitivity
955/(955+2)
#specificity
39/(39+37)

#10-fold CV with knn
library(caret)

tc <- trainControl(method="cv",number=10)
knn_fit<- train(course_type ~ .,
                method="knn",
                tuneGrid   = expand.grid(k = 1:10),
                trControl=tc,
                metric="Accuracy",
                data=courses)
knn_fit
