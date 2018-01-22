library(readxl)
data1 <- read_excel("song_classification.xlsx", sheet=1, col_names=T)
data2 <- read_excel("Attr_data.xlsx", sheet=1, col_names=T)
#Create data for training
sample.ind = sample(2,  
                    nrow(data2),
                    replace = T,
                    prob = c(0.7,0.3))
data.dev = data2[,6:20][sample.ind==1,]  
data.val = data2[,6:20][sample.ind==2,]  

# Original Data
table(data2$className)/nrow(data2)
# Training Data
table(data.dev$className)/nrow(data.dev)  
# Testing Data
table(data.val$className)/nrow(data.val) 

#Fit Random Forest Model
#install.packages("randomForest")
library(randomForest)
data.dev$className = factor(data.dev$className)
rf = randomForest(className ~ .,  
                  ntree = 800,
                  data = data.dev[,-14])
plot(rf)
print(rf)

# Variable Importance
varImpPlot(rf,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")

#Variable Importance
var.imp = data.frame(importance(rf,  
                                type=2))
# make row names as columns
var.imp$Variables = row.names(var.imp)  
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])

# Predicting response variable
data.dev$predicted.response = predict(rf , data.dev[,-14])

# Create Confusion Matrix
#install.packages('e1071', dependencies=TRUE) 
library(caret)
print(  
  confusionMatrix(data = data.dev$predicted.response,  
                  reference = data.dev$className,
                  positive = 'className'))

# Predicting response variable
data.val$predicted.response <- predict(rf ,data.val[,-14])

# Create Confusion Matrix
print(  
  confusionMatrix(data=data.val$predicted.response,  
                  reference=data.val$className,
                  positive='className'))
##########################################################################################
library(randomForest)
data.all = data2[,6:20] 
data.all = na.omit(data.all)
data.all$className = factor(data.all$className)
rf1 = randomForest(className ~ .,  
                  ntree = 800,
                  data = data.all[,-14])
plot(rf1)
print(rf1)

data.all$predicted.response <- predict(rf1 ,data.all[,-14])

# Create Confusion Matrix
print(  
  confusionMatrix(data=data.all$predicted.response,  
                  reference=data.all$className,
                  positive='className')) 

#######################################################################################
data.new <- data1[2:14]
data.new <- na.omit(data.new)
data.new$className <- predict(rf1 ,data.new) 

#######################################################################################
data2 = na.omit(data2)
mydata1 <- cbind(data2[,3:5], data.all)
mydata2 <- cbind(data1[,16:17], data.new)
colnames(mydata1)[1] = "song_title" 
finalData.attr <- merge(mydata1, mydata2, all = T)
x <- unique(x, by = "song_title")
x = x[,1:16]

X2 <- x [!duplicated(x[,c("song_title","className")]),]
library(xlsx)
write.xlsx(X2, "c:\\Users\\achaina\\Desktop\\MS BAIM\\R\\audio features\\X2_data.xlsx")

#######################################################################################

