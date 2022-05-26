library(MASS)
library(caret)
library(tree)
library(randomForest)
library(e1071)
library(cluster)
library(factoextra)
library(neuralnet)
library(ggplot2)

########################
# 1.  Data Preparation #
########################

# a.  Load the dataset insurance.csv into memory.

library(readr)
insurance <- read_csv("C:/Users/Justin/Google Drive/School/Data Science/Predective Modeling/insurance.csv")
View(insurance)


# b.  In the data frame, transform the variable charges by setting
#     insurance$charges = log(insurance$charges). Do not transform
#     it outside of the data frame.

insurance$charges <- log(insurance$charges)


# c.  Using the data set from 1.b, use the model.matrix() function
#     to create another data set that uses dummy variables in place
#     of categorical variables. Verify that the first column only has
#     ones (1) as values, and then discard the column only after
#     verifying it has only ones as values.

insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)

insurance.mm <- model.matrix(~ age + sex + bmi + children + smoker + region + charges, data = insurance)
insurance.mm <- insurance.mm[,-1]


# d.  Use the sample() function with set.seed equal to 1 to generate
#     row indexes for your training and tests sets, with 2/3 of the
#     row indexes for your training set and 1/3 for your test set. Do
#     not use any method other than the sample() function for
#     splitting your data.

set.seed(1)
index <- sample(1:nrow(insurance), nrow(insurance) * 2/3)


# e.  Create a training and test data set from the data set created in
#     1.b using the training and test row indexes created in 1.d.
#     Unless otherwise stated, only use the training and test
#     data sets created in this step.

insuranceTrain <- insurance[index,]
insuranceTest <- insurance[-index,]


# f.  Create a training and test data set from data set created in 1.c
#     using the training and test row indexes created in 1.d

insurance.mmTrain <- insurance.mm[index,]
insurance.mmTest <- insurance.mm[-index,]


#################################################
# 2.  Build a multiple linear regression model. #
#################################################

# a.  Perform multiple linear regression with charges as the
#     response and the predictors are age, sex, bmi, children,
#     smoker, and region. Print out the results using the
#     summary() function. Use the training data set created in
#     step 1.e to train your model.

lm.Q2 <- lm(charges ~ age + sex + bmi + children + smoker + region, data = insuranceTrain)
summary(lm.Q2)

# b.  Is there a relationship between the predictors and the
#     response?
# Answer: According to the p-value (2.2e-16), there is a 
#         relationship between the predictors and the response.


# c.  Does sex have a statistically significant relationship to the
#     response?
# Answer: According to the p-value (2e-16), there is a statistically 
#         significant relationship between 'sex' and the response.


# d.  Perform best subset selection using the stepAIC() function
#     from the MASS library, choose best model based on AIC. For
#     the "direction" parameter in the stepAIC() method, set
#     direciton="backward"
# Answer: The best model contains all of the above predictors. 
#         Those are age, sex, bmi, children, smoker, and region.

lm.bwdQ2 <- stepAIC(lm.Q2, direction = "backward")


# e.  Compute the test error of the best model in 2.d based on AIC
#     using LOOCV using trainControl() and train() from the caret
#     library. Report the MSE by squaring the reported RMSE.
# Answer: The MSE is 0.1791349
trainControlLOOCV <- trainControl(method = "LOOCV")
bestModel.LOOCVQ2 <- train(charges ~ age + sex + bmi + children + smoker + region, data = insuranceTrain, trControl = trainControlLOOCV, method = "lm")
print(bestModel.LOOCVQ2)
(bestModel.LOOCVQ2$results$RMSE)^2

# f.  Calculate the test error of the best model in 2.d based on AIC
#     using 10-fold Cross-Validation. Use train and trainControl
#     from the caret library. Refer to model selected in 2.d based
#     on AIC. Report the MSE.
# Answer: The MSE is 0.1776178

trainControl10 <- trainControl(method = "CV", number = 10)
bestModel.CV10Q2 <- train(charges ~ age + sex + bmi + children + smoker + region, data = insuranceTrain, trControl = trainControl10, method = "lm")
print(bestModel.CV10Q2)
(bestModel.CV10Q2$results$RMSE)^2

# g.  Calculate and report the test MSE using the best model from 
#     2.d and the test data set from step 1.e.
# Answer: The MSE is 1.517801

predict.Q2 <- predict(lm.Q2, data = insuranceTest)
observ.testCharges <- insuranceTest$charges
mean((observ.testCharges - predict.Q2)^2)

# h.  Compare the test MSE calculated in step 2.f using 10-fold
#     cross-validation with the test MSE calculated in step 2.g.
#     How similar are they?
# Answer: They are not even close. The MSE for 10-fold cross-validation 
#         (0.1776178) is significantly lower than the MSE from the best 
#         model without train control (1.517801).


######################################
# 3.  Build a regression tree model. #
######################################

# a.  Build a regression tree model using function tree(), where
#     charges is the response and the predictors are age, sex, bmi,
#     children, smoker, and region.

tree.Q3 <- tree(charges ~ age + sex + bmi + children + smoker + region, data = insuranceTrain)

# b.  Find the optimal tree by using cross-validation and display
#     the results in a graphic. Report the best size.
# Answer: The optimal tree size is 3.

cv.bestTree <- cv.tree(tree.Q3)
plot(cv.bestTree$size, cv.bestTree$dev, type = 'b')

# c.  Justify the number you picked for the optimal tree with
#     regard to the principle of variance-bias trade-off.
# Answer: The optimal tree size is 3 there is a large deviance between 
#         2 and 3, but not enough between 3 and 4 to justify more complexity.


# d.  Prune the tree using the optimal size found in 3.b

prune.Q3 <- prune.tree(tree.Q3, best = 3)


# e.  Plot the best tree model and give labels.
plot(prune.Q3)
text(prune.Q3, pretty = 0)

# f.  Calculate the test MSE for the best model.
# Answer: The MSE for the best tree model is 1.436819

predict.Q3 <- predict(prune.Q3, data = insuranceTest)
mean((observ.testCharges - predict.Q3)^2)


####################################
# 4.  Build a random forest model. #
####################################

# a.  Build a random forest model using function randomForest(),
#     where charges is the response and the predictors are age, sex,
#     bmi, children, smoker, and region.

rf.Q4 <- randomForest(charges ~ age + sex + bmi + children + smoker + region, data = insuranceTrain, importance = TRUE)

# b.  Compute the test error using the test data set.
# Answer: The MSE for the random forest model is 1.426969

predict.Q4 <- predict(rf.Q4, data = insuranceTest)
mean((observ.testCharges - predict.Q4)^2)


# c.  Extract variable importance measure using the importance()
#     function.

importance(rf.Q4)


# d.  Plot the variable importance using the function, varImpPlot().
#     Which are the top 3 important predictors in this model?
# Answer: The top 3 most important predictors are smoker, age, and bmi.

varImpPlot(rf.Q4)


############################################
# 5.  Build a support vector machine model #
############################################

# a.  The response is charges and the predictors are age, sex, bmi,
#     children, smoker, and region. Please use the svm() function
#     with radial kernel and gamma=5 and cost = 50.

svm.Q5 <- svm(charges ~ age + sex + bmi + children + smoker + region, data = insuranceTrain, kernel = "radial", gamma = 5, cost = 10)


# b.  Perform a grid search to find the best model with potential
#     cost: 1, 10, 50, 100 and potential gamma: 1,3 and 5 and
#     potential kernel: "linear","polynomial","radial" and
#     "sigmoid". And use the training set created in step 1.e.

tune.Q5 <- tune(svm, charges ~ age + sex + bmi + children + smoker + region, data = insuranceTrain, ranges = list(kernel = c("linear","radial","sigmoid"), cost = c(1,10,50,100), gamma = c(1,3,5)))

# c.  Print out the model results. What are the best model
#     parameters?
# Answer: The best model parameters are kernel = radial, cost = 1, gamma = 1

summary(tune.Q5)

# d.  Forecast charges using the test dataset and the best model
#     found in c).

predict.Q5 <- predict(tune.Q5$best.model, data = insuranceTest)


# e.  Compute the MSE (Mean Squared Error) on the test data.
# Answer: The MSE of this model is 1.566672

mean((observ.testCharges - predict.Q5)^2)


#############################################
# 6.  Perform the k-means cluster analysis. #
#############################################

# a.  Use the training data set created in step 1.f and standardize
#     the inputs using the scale() function.

scaled.insurance.mmTrain <- scale(insurance.mmTrain)

# b.  Convert the standardized inputs to a data frame using the
#     as.data.frame() function.

scaled.insurance.mmTrain <- as.data.frame(scaled.insurance.mmTrain)

# c.  Determine the optimal number of clusters, and use the
#     gap_stat method and set iter.max=20. Justify your answer.
#     It may take longer running time since it uses a large dataset.

my.kmeans <- function(x,k) list(cluster = kmeans(x,k,iter.max = 20))
fviz_nbclust(scaled.insurance.mmTrain, my.kmeans, method = "gap_stat")

# d.  Perform k-means clustering using the optimal number of
#     clusters found in step 6.c. Set parameter nstart = 25

km.Q6 <- kmeans(scaled.insurance.mmTrain, 5, nstart = 25)

# e.  Visualize the clusters in different colors, setting parameter
#     geom="point"

fviz_cluster(km.Q6, data = scaled.insurance.mmTrain, geom = "point")

######################################
# 7.  Build a neural networks model. #
######################################

# a.  Using the training data set created in step 1.f, create a 
#     neural network model where the response is charges and the
#     predictors are age, sexmale, bmi, children, smokeryes, 
#     regionnorthwest, regionsoutheast, and regionsouthwest.
#     Please use 1 hidden layer with 1 neuron. Do not scale
#     the data.

nn.Q7 <- neuralnet(charges ~ age + sexmale + bmi + children + smokeryes + regionnorthwest + regionsoutheast + regionsouthwest, data = insurance.mmTrain, hidden = c(1))

# b.  Plot the neural network.

plot(nn.Q7)

# c.  Forecast the charges in the test dataset.

predict.Q7 <- compute(nn.Q7, insurance.mmTest[,c("age","sexmale","bmi","children","smokeryes","regionnorthwest","regionsoutheast","regionsouthwest")])

# d.  Compute test error (MSE).
# Answer: The MSE for this model is 0.8516586

observ.mmtestCharges <- insurance.mmTest[,"charges"]
mean((observ.mmtestCharges - predict.Q7$net.result)^2)

################################
# 8.  Putting it all together. #
################################

# a.  For predicting insurance charges, your supervisor asks you to
#     choose the best model among the multiple regression,
#     regression tree, random forest, support vector machine, and
#     neural network models. Compare the test MSEs of the models
#     generated in steps 2.g, 3.f, 4.b, 5.e, and 7.d. Display the names
#     for these types of these models, using these labels:
#     "Multiple Linear Regression", "Regression Tree", "Random Forest", 
#     "Support Vector Machine", and "Neural Network" and their
#     corresponding test MSEs in a data.frame. Label the column in your
#     data frame with the labels as "Model.Type", and label the column
#     with the test MSEs as "Test.MSE" and round the data in this
#     column to 4 decimal places. Present the formatted data to your
#     supervisor and recommend which model is best and why.

test.MSE <- data.frame(Model_Type = c("Multiple Linear Regression","Regression Tree","Random Forest","Support Vector Machine","Neural Network"), Test_MSE = c(round(c(1.517801,1.436819,1.426969,1.566672,0.8516586), digits = 4)))


# b.  Another supervisor from the sales department has requested
#     your help to create a predictive model that his sales
#     representatives can use to explain to clients what the potential
#     costs could be for different kinds of customers, and they need
#     an easy and visual way of explaining it. What model would
#     you recommend, and what are the benefits and disadvantages
#     of your recommended model compared to other models?

salesData <- lm.Q2
salesData$model$charges <- exp(salesData$model$charges)
ggplot(data = salesData, aes(x = age, y = charges, color = smoker)) + geom_point() + geom_smooth(aes(age, charges))

# c.  The supervisor from the sales department likes your regression
#     tree model. But she says that the sales people say the numbers
#     in it are way too low and suggests that maybe the numbers
#     on the leaf nodes predicting charges are log transformations
#     of the actual charges. You realize that in step 1.b of this
#     project that you had indeed transformed charges using the log
#     function. And now you realize that you need to reverse the
#     transformation in your final output. The solution you have
#     is to reverse the log transformation of the variables in 
#     the regression tree model you created and redisplay the result.
#     Follow these steps:
#
#     i.   Copy your pruned tree model to a new variable.

salesTree <- prune.Q3


#     ii.  In your new variable, find the data.frame named
#          "frame" and reverse the log transformation on the
#          data.frame column yval using the exp() function.
#          (If the copy of your pruned tree model is named 
#          copy_of_my_pruned_tree, then the data frame is
#          accessed as copy_of_my_pruned_tree$frame, and it
#          works just like a normal data frame.).

salesTree$frame$yval <- exp(salesTree$frame$yval)


#     iii. After you reverse the log transform on the yval
#          column, then replot the tree with labels.

plot(salesTree)
text(salesTree)
