column_names <- c(
  "Case", "Brozek", "Siri",
  "Density", "Age", "Weight", "Height", "Adiposity_Index",
  "Fat_Free_Weight", "Neck", "Chest","Abdomen", "Hip", "Thigh",
  "Knee", "Ankle", "Biceps","Forearm", "Wrist"
)
data <- read.table("C:\\Users\\chanu\\OneDrive\\Desktop\\STAT4110\\fat.dat.txt", header = FALSE, col.names = column_names)
head(data)

#cleaning the data

# 1) calculating the density value and correcting them 

# i) case 48 

# from Brozek equation;
density1 <- 457/(6.4 + 414.2)
density1
#from siri's equation;
density2<- 495/(5.6 + 450)
density2

corrected_density_48 <- round((density1+ density2)/2,4)
corrected_density_48

data$Density[data$Case==48] <- corrected_density_48

#ii) case 76

# from Brozek equation;
density1 <- 457/(18.3 + 414.2)
density1
#from siri's equation;
density2<- 495/(18.5 + 450)
density2

corrected_density_76 <- round((density1+ density2)/2,4)

corrected_density_76

data$Density[data$Case==76] <- corrected_density_76

#iii) case 96

# from Brozek equation;
density1 <- 457/(17.3 + 414.2)
density1
#from siri's equation;
density2<- 495/(17.4 + 450)
density2

corrected_density_96 <- round((density1+ density2)/2,4)
corrected_density_96

data$Density[data$Case==96] <- corrected_density_96


#2) Correcting the height in case 42

data$Height[data$Case == 42] <- 69.5

#3) row 182 with 0 as fat percentages 

# We can more accurate insight to the data if we delete this data, as fat percentages cannot be negative 

data <- data[-182,]



library(ggplot2)

# Plot for Brozek's body fat percentage
ggplot(data, aes(x = Brozek)) +
  geom_histogram(aes(y = ..density..), fill = "orange", color = "black", bins = 20, alpha = 0.5) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Distribution of Body Fat Percentage (Brozek)",
    x = "Body Fat Percentage (Brozek)",
    y = "Density"
  ) +
  theme_minimal()

# Plot for Siri's body fat percentage
ggplot(data, aes(x = Siri)) +
  geom_histogram(aes(y = ..density..), fill = "orange", color = "black", bins = 20, alpha = 0.5) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Distribution of Body Fat Percentage (Siri)",
    x = "Body Fat Percentage (Siri)",
    y = "Density"
  ) +
  theme_minimal()


measurements <- c("Weight", "Height", "Adiposity_Index", "Neck",
                  "Chest", "Abdomen", "Hip", "Thigh", "Knee", "Ankle", "Biceps", "Forearm", "Wrist")

for (measure in measurements){
  print(
    ggplot(data,aes_string(x=measure,y="Brozek"))+
      geom_point(color="blue",size=2)+
      geom_smooth(method="lm",color="red",se=FALSE)+
      labs(x=measure,
           y="body fat percentage (Borzek)",
           title=paste(measure,"vs Body Fat Percentage"))+
      theme_minimal(),
    
  )
}

ggplot(data,aes(x= Age,y= Brozek))+
  geom_point(color="skyblue",size=2)+
  geom_smooth(method="lm",color="red",se=FALSE)+
  labs(x="Age",
       y="body fat percentage (Borzek)",
       title= "Age vs Body Fat Percentage")+
  theme_minimal()

max(data$Age)
min(data$Age)

data$Age_Group <- cut(data$Age, breaks = c(20, 30, 40, 50, 60, 70,80,90), labels = c("20-30", "30-40", "40-50", "50-60", "60-70","70-80","80-90"))

ggplot(data, aes(x = Age_Group, y = Brozek)) +
  geom_boxplot(fill = c("pink", "lightgreen", "lightblue", "yellow", "orange",
                        "purple","green")) +
  labs(
    title = "Body Fat Percentage by Age Group",
    x = "Age Group",
    y = "Body Fat Percentage (Brozek)"
  ) +
  theme_minimal()

data <- data[,-20]
pairs(data)
cor(data[,-1])


install.packages("reshape2")
library(reshape2)


numeric_data<- data[,-1]

cor_matrix <- cor(numeric_data)

heatmap(cor_matrix, 
        col = heat.colors(256),  # Color palette
        main = "Correlation Heatmap", 
        xlab = "Variables", 
        ylab = "Variables", 
        margins = c(5, 5))  # Adjust margins


# First linear model with density 

fit1 <- lm(Brozek~ Density,data=data)
summary(fit1)

# Second Linear Model with other variables

fit2 <- lm(Brozek~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Biceps+Forearm+Wrist,data=data)
summary(fit2)

library(leaps)

leaps <-regsubsets(Brozek~Age+Weight+Height+Neck+Chest+Hip+Abdomen+Thigh+Knee+Ankle+Biceps+Forearm+Wrist, data=data, nbest=1)
plot(leaps, scale="adjr2")

lm_model <-lm (Brozek~Age+Weight+Neck+Hip+Abdomen+Thigh+Forearm+Wrist,data=data)
summary(lm_model)
coef(lm_model)

library(car)
library(leaps)
outlierTest(lm_model)

hat.plot <- function(fit,mydata) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  text(hatvalues(fit), labels=rownames(mydata), cex=0.9, font=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(lm_model, data)

( cutoff <- 4/(nrow(data)-length(lm_model$coefficients)-2) )
plot(lm_model, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")



#Model Evaluation

#1.linear regression

set.seed(123) 
n <- nrow(data)
train_indices <- sample(1:n, size = 0.7* n) # 70% training data
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

lm_model <- lm(Brozek~Age+Weight+Neck+Hip+Abdomen+Thigh+Forearm+Wrist, data = train_data)
summary(lm_model)

actual <- test_data$Brozek
lm_test_predictions <- predict(lm_model, newdata = test_data)


lm_test_ss_total <- sum((actual - mean(actual))^2)
lm_test_ss_residual <- sum((actual - test_predictions)^2)
lm_test_r_squared <- 1 - (lm_test_ss_residual / lm_test_ss_total)
print(paste("Ridge Regression Test R²:", lm_test_r_squared))

plot(actual, test_predictions, 
     main = "Actual vs Predicted",
     xlab = "Actual Values",
     ylab = "Predicted Values",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)


#2.ridge regression 

set.seed(123)
X_train <- as.matrix(train_data[, c("Age", "Weight", "Height", "Neck", "Chest", "Hip", "Abdomen", "Thigh", "Knee", "Ankle", "Biceps", "Forearm", "Wrist")])
y_train <- train_data$Brozek
X_test <- as.matrix(test_data[, c("Age", "Weight", "Height", "Neck", "Chest", "Hip", "Abdomen", "Thigh", "Knee", "Ankle", "Biceps", "Forearm", "Wrist")])
y_test <- test_data$Brozek

library(glmnet)


ridge_model <- glmnet(X_train, y_train, alpha = 0)
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0)
best_lambda_ridge <- cv_ridge$lambda.min


ridge_test_predictions <- predict(ridge_model, s = best_lambda_ridge, newx = X_test)


ridge_test_ss_total <- sum((y_test - mean(y_test))^2)
ridge_test_ss_residual <- sum((y_test - ridge_test_predictions)^2)
ridge_test_r_squared <- 1 - (ridge_test_ss_residual / ridge_test_ss_total)

coef(ridge_model, s = best_lambda_ridge)
print(paste("Ridge Regression Test R²:", ridge_test_r_squared))



# 3. Lasso Regression
set.seed(123)
lasso_model <- glmnet(X_train, y_train, alpha = 1)
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1)
best_lambda_lasso <- cv_lasso$lambda.min


lasso_test_predictions <- predict(lasso_model, s = best_lambda_lasso, newx = X_test)


lasso_test_ss_total <- sum((y_test - mean(y_test))^2)
lasso_test_ss_residual <- sum((y_test - lasso_test_predictions)^2)
lasso_test_r_squared <- 1 - (lasso_test_ss_residual / lasso_test_ss_total)

coef(lasso_model, s = best_lambda_lasso)
print(paste("Lasso Regression Test R²:", lasso_test_r_squared))

#All Models Evaluations 

cat("Linear Model Test R²:", lm_test_r_squared, "\n")
cat("Ridge Model Test R²:", ridge_test_r_squared, "\n")
cat("Lasso Model Test R²:", lasso_test_r_squared, "\n")

results_df <- data.frame(
  Actual = rep(y_test, 3),
  Predicted = c(lm_test_predictions, ridge_test_predictions, lasso_test_predictions),
  Model = rep(c("Linear Regression", "Ridge Regression", "Lasso Regression"), each = length(y_test))
)

ggplot(results_df, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Predicted vs Actual: LM, Ridge, and Lasso Models", x = "Actual", y = "Predicted") +
  theme(legend.position = "top")


r2_values <- data.frame(
  Model = c("Linear Regression", "Ridge Regression", "Lasso Regression"),
  R2 = c(lm_test_r_squared,ridge_test_r_squared,lasso_test_r_squared)
)

# Plotting the histogram (or bar plot)
ggplot(r2_values, aes(x = Model, y = R2, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
  labs(title = "R-squared Comparison for LM, Ridge, and Lasso Models", 
       x = "Model", 
       y = "R-squared (Explained Variance)") +
  theme_minimal() +
  theme(legend.position = "none")

par(mfrow=c(2,2))
plot(lm_model)

plot(ridge_model)

plot(lasso_model)




