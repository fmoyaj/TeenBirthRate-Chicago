# Data Science Capstone 2022 - Francisca Moya Jimenez with contributions from Andrea Mock

## Loading the Merged Dataset ####
chicago.data <- read.csv('/Users/francisca/Desktop/STAT 318/Final Project/Chicago_data_by_community_cleaned.csv',
                         header = TRUE)

## Data Cleaning ####

# NA's
library(dplyr)
chicago.data %>% is.na() %>% colSums() # Identifying predictors with NAs

which(is.na(chicago.data$Childhood.Blood.Lead.Level.Screening)) # Index: 54
which(is.na(chicago.data$Childhood.Lead.Poisoning)) # Index: 54
chicago.data[54,] # Riverdale

# From the write.csv() in cleaning, an X column with an index was added. Removing that column.
chicago.data <- chicago.data[2:34]
colnames(chicago.data)

# Visualizing NA's
library(VIM)
aggr(chicago.data, combined = TRUE, numbers = TRUE, labels = TRUE, cex.axis= 0.2)

# Copying original dataset
chicago.clean <- chicago.data


# Imputation of NA's for missing variables since there is more than 15% missingness
sum(chicago.data$Gonorrhea.in.Males == ".")/nrow(chicago.data) # Rows with 15% missingness are candidates for deletion
sum(is.na(chicago.data$Gonorrhea.in.Females))/nrow(chicago.data) # Rows with 15% missingness are candidates for deletion
chicago.clean <- chicago.clean[, -c(21, 22)] # Removing variables above

chicago.clean$Childhood.Blood.Lead.Level.Screening[54] <- mean(na.omit(chicago.data$Childhood.Blood.Lead.Level.Screening))
chicago.clean$Childhood.Lead.Poisoning[54] <- median(na.omit(chicago.data$Childhood.Lead.Poisoning))
chicago.clean[54,]

# Dealing with multicollinearity by calculating VIF (backward)
chicago.multicollinearity <- chicago.clean %>% dplyr::select( -Community.Area, -Community.Area.Name,
                                                      -Teen.Birth.Rate)

# Multicollinearity: stepwise elimination for VIF
library(usdm)
vifstep(chicago.multicollinearity) # Variables to remove: Assault..Homicide. Cancer..All.Sites. No.High.School.Diploma General.Fertility.Rate Life.Expectancy Vulnerability.Index

# Dataframe without multicollinearity issues
chicago.reduced <- chicago.clean[, -c(9, 11, 25, 4, 28, 31)]

## Model Fitting ####
# Create a subset of response and predictors
attach(chicago.reduced)
predictors <- chicago.reduced[, 3:25][, -5]
full.model <- lm(Teen.Birth.Rate~., data = predictors)
# Automatic selection
step(full.model, direction = "both", trace = 0, k=2) # AIC
step(full.model, direction = "both", trace = 0, k=log(nrow(predictors))) # BIC

# Mallow's Cp
library(leaps)
result.cp <- leaps(x = predictors, y = chicago.clean$Teen.Birth.Rate, int = TRUE,
      method = "Cp", nbest = 10)
which.min(result.cp$Cp)
result.cp$which[61,]
colnames(predictors[c(1,3, 5, 7, 8, 17, 20)])

# R-squared Adjusted
result.r2adj <- leaps(x = predictors, y = chicago.clean$Teen.Birth.Rate, int = TRUE,
                   method = "adjr2", nbest = 10)
which.max(result.r2adj$adjr2)
result.r2adj$which[101,]
colnames(predictors[c(1,2,3,5,7,8,9,17,20,21,22)])

## 5-fold Cross-Validation ####
# Building folds
mydata <- chicago.reduced
Y <- Teen.Birth.Rate # Response
n <- nrow(chicago.reduced) # Sample size
K <- 5 #5-fold CV
n.fold <- floor(n/K)

set.seed(4657) # Set seed for reproducibility
n.shuffle <- sample(1:n, n, replace=FALSE) #Shuffle the n indexes
index.fold <- list()

for(i in 1:K)
{
  if(i<K) {
    index.fold[[i]] <- n.shuffle[((i-1)*n.fold+1):(i*n.fold)] }
  else
  {
    index.fold[[i]] <- n.shuffle[((K-1)*n.fold+1):n]
  }
}

# Mallow's Cp
CV.score.mallow <- 0
for(i in 1:K) {
  #Dit the full model based on the data excluding the ith fold
  fit <- lm(Teen.Birth.Rate ~ Birth.Rate + Prenatal.Care.Beginning.in.First.Trimester +
              Breast.cancer.in.females + Diabetes.related + Firearm.related +
              Crowded.Housing + Unemployment, data=mydata[-index.fold[[i]],])
  pred <- predict(fit,mydata[index.fold[[i]],])
  #compute average squared error for the ith fold
  CV.score.mallow <- CV.score.mallow +(1/n)*sum((Y[index.fold[[i]]]-pred)^2)
}

# Adjusted R-squared
CV.score.r2 <- 0
for(i in 1:K) {
  #Dit the full model based on the data excluding the ith fold
  fit.r <- lm(Teen.Birth.Rate~Birth.Rate + Low.Birth.Weight + Prenatal.Care.Beginning.in.First.Trimester +
                Breast.cancer.in.females + Diabetes.related + Firearm.related +
                Infant.Mortality.Rate + Crowded.Housing + Unemployment +
                Number.Affordable.Buildings + Number.Affordable.Units, data=mydata[-index.fold[[i]],]) #make prediction on each observation in the ith fold
  pred.r <- predict(fit.r,mydata[index.fold[[i]],])
  #compute average squared error for the ith fold
  CV.score.r2 <- CV.score.r2+(1/n)*sum((Y[index.fold[[i]]]-pred.r)^2)
}

# AIC
CV.score.aic<- 0
for(i in 1:K) {
  fit.aic <- lm(Teen.Birth.Rate~Birth.Rate + Prenatal.Care.Beginning.in.First.Trimester +
                  Breast.cancer.in.females + Diabetes.related + Firearm.related +
                  Crowded.Housing + Unemployment + Number.Affordable.Buildings, data=mydata[-index.fold[[i]],]) #make prediction on each observation in the ith fold
  pred.aic <- predict(fit.aic,mydata[index.fold[[i]],])
  #compute average squared error for the ith fold
  CV.score.aic <- CV.score.aic+(1/n)*sum((Y[index.fold[[i]]]-pred.aic)^2)
}

# BIC --- SAME MODEL AS MALLOW'S CP
CV.score.bic<- 0
for(i in 1:K) {
  #fit the full model based on the data excluding the ith fold
  fit.bic <- lm(Teen.Birth.Rate ~ Birth.Rate + Prenatal.Care.Beginning.in.First.Trimester + Breast.cancer.in.females +
                  Diabetes.related + Firearm.related + Crowded.Housing + Unemployment, data=mydata[-index.fold[[i]],]) #make prediction on each observation in the ith fold
  pred.bic <- predict(fit.bic,mydata[index.fold[[i]],])
  #compute average squared error for the ith fold
  CV.score.bic <- CV.score.bic+(1/n)*sum((Y[index.fold[[i]]]-pred.bic)^2)
}

# Finding min CV score
which.min(c(CV.score.mallow, CV.score.r2, CV.score.aic, CV.score.bic)) #CV.score.mallow


# Model chosen (Mallow's Cp, BIC): Teen.Birth.Rate~ Birth.Rate
# + Prenatal.Care.Beginning.in.First.Trimester + Breast.cancer.in.females +
# Diabetes.related + Firearm.related + Crowded.Housing + Unemployment

best.lm <- lm(Teen.Birth.Rate~Birth.Rate + Prenatal.Care.Beginning.in.First.Trimester +
                Breast.cancer.in.females + Diabetes.related + Firearm.related + Crowded.Housing +
                Unemployment, data = chicago.reduced)

summary(best.lm) # All predictors are significant

## Regression Assumptions ####
# Residual plot
plot(resid(best.lm)~fitted(best.lm), main = "Residual Plot for Best Model",
     ylab = "Residuals", xlab = "Fitted Values")
abline(h = 0)

# Q-Q plot
qqnorm(resid(best.lm))
qqline(resid(best.lm), col = "red")

# Checking for multicollinearity again
pred <- data.frame(Birth.Rate, Prenatal.Care.Beginning.in.First.Trimester,
                     Breast.cancer.in.females, Diabetes.related, Firearm.related, Crowded.Housing,
                     Unemployment)
vifstep(pred)

## Outlier Detection ####
# Y-direction
residuals.y <-rstudent(best.lm)
df <- nrow(chicago.clean) - length(best.lm$coefficients)
outliers.y <- which(residuals.y > qt(0.975, df) | residuals.y < qt(0.025, df)) # Higher than cutoff at 95% confidence

# X-direction
leverage.x <- hatvalues(best.lm)
outliers.x <- which(leverage.x > 2*mean(leverage.x))

# Identifying influential residuals
cooks.D <- cooks.distance(best.lm)
cutoff <- qf(0.5, df1 = 7, df2= 70) # df = p, df2 = n-p from notes, 50th percentile
which(cooks.D[c(outliers.y, outliers.x)] > cutoff) # No outlying value seems to be influential

# Studentized deleted residuals v. fitted values plot
par(mfrow=c(1,1))
plot(residuals.y~fitted(best.lm),
     ylab = "Studentized deleted residuals",
     xlab = "Fitted values",
     main = "Studentized Deleted Residuals v. Fitted Values")
abline(h=0, col = "red")

# Looking at available plots for model

# Possible influential outliers: 5, 22, and 30 -- North Center (lower than predicted), Logan Square (higher than expected), South Lawndale (Little Village) (higher than expected)


## Model Refinement: Fitting a Model With Interactions ####
chosenPredictors <- data.frame(Birth.Rate, Prenatal.Care.Beginning.in.First.Trimester,
                               Breast.cancer.in.females, Diabetes.related, Firearm.related, Crowded.Housing,
                               Unemployment) # Use predictors found by best first-order model
additive.lm <- lm(Teen.Birth.Rate~., data = chosenPredictors) # Set additive model as lower bound and model with all pairwise interactions as upper bound
step(additive.lm,.~.^2,direction="both",k=2, trace = 0) # AIC
step(additive.lm,.~.^2,direction="both",k=log(nrow(chosenPredictors)), trace = 0) # BIC

# Fitting models
AIC.lm <- lm(Teen.Birth.Rate ~ Birth.Rate + Prenatal.Care.Beginning.in.First.Trimester +
               Breast.cancer.in.females + Diabetes.related + Firearm.related +
               Crowded.Housing + Unemployment + Breast.cancer.in.females:Firearm.related +
               Prenatal.Care.Beginning.in.First.Trimester:Diabetes.related +
               Birth.Rate:Firearm.related + Breast.cancer.in.females:Crowded.Housing,
             data = chicago.reduced)
summary(AIC.lm)
BIC.lm <- lm(Teen.Birth.Rate ~ Birth.Rate + Prenatal.Care.Beginning.in.First.Trimester +
               Breast.cancer.in.females + Diabetes.related + Firearm.related +
               Crowded.Housing + Unemployment + Breast.cancer.in.females:Firearm.related +
               Prenatal.Care.Beginning.in.First.Trimester:Diabetes.related,
             data = chicago.reduced)
summary(BIC.lm)

# Significant model based on suggested by AIC
significantAIC.lm <- lm(Teen.Birth.Rate ~ Birth.Rate + Prenatal.Care.Beginning.in.First.Trimester +
                          Breast.cancer.in.females + Diabetes.related + Firearm.related +
                          Crowded.Housing + Unemployment + Breast.cancer.in.females:Firearm.related +
                          Prenatal.Care.Beginning.in.First.Trimester:Diabetes.related,
                        data = chicago.reduced)
summary(significantAIC.lm)
anova(AIC.lm, significantAIC.lm) # Can we eliminate predictors from original AIC model? Yes
plot(significantAIC.lm)
plot(BIC.lm)

# Final 2 models with interactions
summary(significantAIC.lm)
summary(BIC.lm)

# We can look at 5-fold CV scores
CV.score.intBIC <- 0
for(i in 1:K) {
  fit <- lm(Teen.Birth.Rate ~ Birth.Rate + Prenatal.Care.Beginning.in.First.Trimester +
              Breast.cancer.in.females + Diabetes.related + Firearm.related +
              Crowded.Housing + Unemployment + Breast.cancer.in.females:Firearm.related +
              Prenatal.Care.Beginning.in.First.Trimester:Diabetes.related
            , data=mydata[-index.fold[[i]],])
  pred <- predict(fit,mydata[index.fold[[i]],])
  #compute average squared error for the ith fold
  CV.score.intBIC <- CV.score.intBIC +(1/n)*sum((Y[index.fold[[i]]]-pred)^2)
}

CV.score.intBIC


CV.score.intAIC <- 0
for(i in 1:K) {
  fit <- lm(Teen.Birth.Rate ~ Birth.Rate + Prenatal.Care.Beginning.in.First.Trimester +
              Breast.cancer.in.females + Diabetes.related + Firearm.related +
              Crowded.Housing + Unemployment + Breast.cancer.in.females:Firearm.related +
              Prenatal.Care.Beginning.in.First.Trimester:Diabetes.related +
              Birth.Rate:Firearm.related
            , data=mydata[-index.fold[[i]],])
  pred <- predict(fit,mydata[index.fold[[i]],])
  #compute average squared error for the ith fold
  CV.score.intAIC <- CV.score.intAIC +(1/n)*sum((Y[index.fold[[i]]]-pred)^2)
}

CV.score.intAIC # Smaller CV score

plot(significantAIC.lm)
summary(significantAIC.lm)


## Outlier Detection for Interaction Model ####
# Y-direction
residuals.y <-rstudent(significantAIC.lm)
df <- nrow(chicago.clean) - length(significantAIC.lm$coefficients)
outliers.y <- which(residuals.y > qt(0.975, df) | residuals.y < qt(0.025, df)) # Higher than cutoff at 95% confidence

# X-direction
leverage.x <- hatvalues(significantAIC.lm)
outliers.x <- which(leverage.x > 2*mean(leverage.x))

# Identifying influential residuals
cooks.D <- cooks.distance(significantAIC.lm)
cutoff <- qf(0.5, df1 = 9, df2= 61) # df = p, df2 = n-p from notes, 50th percentile
which(cooks.D[c(outliers.y, outliers.x)] > cutoff) # No outlying value seems to be influential


## Regression Assumptions for Interaction Model ####
par(mfrow=c(1,2))
plot(residuals.y~fitted(significantAIC.lm), main = "Residual Plot of Model With Interactions",
     xlab = "Fitted values", ylab = "Studentized deleted residuals")
abline(h=0, col = "green")
qqnorm(significantAIC.lm$residuals)
qqline(significantAIC.lm$residuals, col = "red")



## Visualizations ####
library(sjPlot)
font_size(title = 12, axis_title.x = 14, axis_title.y = 14, labels.x = 12)
set_theme(legend.item.size = 0.5)
# First interaction term
plot1 <- plot_model(significantAIC.lm, type = "int", mdrt.values = "quart",
                    title = "Interaction Term: Breast Cancer in Females:Firearm-related Deaths",
                    axis.labels = c("Predicted teen birth rate", "Breast cancer in females"),
                    legend.title = "Quartiles of firearm-related deaths")[1] # 1st quartile, median, 3rd quartile
plot1
# Second interaction term
plot2 <- plot_model(significantAIC.lm, type = "int", mdrt.values = "quart", title = "Interaction Term: Prenatal Care Beginning in First Trimester:Diabetes-related Deaths",
                    legend.title = "Quartiles of Diabetes-related deaths")[2] # 1st quartile, median, 3rd quartile
plot2

# Distribution of response variable
boxplot(Teen.Birth.Rate, main = "Distribution of Teen Birth Rate in Chicago
        (Births per 1,000 Females Aged 15 to 19 Years)",
        ylab = "Rate", col = "plum4")
minimum <- which.min(Teen.Birth.Rate) # Index of min rate
maximum <- which.max(Teen.Birth.Rate) # Index of max rate
chicago.reduced[minimum,]
chicago.reduced[maximum,]

# ggplot's boxplot with jitter
library(viridis)
library(ggplot2)
data %>%
  ggplot(data = chicago.reduced, mapping = aes(x="", y = Teen.Birth.Rate)) +
  geom_boxplot(fill = "plum4") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_light() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Distribution of Teen Birth Rate in Chicago
(Births per 1,000 Females Aged 15 to 19 Years)") +
  xlab("") +
  ylab("Teen Birth Rate")


# Horizontal lollipop graph
# Ordered data
detach(chicago.reduced)
data <- data.frame(Community.Area = chicago.reduced$Community.Area.Name,
                   Teen.Birth.Rate = chicago.reduced$Teen.Birth.Rate)
data$Community.Area <- factor(data$Community.Area[order(-chicago.reduced$Teen.Birth.Rate)], levels = data$Community.Area[order(-chicago.reduced$Teen.Birth.Rate)])
data$Teen.Birth.Rate <- data$Teen.Birth.Rate[order(-chicago.reduced$Teen.Birth.Rate)]





#dev.new() # Show graph in new window
ggplot(data, aes(x=factor(Community.Area, levels = Community.Area[order(-chicago.reduced$Teen.Birth.Rate)]), y=Teen.Birth.Rate)) +
  geom_segment( aes(x=Community.Area, xend=Community.Area, y=0, yend=Teen.Birth.Rate), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    text = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(face="plain", color="#434343",
                               size=8, angle=0)
  ) +
  ylab("Teen Birth Rate") +
  xlab("Community Area") +
  ggtitle("Chicago Teen Birth Rate by Community Area")
#dev.off()


# Relationship with predictors in the final model
attach(chicago.reduced)

# Birth Rate
birth <- ggplot(chicago.reduced, aes(x=Birth.Rate, y=Teen.Birth.Rate)) +
  geom_point(color = "#53cf40") +
  theme(text = element_text(size = 11)) +
  xlab("Birth rate") +
  ylab("Teen birth rate") +
  ggtitle("Teen Birth Rate v. Birth Rate")
# Prenatal care
prenatal <- ggplot(chicago.reduced, aes(x=Prenatal.Care.Beginning.in.First.Trimester, y=Teen.Birth.Rate)) +
  geom_point(col = "#50c265") +
  theme(text = element_text(size = 11)) +
  xlab("Prenatal care in first trimester") +
  ylab("Teen birth rate") +
  ggtitle("Teen Birth Rate v.
Prenatal Care in First Trimester")
# Breast cancer
breast.cancer <- ggplot(chicago.reduced, aes(x=Breast.cancer.in.females, y=Teen.Birth.Rate)) +
  geom_point(col = "#4db48d") +
  theme(text = element_text(size = 11)) +
  xlab("Breast Cancer in Females") +
  ylab("Teen birth rate") +
  ggtitle("Teen Birth Rate v.
Breast Cancer in Females")
# Diabetes-Related Deaths
diabetes <- ggplot(chicago.reduced, aes(x=Diabetes.related, y=Teen.Birth.Rate)) +
  geom_point(col = "#4aa4bc") +
  theme(text = element_text(size = 11)) +
  xlab("Diabetes-related deaths") +
  ylab("Teen birth rate") +
  ggtitle("Teen Birth Rate v.
Diabetes-related Deaths")
# Crowded Housing
housing <- ggplot(chicago.reduced, aes(x=Crowded.Housing, y=Teen.Birth.Rate)) +
  geom_point(col = "#458cff") +
  theme(text = element_text(size = 11)) +
  xlab("Crowded housing") +
  ylab("Teen birth rate") +
  ggtitle("Teen Birth Rate v. Crowded Housing")
# Unemployment
unemployment <- ggplot(chicago.reduced, aes(x=Unemployment, y=Teen.Birth.Rate)) +
  geom_point(col = "#7a8aff") +
  theme(text = element_text(size = 11)) +
  xlab("Unemployment") +
  ylab("Teen birth rate") +
  ggtitle("Teen Birth Rate v. Unemployment")

require(gridExtra)
grid.arrange(birth, prenatal, breast.cancer, diabetes, housing, unemployment, ncol=3)

# Visualizing predictions
ggplot(chicago.reduced, aes(x=best.lm$fitted.values, y=Teen.Birth.Rate)) +
  geom_point(col = "#7a8aff") +
  geom_smooth(method = loess, se=FALSE) +
  xlab("Predictions for teen birth rate") +
  ylab("Teen birth rate") +
  ggtitle("Comparing Predictions and Values for Teen Birth Rate") +
  geom_abline(color = "red")



# Visualizing conditional relationship to response in first-order model
summary(best.lm)
par(mfrow=c(2,4))
# Model fitted by taking out Birth Rate as a predictor
m1 <- lm(Teen.Birth.Rate~Prenatal.Care.Beginning.in.First.Trimester+Breast.cancer.in.females
         + Diabetes.related + Firearm.related + Crowded.Housing + Unemployment)
plot(m1$residuals~Birth.Rate, col= "blueviolet", main = "Conditional Relationship of \nResponse v. Birth Rate", ylab = "Residuals", cex.lab=1.5)  # Plot of residuals v Birth Rate
# Model fitted by taking out Prenatal Care as a predictor
m2 <- lm(Teen.Birth.Rate~Birth.Rate+Breast.cancer.in.females
         + Diabetes.related + Firearm.related + Crowded.Housing + Unemployment)
plot(m2$residuals~Prenatal.Care.Beginning.in.First.Trimester, main = "Conditional Relationship of \nResponse v. Prenatal Care", ylab = "Residuals", col= "blueviolet", cex.lab=1.5) # Plot of residuals v Prenatal care
# Model fitted by taking out Breast Cancer in Females as a predictor
m3 <- lm(Teen.Birth.Rate~Birth.Rate + Prenatal.Care.Beginning.in.First.Trimester
         + Diabetes.related + Firearm.related + Crowded.Housing + Unemployment)
plot(m3$residuals~Breast.cancer.in.females, main = "Conditional Relationship of \nResponse v. Breast Cancer in Females", ylab = "Residuals", col= "blueviolet", cex.lab=1.5) # Plot of residuals v Breast cancer
# Model fitted by taking out Diabetes-related Deaths as a predictor
m4 <- lm(Teen.Birth.Rate~Birth.Rate + Prenatal.Care.Beginning.in.First.Trimester
         + Breast.cancer.in.females + Firearm.related + Crowded.Housing + Unemployment)
plot(m4$residuals~Diabetes.related, main = "Conditional Relationship of Response \nv. Diabetes-related Deaths", ylab = "Residuals", col= "blueviolet", cex.lab=1.5) # Plot of residuals v Diabetes-related deaths
# Model fitted by taking out Firearm-related Deaths as a predictor
m5 <- lm(Teen.Birth.Rate~Birth.Rate + Prenatal.Care.Beginning.in.First.Trimester
         + Breast.cancer.in.females + Diabetes.related + Crowded.Housing + Unemployment)
plot(m5$residuals~Firearm.related, main = "Conditional Relationship of \nResponse v. Firearm-related Deaths", ylab = "Residuals", col= "blueviolet", cex.lab=1.5) # Plot of residuals v Firearm-related deaths
# Model fitted by taking out Crowded Housing as a predictor
m6 <- lm(Teen.Birth.Rate~Birth.Rate + Prenatal.Care.Beginning.in.First.Trimester
         + Breast.cancer.in.females + Diabetes.related + Firearm.related + Unemployment)
plot(m6$residuals~Crowded.Housing, main = "Conditional Relationship of \nResponse v. Crowded Housing", ylab = "Residuals", col= "blueviolet", cex.lab=1.5) # Plot of residuals v Crowded Housing
# Model fitted by taking out Unemployment as a predictor
m7 <- lm(Teen.Birth.Rate~Birth.Rate + Prenatal.Care.Beginning.in.First.Trimester
         + Breast.cancer.in.females + Diabetes.related + Firearm.related + Crowded.Housing)
plot(m7$residuals~Unemployment, main = "Conditional Relationship of Response \nv. Unemployment", ylab = "Residuals", col= "blueviolet", cex.lab=1.5) # Plot of residuals v unemployment











