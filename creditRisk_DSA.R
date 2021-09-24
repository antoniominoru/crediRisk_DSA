# Logistic Regression 
# Prevision and detection of credit risk


# Packages
library(caret)
library(e1071) 

# Dataframe
credito_dataset <- read.csv("credit_dataset_final.csv", header = TRUE, sep = ",")

# Functions
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center = T, scale = T)
  }
  return(df)
}

# Pré-processing  
numeric.vars <- c("credit.duration.months", "age", "credit.amount")
credito_dataset_scaled <- scale.features(credito_dataset, numeric.vars)
categorical.vars <- c('credit.rating', 'account.balance', 'previous.credit.payment.status',
                      'credit.purpose', 'savings', 'employment.duration', 'installment.rate',
                      'marital.status', 'guarantor', 'residence.duration', 'current.assets',
                      'other.credits', 'apartment.type', 'bank.credits', 'occupation', 
                      'dependents', 'telephone', 'foreign.worker')
credito_dataset_final <- to.factors(df = credito_dataset_scaled, variables = categorical.vars)

# Split train and test
indexes <- sample(1:nrow(credito_dataset_final), size = 0.7 * nrow(credito_dataset_final))
train.data <- credito_dataset_final[indexes,]
test.data <- credito_dataset_final[-indexes,]

# separate target
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]

# building logistic regression model
formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
model_v1 <- glm(formula = formula.init, data = train.data, family = "binomial")
summary(model_v1)

# Prevision
previsoes <- predict(model_v1, test.data, type = "response")
previsoes <- round(previsoes)
confusionMatrix(table(data = previsoes, reference = test.class.var), positive = '1')


# create model 2

formula.new <- "credit.rating ~ account.balance + credit.purpose + previous.credit.payment.status + savings + credit.duration.months"
formula.new <- as.formula(formula.new)
model_v2 <- glm(formula = formula.new, data = train.data, family = "binomial")
summary(model_v2)

# Prevision 
previsoes_new <- predict(model_v2, test.data, type = "response") 
previsoes_new <- round(previsoes_new)
confusionMatrix(table(data = previsoes_new, reference = test.class.var), positive = '1')

