df <- read.csv('/path/to/db')

nrow(df)
dimnames(df)

# Logistic Regression

# Exclude the M-stage
# exclude <- list("M")
# df <- df[,!(names(df) %in% exclude)]

#df_na <- na.omit(df)

# Remove the only row with an N3 stage
df <- df[df["N"] != "N3",]

# Predicting
pred_col <- "Outcome"

# Fraction of rows used for training that do not have the outcome
fraction_without_pred <- 0.1
# Data split (fraction for training)
split <- 0.7

# Build the data frames
df_pred <- df[!is.na(df[pred_col]),]
df_no_pred <- df[is.na(df[pred_col]),]
size <- nrow(df_pred)
split_pred <- sort(sample(size, size * split))
df_train_c <- rbind(df_pred[split_pred,], df_no_pred[sample(nrow(df_no_pred), round(nrow(df_no_pred) * fraction_without_pred)), ])
# df_train_c - data with outcome + without outcome (if fraction_without_pred > 0)
# df_train_c_pred - only data with outcome
# df_train - imputation on data with outcome + without outcome (if fraction_without_pred > 0)
# df_valid_c - only data with outcome
# df_valid - imputation on data with outcome
df_train_c <- data.frame(lapply(df_train_c , as.factor))
df_train_c_pred <- data.frame(lapply(df_pred[split_pred,] , as.factor))
df_valid_c <- data.frame(lapply(df_pred[-split_pred,] , as.factor))
df_valid <- data.frame(lapply(df_pred[-split_pred,] , as.factor))
df_train <- mice::complete(mice::mice(df_train_c, m=5), "stacked")
df_valid <- mice::complete(mice::mice(df_valid, m=5), "stacked")
for (row_name in names(df_train)) {
  levels(df_valid[[row_name]]) <- levels(df_train[[row_name]])
  levels(df_valid_c[[row_name]]) <- levels(df_train[[row_name]])
  levels(df_train_c_pred[[row_name]]) <- levels(df_train[[row_name]])
}

#df_f <- data.frame(lapply(df , as.factor))
#df_f <- mice::complete(mice::mice(df_f, m=3), "stacked")

#split <- 0.7
#size <- nrow(df_f)
#split_pred <- sort(sample(size, size * split))
#df_train <- df_f[split_pred,]
#df_valid <- df_f[-split_pred,]

model <- glm(Outcome ~., family=binomial(link='logit'), data=df_train)

summary(model)

#result <- predict(model, newdata = df_train, type="response")
#b_result <- ifelse(result > 0.5, 1, 0)
#mis_error <- mean(b_result != 0)

### Results

# Training
result <- predict(model, newdata = df_train, type="response")
b_result <- ifelse(result > 0.5, 1, 0)
mis_error <- mean(b_result != df_train$Outcome)
roc_object <- roc(df_train$Outcome, result)
auc(roc_object)

# No imputation performed - only rows with Outcome
result <- predict(model, newdata = df_train_c_pred, type="response")
b_result <- ifelse(result > 0.5, 1, 0)
mis_error <- mean(b_result != df_train_c_pred$Outcome)
roc_object <- roc(df_train_c_pred$Outcome, result)
auc(roc_object)

# Validation
result <- predict(model, newdata = df_valid, type="response")
b_result <- ifelse(result > 0.5, 1, 0)
mis_error <- mean(b_result != df_valid$Outcome)
roc_object <- roc(df_valid$Outcome, result)
auc(roc_object)

# No imputation performed
result <- predict(model, newdata = df_valid_c, type="response")
b_result <- ifelse(result > 0.5, 1, 0)
mis_error <- mean(b_result != df_valid$Outcome)
roc_object <- roc(df_valid$Outcome, result)
auc(roc_object)
