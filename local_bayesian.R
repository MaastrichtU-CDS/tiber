df <- read.csv('/path/to/db')

nrow(df)
dimnames(df)

# Bayesian Network

# Exclude the M-stage
# exclude <- list("M")
# df <- df[,!(names(df) %in% exclude)]

# Remove all rows with missing values
# df_na <- na.omit(df)

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
#df_f <- mice::complete(mice::mice(df_f, m=5), "stacked")

#split <- 0.7
#size <- nrow(df_f)
#split_pred <- sort(sample(size, size * split))
#df_train <- df_f[split_pred,]
#df_valid <- df_f[-split_pred,]

weighted_strength <- 0.25
config <- list()
parameters <- list(
  df_train,
  algorithm = "hc",
  R = 100,
  algorithm.args = list(score = "bde", restart = 5, perturb = 5)
)

# arc_config <- config[["arc_strength_args"]]
arc_config <- list(
  algorithm = "hc",
  R = 100,
  algorithm.args = list(
    score = "bde",
    restart = 5,
    perturb = 5,
    blacklist = matrix(
      c(
        "Outcome", "age_discretised",
        "Outcome", "distance_discretised",
        "Outcome", "Gender",
        "Outcome", "M",
        "Outcome", "T",
        "Outcome", "N",
        "Outcome", "MRF_involved",
        "Outcome", "treatment_type",
        "Outcome", "gtv_discretised_local",
        "Outcome", "WHO"
      ),
      ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to"))
    )
  ),
  debug = TRUE
)
if (!is.null(arc_config)){
  for (arg in names(arc_config)) {
    if (arg != "") {
      parameters[arg] = arc_config[arg]
    }
  }
}

arcs <- do.call(bnlearn::boot.strength, parameters)
arcs$sample <- nrow(df_train)

# Parameters
nodes <- unique(c(arcs[["from"]], arcs[["to"]]))
g <- bnlearn::empty.graph(nodes)

# Select arcs
allArcs <- data.frame()
allArcs <- rbind(allArcs, as.data.frame(arcs))

AllSitesInfo <-
  allArcs %>%
  dplyr::group_by(from, to) %>%
  dplyr::summarise(strength = weighted.mean(strength, sample),
                   direction = weighted.mean(direction, sample), .groups = 'drop')

class(AllSitesInfo) <- c("bn.strength", "data.frame")
attributes(AllSitesInfo)[["method"]] <- "bootstrap"
attributes(AllSitesInfo)[["threshold"]] <- weighted_strength
attributes(AllSitesInfo)[["nodes"]] <- nodes

averaged_network <- bnlearn::pdag2dag(
  bnlearn::averaged.network(AllSitesInfo),
  ordering=config[["ordering"]] %>% if (is.null(.)) nodes else .
)

bnlearn::arcs(g) <- averaged_network$arcs

# Train the Bayesian network
img_ss = 2
fitted <- bnlearn::bn.fit(g, df_train, method="bayes", iss=img_ss)

### Metrics

# Training
result <- predict(fitted, node="Outcome", data = df_train, method="bayes-lw", prob=TRUE)
roc_object <- roc(df_train$Outcome, c(attributes(result)$prob[2,]))
auc(roc_object)

df_train_imputed <- bnlearn::impute(fitted, data = df_train_c, method="parents")
result <- predict(fitted, node="Outcome", data = df_train_imputed, method="bayes-lw", prob=TRUE)
roc_object <- roc(df_train_imputed$Outcome, c(attributes(result)$prob[2,]))
auc(roc_object)

df_train_imputed <- bnlearn::impute(fitted, data = df_train_c_pred, method="parents")
result <- predict(fitted, node="Outcome", data = df_train_imputed, method="bayes-lw", prob=TRUE)
roc_object <- roc(df_train_imputed$Outcome, c(attributes(result)$prob[2,]))
auc(roc_object)

# Validation
result <- predict(fitted, node="Outcome", data = df_valid, method="bayes-lw", prob=TRUE)
roc_object <- roc(c(df_valid$Outcome), c(attributes(result)$prob[2,]))
auc(roc_object)

df_valid_imputed <- bnlearn::impute(fitted, data = df_valid_c, method="parents")
result <- predict(fitted, node="Outcome", data = df_valid_imputed, method="bayes-lw", prob=TRUE)
roc_object <- roc(c(df_valid_imputed$Outcome), c(attributes(result)$prob[2,]))
auc(roc_object)
