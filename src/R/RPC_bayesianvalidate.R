compute_metrics <- function (df, model, pred_col) {
    # Compute the ROC AUC, PR AUC and confusion matrix
    result <- list(n_obs=nrow(df))

    preds <- predict(model, node=pred_col, data=df, method="bayes-lw", prob=TRUE)

    # ROC evaluation in case of a binary predictor
    if (length(levels(df[[pred_col]])) == 2) {
        pred <- ROCR::prediction(c(attributes(preds)$prob[2,]), c(df[, pred_col]))
        perf <- ROCR::performance(pred, "tpr", "fpr")
        perf_p_r <- ROCR::performance(pred, "prec", "rec")
        roc <- list(
            fpr=perf@x.values[[1]],
            tpr=perf@y.values[[1]],
            prec=perf_p_r@y.values[[1]],
            thresholds=perf@alpha.values[[1]],
            negative_count=sum(df[[pred_col]] == levels(df[[pred_col]])[[1]]),
            total_count=nrow(df),
            auc=ROCR::performance(pred, measure = "auc")@y.values[[1]]
        )
        result[["roc"]] <- roc
    }

    # Confusion matrix
    confusion_matrix <- caret::confusionMatrix(
        preds, df[, pred_col], dnn = c("Prediction", "Reference")
    )
    result <- c(result, "cm"=list(confusion_matrix[["table"]]))

    vtg::log$info("Sending back the results")
    return(result)
}

validate_data <- function(df, model, pred_col, factors_by_column, config, train_set=FALSE, validating=FALSE, external_set=FALSE) {
    df <- factor_dataframe(df, config, train_set, external_set, factors_by_column, validating)
    vtg::log$info("Got {nrow(df)} rows in this node's data")
    return(compute_metrics(df, model, pred_col))
}

RPC_bayesianvalidate <- function(df, model, pred_col, factors_by_column, config, external_set=FALSE) {
    vtg::log$info("Starting bayesian validate")
    result = tryCatch({
        set_seed_config(config)
        requireNamespace("bnlearn", quietly=T)
        performance_result <- list()
        if (external_set) {
            vtg::log$info("Testing set")
            performance_result[["test_set"]] <- validate_data(
                df, model, pred_col, factors_by_column, config, external_set=TRUE
            )

        } else {
            # Training set
            if ("impute" %in% names(config) && config[["impute"]]) {
                vtg::log$info("Imputed data")
                performance_result[["train_set_imputed"]] <- validate_data(
                    df, model, pred_col, factors_by_column, config, train=TRUE
                )
                performance_result[["validation_set_imputed"]] <- validate_data(
                    df, model, pred_col, factors_by_column, config, train=FALSE
                )
            }
            vtg::log$info("Original dataset")
            # The validating flag will only keep the rows with less than 'nan_limit'
            # number of missing values (by default 1)
            performance_result[["train_set"]] <- validate_data(
                df, model, pred_col, factors_by_column, config, train=TRUE, validating=TRUE
            )
            performance_result[["validation_set"]] <- validate_data(
                df, model, pred_col, factors_by_column, config, train=FALSE, validating=TRUE
            )
        }
        return(performance_result)
    }, error = function(e) {
        msg <- "Error while running bayesian validate"
        vtg::log$info(msg)
        vtg::log$info(e)
        return(list(
            "error_message" = paste(msg, e, sep=" ")
        ))
    })
    return(result)
}
