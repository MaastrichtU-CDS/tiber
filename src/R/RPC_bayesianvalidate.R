RPC_bayesianvalidate <- function(df, model, pred_col, factors_by_column, config, train_set=FALSE, external_set=FALSE) {
    vtg::log$info("Starting bayesian validate")
    result = tryCatch({
        set_seed_config(config)

        requireNamespace("bnlearn", quietly=T)
        requireNamespace("mice", quietly=T)

        df <- factor_dataframe(df, config, train_set, external_set, factors_by_column)

        validation_result <- list(n_obs=nrow(df))

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
            validation_result[["roc"]] <- roc
        }

        # Confusion matrix
        confusion_matrix <- caret::confusionMatrix(
            preds, df[, pred_col], dnn = c("Prediction", "Reference")
        )
        validation_result <- c(validation_result, "cm"=list(confusion_matrix[["table"]]))

        vtg::log$info("Sending back the results")
        return(validation_result)
    }, error = function(e) {
        vtg::log$info("Error while running bayesian validate")
        vtg::log$info(e)
        return(list(
            "error_message" = paste("Error running bayesian validate:", e, sep=" ")
        ))
    })
    return(result)
}
