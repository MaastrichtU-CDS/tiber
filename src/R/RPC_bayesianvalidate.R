RPC_bayesianvalidate <- function(df, model, pred_col, config) {
    vtg::log$info("Starting bayesian validate")
    requireNamespace("bnlearn", quietly=T)

    df <- factor_dataframe(df, config)
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
    if (!("predictions" %in% names(config)) || config[["predictions"]]) {
        result <- c(result, "predictions"=list(preds), "outcomes"=list(df[, pred_col]))
    }

    vtg::log$info("Sending back the results")
    return(result)
}
