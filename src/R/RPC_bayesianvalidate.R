RPC_bayesianvalidate <- function(df, model, pred_col, config) {
    vtg::log$info("Starting bayesian validate")
    df <- factor_dataframe(df, config)

    preds <- predict(model, node=pred_col, data = df, method="bayes-lw", prob=TRUE)

    vtg::log$info("Sending back the results")
    return(list(n_obs=nrow(df), preds=preds, outcomes=df[, pred_col]))
}
