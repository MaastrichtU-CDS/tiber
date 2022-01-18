RPC_bayesianvalidate <- function(df, model, pred_col) {
    requireNamespace("bnlearn", quietly=T)

    df <- data.frame(lapply(df , as.factor))
    df <- df[, -1]

    preds <- predict(model, node=pred_col, data = df, method="bayes-lw", prob=TRUE)

    return(list(n_obs=nrow(df), preds=preds, outcomes=df[, pred_col]))
}
