RPC_bayesiantrain <- function(df, nodes, arcs, pred_col) {
    df <- data.frame(lapply(df , as.factor))
    df <- df[, -1]

    g <- bnlearn::empty.graph(nodes)
    bnlearn::arcs(g) <- arcs

    fitted <- bnlearn::bn.fit(g, df)

    preds <- predict(fitted, node=pred_col, data = df, method="bayes-lw", prob=TRUE)

    # Try returning full bn
    return(list(n_obs=nrow(df), model=fitted, preds=preds, outcomes=df[, pred_col]))
}