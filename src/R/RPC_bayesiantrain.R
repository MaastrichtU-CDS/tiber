RPC_bayesiantrain <- function(df, nodes, arcs) {
    df <- data.frame(lapply(df , as.factor))
    df <- df[, -1]

    g <- bnlearn::empty.graph(nodes)
    bnlearn::arcs(g) <- arcs

    fitted <- bnlearn::bn.fit(g, df)

    return(list(n_obs=nrow(df), model=fitted))
}
