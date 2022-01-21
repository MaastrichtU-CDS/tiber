RPC_bayesiantrain <- function(df, nodes, arcs, config) {
    vtg::log$info("Starting bayesian train")
    df <- factor_dataframe(df, config)

    g <- bnlearn::empty.graph(nodes)
    bnlearn::arcs(g) <- arcs

    fitted <- bnlearn::bn.fit(g, df)

    vtg::log$info("Sending back the results")
    return(list(n_obs=nrow(df), model=fitted))
}
