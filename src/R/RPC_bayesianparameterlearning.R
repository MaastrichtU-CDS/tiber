RPC_bayesianparameterlearning <- function(df, nodes, arcs, factors_by_column, config) {
    vtg::log$info("Starting bayesian parameter learning")
    result = tryCatch({
        set_seed_config(config)
        df <- factor_dataframe(df, config, factors_by_column=factors_by_column)

        g <- bnlearn::empty.graph(nodes)
        bnlearn::arcs(g) <- arcs

        # Train the Bayesian network
        img_ss = 2
        fitted <- bnlearn::bn.fit(g, df, method="bayes", iss=img_ss)
        # Count the number of cases for each node
        node_count <- list()
        for (node in names(fitted)) {
            parents <- rev(names(dimnames(fitted[[node]]$prob))[-1])
            parents_grouped <- aggregate(df[[node]], by=df[parents], FUN=length, drop=FALSE)
            sample_length <- rep(
                parents_grouped[,ncol(parents_grouped)],
                each=dim(fitted[[node]]$prob)[[1]]
            )
            node_count[[node]] <- replace(
                sample_length, is.na(sample_length), img_ss)
        }

        vtg::log$info("Sending back the results")
        return(list(n_obs=nrow(df), model=fitted, count=node_count))
    }, error = function(e) {
        msg <- "Error while running bayesian parameter learning"
        vtg::log$info(msg)
        vtg::log$info(e)
        return(list(
            "error_message" = paste(msg, e, sep=" ")
        ))
    })
    return(result)
}
