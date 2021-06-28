RPC_bayesiannode <- function(df) {
    df <- data.frame(lapply(df , as.factor))

    vtg::log$info("Got {nrow(df)} rows in this node's data")

    bootstrapped <- bnlearn::boot.strength(df[,-1], 
                            algorithm = "hc", R = 400, 
                            algorithm.args = list(score = "bde",
                            restart = 5, perturb = 5))   

    arcs <- bootstrapped[bootstrapped$from < bootstrapped$to,]

    arcs$sample <- nrow(df)

    vtg::log$info("Got {nrow(arcs)} arcs out of this")

    return(arcs)
}