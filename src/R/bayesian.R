bayesian <- function(client, pred_col, config=list()) {
    vtg::log$info("Running bayesian main algorithm")
    pkg.name <- getPackageName()

    image.name <- Sys.getenv("IMAGE_NAME")

    client$set.task.image(
        image.name,
        task.name="bayesian"
    )

    # Send off a new container if necessary
    if (client$use.master.container) {
        vtg::log$info("Running `bayesian` in master container using image '{image.name}'")
        result <- client$call("bayesian")
        return(result)
    }

    vtg::log$info("Running `bayesian` locally")
    vtg::log$info("Calling node bootstraps")

    # Get the structure of the network
    responses <- client$call("bayesiannode", config[["arc_strength_args"]])

    vtg::log$info("Got {length(responses)} responses")
    if (length(responses) == 0) {
        return(c("No responses obtained from the node, please check the logs"))
    }

    for (i in 1:length(responses)) {
        vtg::log$info("Returned DF {i} has {nrow(responses[[i]])} rows")
    }

    # Combine the structures
    allArcs <- data.frame()
    for (r in responses) {
        allArcs <- rbind(allArcs, as.data.frame(r))
    }

    AllSitesInfo <-
        allArcs %>%
        dplyr::group_by(from, to) %>%
        dplyr::summarise(weighted_strength = weighted.mean(strength, sample),
                    weighted_direction = weighted.mean(direction, sample), .groups = 'drop')

    FinalArc <- as.data.frame(subset(
        AllSitesInfo,
        weighted_strength >= (
            config[["weighted_strength"]] %>% if (is.null(.)) 0.2 else .),
        select = c(from, to)
    ))

    vtg::log$info("Ended up with {nrow(FinalArc)} arcs")

    FinalStructure <- bnlearn::empty.graph(unique(c(allArcs$to, allArcs$from)))
    bnlearn::arcs(FinalStructure) <- FinalArc

    results <- list(structure=FinalStructure$arcs)

    # Train the network
    vtg::log$info("Training the network")
    responses <- client$call('bayesiantrain', bnlearn::nodes(FinalStructure), as.data.frame(bnlearn::arcs(FinalStructure)), pred_col)

    # We only keep the fit on the largest sample
    biggest_i <- which.max(lapply(responses, function(x){return(x[['n_obs']])}))
    model <- responses[[biggest_i]][['model']]

    results <- c(results, list(train_preds=responses[[biggest_i]][['preds']], train_outcomes=responses[[biggest_i]][['outcomes']]))

    # Now validate it against the other nodes
    vtg::log$info("Validating the network")
    responses <- client$call('bayesiantest', model, pred_col)
    best_i <- which.max(lapply(responses, function(x){return(x[['n_obs']])}))

    results <- c(results, test_results=responses[-best_i])

}


