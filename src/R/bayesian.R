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
        vtg::log$info(
            "Running `bayesian` in master container using image '{image.name}'")
        result <- client$call("bayesian")
        return(result)
    }
    vtg::log$info("Running `bayesian` locally")

    # Define the nodes used for training and validation
    # By default, it will select a random node for validation
    collaboration_org_ids = client$collaboration$organizations
    validate_results <- TRUE
    if (!("val_org_id" %in% names(config))) {
        org_id <- collaboration_org_ids[[sample(1:length(collaboration_org_ids), 1)]]$id
        vtg::log$info("Organization '{org_id}' selected for validation")
        config[["val_org_id"]] <- org_id
    } else if (length(config[["val_org_id"]]) == 0) {
        vtg::log$info("Validation won't be performed")
        validate_results <- FALSE
    }

    # Update the client organizations according to the ones that will be used
    # for training
    validation_orgs <- list()
    if (validate_results) {
        client$collaboration$organizations <- list()
        for (collaboration in collaboration_org_ids) {
            if (collaboration$id %in% config[["val_org_id"]]) {
                validation_orgs <- append(validation_orgs, list(collaboration))
            } else {
                client$collaboration$organizations <- append(
                    client$collaboration$organizations, list(collaboration))
            }
        }
    }

    # Get the structure of the network
    vtg::log$info("Getting the arc strength from each node")
    responses <- client$call("bayesiannode", config)

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

    # Training the network
    vtg::log$info("Training the network")
    responses <- client$call(
        "bayesiantrain",
        bnlearn::nodes(FinalStructure),
        as.data.frame(bnlearn::arcs(FinalStructure)),
        config
    )

    # Weighted average to determine the parameters for the conditional probability tables
    vtg::log$info("Aggregate the conditional probability tables")
    total_samples <- Reduce('+', sapply(responses, "[", "n_obs"))
    prob_dist <- list()
    model_structure <- responses[[1]][["model"]]
    for (node in names(model_structure)) {
        weighted_prob <- lapply(
            1:length(responses),
            function(i) c(responses[[i]]$model[[node]][["prob"]]) * responses[[i]][["n_obs"]] / total_samples
        )
        prob_dist[[node]] <- c(Reduce('+', weighted_prob))
        dim(prob_dist[[node]]) <- attributes(model_structure[[node]]$prob)$dim
        dimnames(prob_dist[[node]]) <- attributes(model_structure[[node]]$prob)$dimnames
    }
    aggregated_model <- bnlearn::custom.fit(FinalStructure, dist=prob_dist)

    # Validate the training set
    vtg::log$info("Validating the network - training")
    responses <- client$call("bayesianvalidate", aggregated_model, pred_col, config)
    results <- list("structure"=FinalStructure$arcs, "model"=aggregated_model, "training_results"=evaluation(responses))

    # Validate the testing set
    if (validate_results) {
        vtg::log$info("Validating the network - validation")
        client$collaboration$organizations <- validation_orgs
        responses <- client$call("bayesianvalidate", aggregated_model, pred_col, config)

        results <- c(results, "test_results"=evaluation(responses), "val_org"=config[["val_org_id"]])
    }

    return(results)
}


