bayesian <- function(client) {
    vtg::log$info("Running bayesian main algorithm")
    pkg.name <- getPackageName()

    image.name <- Sys.getenv("IMAGE_NAME")

    client$set.task.image(
        image.name,
        task.name="bayesian"
    )

    if (client$use.master.container) {
        vtg::log$info("Running `bayesian` in master container using image '{image.name}'")
        result <- client$call("bayesian")
        return(result)
    }

    vtg::log$info("Running `bayesian` locally")
    vtg::log$info("Calling node bootstraps")

    responses <- client$call("bayesiannode")

    vtg::log$info("Got {length(responses)} responses")
    for (i in 1:length(responses)) {
        vtg::log$info("Returned DF {i} has {nrow(responses[[i]])} rows")
    }

    allArcs <- data.frame()
    for (r in responses) {
        allArcs <- rbind(allArcs, as.data.frame(r))
    }

    AllSitesInfo <- 
        allArcs %>% 
        dplyr::group_by(from, to) %>% 
        dplyr::summarise(weighted_strength = weighted.mean(strength, sample),
                    weighted_direction = weighted.mean(direction, sample), .groups = 'drop')

    FinalArc <- as.data.frame(subset(AllSitesInfo, weighted_strength >= 0.2 ,select = c(from, to)))

    vtg::log$info("Ended up with {nrow(FinalArc)} arcs")

    FinalStructure <- bnlearn::empty.graph(unique(c(allArcs$to, allArcs$from)))
    bnlearn::arcs(FinalStructure) <- FinalArc

    return(FinalStructure$arcs)
}


