factor_dataframe <- function(df, config) {
    df <- data.frame(lapply(df , as.factor))
    if ("exclude" %in% names(config)) {
        df <- df[,!(names(df) %in% config[["exclude"]])]
    }
    return(df)
}

parse_roc <- function(partial_rocs) {
    # Preparing the input
    fpr <- sapply(partial_rocs, `[`, "fpr")
    tpr <- sapply(partial_rocs, `[`, "tpr")
    thresholds <- sapply(partial_rocs, `[`, "thresholds")
    negative_count <- unname(unlist(sapply(partial_rocs, `[`, "negative_count")))
    total_count <- unname(unlist(sapply(partial_rocs, `[`, "total_count")))
    partial_auc <- unname(unlist(sapply(partial_rocs, `[`, "auc")))

    # Compute the global ROC curve for the model
    roc_aggregated <- ROCaggregator::roc_curve(fpr, tpr, thresholds, negative_count, total_count)

    # Calculate the AUC
    roc_auc <- pracma::trapz(roc_aggregated$fpr, roc_aggregated$tpr)

    sprintf("ROC AUC aggregated from each node's results: %f", roc_auc)

    # Calculate the precision-recall
    precision_recall_aggregated <- ROCaggregator::precision_recall_curve(
        fpr, tpr, thresholds, negative_count, total_count)

    # Calculate the precision-recall AUC
    precision_recall_auc <- -(pracma::trapz(
        precision_recall_aggregated$recall, precision_recall_aggregated$pre))

    return(list(
        roc=list(roc_aggregated, auc=roc_auc, partial_auc=partial_auc),
        pr=list(precision_recall_aggregated, auc=precision_recall_auc)
    ))
}

evaluation <- function(responses) {
    eval <- list()
    if ("roc" %in% names(responses[[1]])) {
        eval <- c(eval, "metrics"=parse_roc(sapply(responses, `[`, "roc")))
    }
    if ("preds" %in% names(responses[[1]])) {
        pred <- unname(unlist(sapply(partial_rocs, `[`, "preds")))
        outcome <- unname(unlist(sapply(partial_rocs, `[`, "outcomes")))
        eval <- c(eval, "predictions"=pred, "outcomes"=outcome)
    }
    return(eval)
}
