compute_metrics <- function (df, model, pred_col, predict=FALSE) {
    # Compute the ROC AUC, PR AUC and confusion matrix
    result <- list(n_obs=nrow(df))

    preds <- predict(model, node=pred_col, data=df, method="exact", prob=TRUE)
    # preds <- predict(model, node=pred_col, data=df, method="bayes-lw", prob=TRUE)

    result[["na_pred"]] <- sum(is.na(preds))
    df <- df[!is.na(preds),]

    # ROC evaluation in case of a binary predictor
    if (length(levels(df[[pred_col]])) == 2 && all(table(df[[pred_col]]) > 0)) {
        pred <- ROCR::prediction(c(na.omit(attributes(preds)$prob[2,])), c(df[, pred_col]))
        perf <- ROCR::performance(pred, "tpr", "fpr")
        perf_p_r <- ROCR::performance(pred, "prec", "rec")
        roc <- list(
            fpr=perf@x.values[[1]],
            tpr=perf@y.values[[1]],
            prec=perf_p_r@y.values[[1]],
            thresholds=perf@alpha.values[[1]],
            negative_count=sum(df[[pred_col]] == levels(df[[pred_col]])[[1]]),
            total_count=nrow(df),
            auc=ROCR::performance(pred, measure = "auc")@y.values[[1]]
        )
        print("Local AUC")
        print(roc$auc)
        result[["roc"]] <- roc
    }

    # Confusion matrix
    confusion_matrix <- caret::confusionMatrix(
        na.omit(preds), df[, pred_col], dnn = c("Prediction", "Reference")
    )
    result <- c(result, "cm"=list(confusion_matrix[["table"]]))

    vtg::log$info("Sending back the results")
    return(result)
}

validate_data <- function(df, model, pred_col, factors_by_column, config, train_set=FALSE, validating=FALSE, external_set=FALSE, imputation_model=NULL) {
    bn_impute <- ("bn_impute" %in% names(config) && config[["bn_impute"]])
    keep_nan <- ("keep_nan" %in% names(config) && config[["keep_nan"]])
    out <- factor_dataframe(df, config, train_set, external_set, factors_by_column, validating, bn_impute, imputation_model)
    df <- out$data
    performance_metrics <- list()
    if (nrow(df) > 0) {
        if (bn_impute && !keep_nan) {
            vtg::log$info("Impute using bnlearn")
            bn_imputation_model <- model
            if ("imputation_model" %in% names(config)) {
                vtg::log$info("Impute model provided")
                bn_imputation_model <- config[["imputation_model"]]
            }
            debug <- FALSE
            if ("debug" %in% names(config)) {
                debug <- config[["debug"]]
            }
            df <- bnlearn::impute(bn_imputation_model, data = df, method="exact", debug = debug)
            # df <- bnlearn::impute(bn_imputation_model, data = df, method="bayes-lw", debug = debug)

            print(paste("Number of rows with missing data:", sum(is.na(df))))
            print(paste("Number of rows with missing outcome:", sum(is.na(df[[pred_col]]))))
        }
        performance_metrics = compute_metrics(df, model, pred_col)
    }
    vtg::log$info("Got {nrow(df)} rows in this node's data")
    return(list("metrics"=performance_metrics, "imputation_model"=out$imputation_model))
}

RPC_bayesianvalidate <- function(df, model, pred_col, factors_by_column, config, external_set=FALSE) {
    vtg::log$info("Starting bayesian validate")
    result = tryCatch({
        requireNamespace("bnlearn", quietly=T)
        performance_result <- list()
        if (external_set) {
            vtg::log$info("Testing set")
            config[["impute"]] <- FALSE
            performance_result[["test_set"]] <- (validate_data(
                df, model, pred_col, factors_by_column, config, external_set=TRUE, validating=TRUE
            ))$metrics
        } else {
            # Training set
            if ("impute" %in% names(config) && config[["impute"]]) {
                vtg::log$info("Imputed data")
                set_seed_config(config)
                out <- validate_data(
                    df, model, pred_col, factors_by_column, config, train_set=TRUE
                )
                performance_result[["train_set_imputed"]] <- out$metrics
                imputation_model <- out$imputation_model
                set_seed_config(config)
                performance_result[["validation_set_imputed"]] <- (validate_data(
                    df, model, pred_col, factors_by_column, config, train_set=FALSE, imputation_model=imputation_model
                ))$metrics
            }
            vtg::log$info("Original dataset")
            config[["impute"]] <- FALSE
            # The validating flag will only keep the rows with less than 'nan_limit'
            # number of missing values (by default 1)
            set_seed_config(config)
            out <- validate_data(
                df, model, pred_col, factors_by_column, config, train_set=TRUE, validating=TRUE
            )
            performance_result[["train_set"]]  <- out$metrics
            imputation_model <- out$imputation_model
            set_seed_config(config)
            performance_result[["validation_set"]] <- validate_data(
                df, model, pred_col, factors_by_column, config, train_set=FALSE, validating=TRUE, imputation_model=imputation_model
            )$metrics
        }
        return(performance_result)
    }, error = function(e) {
        msg <- "Error while running bayesian validate"
        vtg::log$info(msg)
        vtg::log$info(e)
        return(list(
            "error_message" = paste(msg, e, sep=" ")
        ))
    })
    return(result)
}
