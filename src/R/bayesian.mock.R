bayesian.mock <- function(df, countries) {
    datasets <- list()

    for (k in 1:length(countries)) {
        datasets[[k]] <- df %>% filter(trial == countries[k])
    }

    vtg::log$info("Running mock on {length(datasets)} datasets")

    client <- vtg::MockClient$new(datasets, pkgname=getPackageName())
    results <- bayesian(client)
    return(results)
}