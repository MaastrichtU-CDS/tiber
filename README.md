# TIBER

Repository for the TIBER project.

## Usage

The implementation for TIBER provides a federated approach to build a bayesian network.
Using vantage6 infrastructure it's possible to run the algorithm at each center using the following interface:

```R
    client$set.task.image(
        'tiber/docker-image:x.y.z',
        task.name="bayesian"
    )

    client$use.master.container <- TRUE
    config <- list(
        # arc_strength_args = list(
        #     algorithm = "hc",
        #     R = 300
        #     algorithm.args = list(score = "bde", restart = 5, perturb = 5)
        # ),
        # weighted_strength = 0.3
    )
    column_to_predict <- 'PN'
    result <- client$call('bayesian', column_to_predict, config)
```

Calling the main method (`bayesian`) will build the bayesian network using the default configurations:
- algorithm - the structure learning algorithm used: 'hc'
- R - the number of bootstrap replicates: 400
- weighted_strength - threshold to select the network arcs based on the aggregated scoring: 0.2

## Packaging the algorithm

This repository contains the code to build the package and create the docker algorithm image for vantage6.
To build (and push) the docker image:
- `cd` to the `src` directory;
- execute the following command: `make docker`

## Testing

To actually run the algorith, use the `run.R` script. This requires you to have the R version of vantage6 installed.
The `SyntheticPooledSet.csv` file contains data to use when running the algorithm. Each node should contain data from one of the countries from the `trial` column.
