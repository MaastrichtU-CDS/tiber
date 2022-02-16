# TIBER

Repository for the TIBER project.

## Usage

The implementation for TIBER provides a federated approach to build a Bayesian network.
Using vantage6 as the infrastructure makes it possible to run the algorithm at each center using the following interface:

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
        # weighted_strength = 0.3,
        # val_org_id = c(),
        # exclude = c('PT')
    )
    column_to_predict <- 'PN'
    result <- client$call('bayesian', column_to_predict, config)
```

Calling the main method (`bayesian`) will build the bayesian network using the default configurations:
- algorithm - the structure learning algorithm used: 'hc'
- R - the number of bootstrap replicates: 400
- weighted_strength - threshold to select the network arcs based on the aggregated scoring: 0.2

These configurations can be overrided using the `config` list and providing it when calling the vantage6 client.
Additionally, it's also possible to configure other parameters:
- val_org_id - explicitly provide the organization(s) id to use as validation. In case the argument isn't provided, 
one random organization from the collaboration will be chosen. In case an empty vector is provided, no validation 
will be perfomed and all organizations will be used for training.
- exclude - columns from the dataset to be ignore when creating and training the network

## Packaging the algorithm

This repository contains the code to build the package and create the docker algorithm image for vantage6.
To build (and push) the docker image:
- `cd` to the `src` directory;
- execute the following command to build the docker image: `make docker-build`
- execute the following command to build and push the docker image: `make docker`

## Testing

To run the algorithm, use the `run.R` script. This requires you to have the R version of vantage6 installed.

### Mock data

The `SyntheticPooledSet.csv` file contains data to use when running the algorithm. Each node should contain data from one of the countries from the `trial` column.
