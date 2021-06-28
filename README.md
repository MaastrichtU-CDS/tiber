# TIBER

Repository for the TIBER project

## Running the algorithm

This repository contains the code to create algorithm images for vantage6. To build (and push) an image like this, `cd` to the `src` directory and execute the following command:

```bash
make docker
```

To actually run the algorith, use the `run.R` script. This requires you to have the R version of vantage6 installed. The `SyntheticPooledSet.csv` file contains data to run the algorithm on. Each node should contain data from one of the countries from the `trial` column.