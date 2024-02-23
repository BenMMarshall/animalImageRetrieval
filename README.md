
<!-- README.md is generated from README.Rmd. Please edit that file -->

# animalImageRetrieval

<!-- badges: start -->
<!-- badges: end -->

The goal of animalImageRetrieval is to enable quick download of reptile
images from online sources.

## Installation

You can install the development version of animalImageRetrieval from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BenMMarshall/animalImageRetrieval")
```

## Example

``` r
library(here)
library(dplyr)
library(readr)
library(spocc)
library(targets)

# loads in all the custom functions
targets::tar_source()

# reads in the reptile database file inlcuded and filters it so we aren't searching for everything
reptileData <- animalImageRetrieval::read_reptile_data(orderFilter = "Serpentes",
                                                       familyFilter = NULL,
                                                       speciesFilter = NULL)

# we create a list to store each result of the inat searches, one object per
# family so if there is a failure we will only lose less than a family of
# progress
snakeResultsList <- vector("list", length = length(unique(reptileData$Family)))
names(snakeResultsList) <- unique(reptileData$Family)
# loop through family my family getting the results for the first 50 inat observations
for(fam in unique(reptileData$Family)){
  snakeResultsList[[fam]] <- get_urls_inat(reptileData = reptileData,
                                subset = fam,
                                inatLimit = 50)
}
```

## Alternative targets approach

You can use targets::tar_visnetwork() to see the planned workflow for
mass downloading images, and targets::tar_make() to run that. Ensure all
packages listed in the \_targets.R file are present.