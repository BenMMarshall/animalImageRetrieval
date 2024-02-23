
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
