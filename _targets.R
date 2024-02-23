# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(here)

# Set target options:
tar_option_set(
  packages = c("tibble",
               "here",
               "spocc",
               "dplyr",
               "readr",
               "downloader",
               "xml2",
               "rvest",
               "dplyr",
               "stringr",
               "jsonlite",
               "stringr",
               "urltools"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

dir.create(here::here("data"), showWarnings = FALSE)
dir.create(here::here("data", "flickr"), showWarnings = FALSE)
dir.create(here::here("data", "inat"), showWarnings = FALSE)
dir.create(here::here("data", "wiki"), showWarnings = FALSE)
dir.create(here::here("data", "hmap"), showWarnings = FALSE)
dir.create(here::here("data", "flickr", "images"), showWarnings = FALSE)
dir.create(here::here("data", "inat", "images"), showWarnings = FALSE)
dir.create(here::here("data", "wiki", "images"), showWarnings = FALSE)
dir.create(here::here("data", "hmap", "images"), showWarnings = FALSE)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# can filter the reptileDB data by order family or species, if filtering here
# then also filter in the targets pipeline required
reptileData <- animalImageRetrieval::read_reptile_data(orderFilter = "Serpentes",
                                                       familyFilter = NULL,
                                                       speciesFilter = NULL)

values_family <- data.frame(family = unique(reptileData$Family))

# values_orders <-
#   data.frame(order = c("Amphisbaenia",
#     "Crocodylia",
#     "Lacertilia",
#     "Serpentes",
#     "Sphenodon",
#     "Testudines"))

# order is the subdivisions required for hmap
values_orders <-
  data.frame(order = c(
    "Serpentes"))

# TARGETS PIPELINE - use targets::tar_visnetwork() is view
tarList_imageSearch <- list(
  tar_target(
    name = tar_reptileData,
    command = read_reptile_data(orderFilter = "Serpentes",
                                familyFilter = NULL,
                                speciesFilter = NULL)
  ),
  tar_target(
    name = tar_flickrAPI,
    command = read_flickrAPI()
  ),
  tar_map(
    values = values_family,
    tar_target(
      name = tar_flickrUrldata,
      command = get_urls_flickr(reptileData = tar_reptileData,
                                subset = family,
                                flickrAPI = tar_flickrAPI)
    ),
    tar_target(
      name = tar_inatUrldata,
      command = get_urls_inat(reptileData = tar_reptileData,
                              subset = family,
                              inatLimit = 50)
    ),
    tar_target(
      name = tar_wikiUrldata,
      command = get_urls_wiki(reptileData = tar_reptileData,
                              subset = family)
    ),
    tar_target(
      name = tar_flickrImages,
      command = get_imgs(urlData = tar_flickrUrldata,
                         source = "flickr")
    ),
    tar_target(
      name = tar_inatImages,
      command = get_imgs(urlData = tar_inatUrldata,
                         source = "inat")
    ),
    tar_target(
      name = tar_wikiImages,
      command = get_imgs(urlData = tar_wikiUrldata,
                         source = "wiki")
    )
  ),
  tar_map(
    values = values_orders,
    tar_target(
      name = tar_hmapUrldata,
      command = get_urls_hmap(reptileData = tar_reptileData,
                              subset = order)
    ),
    tar_target(
      name = tar_hmapImages,
      command = get_imgs_hmap(tar_hmapUrldata,
                              source = "hmap")
    )
  )
)

tarlist_metadata <- tar_combine(
  name = tar_metadataCompiled,
  list(tarList_imageSearch[[3]][grep("Urldata", names(tarList_imageSearch[[3]]))],
       tarList_imageSearch[[4]][grep("Urldata", names(tarList_imageSearch[[4]]))]),
  command = list(!!!.x)
)

# tarlist_imagedata <- tar_combine(
#   tar_imagedataCompiled,
#   tarList_imageSearch[[3]][grep("Images", names(tarList_imageSearch[[3]]))],
#   # command = list(!!!.x),
#   command = rbind(!!!.x)
# )

tarList_outputs <- list(
  tar_target(
    name = tar_metadataSummary,
    command = summary_metadata(tar_metadataCompiled)
  )
  # tar_target(
  #   name = tar_imagesSummary,
  #   command = summary_images(tar_imagedataCompiled)
  # )
)

list(tarList_imageSearch,
     tarlist_metadata,
     # tarlist_imagedata,
     tarList_outputs)
