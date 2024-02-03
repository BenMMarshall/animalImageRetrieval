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
  packages = c("tibble", "here"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

dir.create(here::here("data", "images"), showWarnings = FALSE)
dir.create(here::here("data", "images", "flickr"), showWarnings = FALSE)
dir.create(here::here("data", "images", "inat"), showWarnings = FALSE)
dir.create(here::here("data", "images", "wiki"), showWarnings = FALSE)
dir.create(here::here("data", "images", "hmap"), showWarnings = FALSE)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

reptileData <- read_reptile_data()

values_family <- data.frame(family = unique(reptileData$Family))

# Replace the target list below with your own:
tarList_imageSearch <- list(
  tar_target(
    name = tar_reptileData,
    command = read_reptile_data()
  ),
  tar_target(
    name = tar_flickrAPI,
    command = read_flickrAPI()
  ),
  tar_map(
    values = values_family,
    tar_target(
      name = tar_flickrMetadata,
      command = get_urls_flickr(tar_reptileData, subset = family, tar_flickrAPI)
    ),
    tar_target(
      name = tar_inatMetadata,
      command = get_urls_inat(tar_reptileData, subset = family)
    ),
    tar_target(
      name = tar_wikiMetadata,
      command = get_urls_wiki(tar_reptileData, subset = family)
    ),
    tar_target(
      name = tar_hmapMetadata,
      command = get_urls_hmap(tar_reptileData, subset = family)
    ),
    tar_target(
      name = tar_flickrImages,
      command = get_imgs_flickr(tar_flickrMetadata, flickrAPI)
    ),
    tar_target(
      name = tar_inatImages,
      command = get_imgs_inat(tar_inatMetadata)
    ),
    tar_target(
      name = tar_wikiImages,
      command = get_imgs_wiki(tar_wikiMetadata),
    ),
    tar_target(
      name = tar_hmapImages,
      command = get_imgs_hmap(tar_hmapMetadata)
    )
  )
)

tarlist_metadata <- tar_combine(
  tar_metadataCompiled,
  tarList_imageSearch[[3]][grep("Metadata", names(tarList_imageSearch[[3]]))],
  # command = list(!!!.x),
  command = rbind(!!!.x)
)

tarlist_imagedata <- tar_combine(
  tar_imagedataCompiled,
  tarList_imageSearch[[3]][grep("Images", names(tarList_imageSearch[[3]]))],
  # command = list(!!!.x),
  command = rbind(!!!.x)
)

tarList_outputs <- list(
  tar_target(
    name = tar_metadataSummary,
    command = summary_metadata(tar_metadataCompiled)
  ),
  tar_target(
    name = tar_imagesSummary,
    command = summary_images(tar_imagedataCompiled)
  )
)

list(tarList_imageSearch,
     tarlist_metadata,
     tarlist_imagedata,
     tarList_outputs)