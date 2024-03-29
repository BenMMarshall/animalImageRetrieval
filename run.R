#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

targets::tar_make()
# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint
targets::tar_make(names = contains("inat"))
targets::tar_make("tar_hmapUrldata_Amphisbaenia")
targets::tar_make("tar_wikiUrldata_Xenosauridae")
targets::tar_make("tar_flickrUrldata_Xenosauridae")
