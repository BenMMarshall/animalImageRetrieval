#' Search for image locations on flickr
#'
#' @name get_urls_flickr
#' @description A
#' @param abc abc
#' @return a
#'
#' @export
get_urls_flickr <- function(reptileData, subset, flickrAPI){

  subsertReptileData <- reptileData[reptileData$Family == subset,]
  species <- subsertReptileData$Species

  # generate the string of tags to limit the images to reptiles
  tags.vec <- c("snake", "reptile", "tortoise", "turtle", "lizard", "crocodile", "alligator",
                "crocodilian", "amphisbaenian", "tuatara", "gharial", "caiman")
  # tags <- "snake"
  tags.coll <- paste0(tags.vec, collapse = ",")

  ## Looping function to produce a full list of results
  i <- 0
  meta.results <- vector(mode = "list", length = length(species))
  names(meta.results) <- species
  flickr.list <- vector(mode = "list", length = length(species))
  names(flickr.list) <- species

  # 3600 request per hour, each run of the function requires one to detect number
  # of images, then subsequent to get each page. The limit is about 1 every second.
  for(sp in species){
    i <- i + 1
    # sp <- species[1]
    ### SOMETHING WRONG WITH SPECIAL CHARACTERS HERE FOR THIS SPECIES
    # if(sp %in% c("Acanthodactylus aegyptius")){
    #   {next}
    # }

    # check if there is already progress in the results and load if there is
    if(i == 1 & (any(list.files(here("data", "flickr")) %in% paste0(subset, "_flickr_results.Rds")))){
      meta.results <- readRDS(file = here("data", "flickr", paste0(subset, "_flickr_meta.Rds")))
      flickr.list <- readRDS(file = here("data", "flickr", paste0(subset, "_flickr_results.Rds")))
    }
    # check if the species has already been searched
    if( !is.null(flickr.list[[sp]]) ){
      print(paste0(sp, " - skipped"))
      {next}
    }

    # get the results for the species
    flickr.df <- search_flickr(search = sp,
                               api_key = flickrAPI,
                               tags = tags.coll,
                               tag_m = "any",
                               content_type = "1", # photos only
                               extra_call = "date_taken,geo,license",
                               page_call = "250")

    # possible beginnings of converting the urls to a downloadable link, but doesn't appear to work with download()
    # # https://www.flickr.com/photos/naturestills/16073589879/sizes/o/
    # images <- read_html("https://www.flickr.com/photos/naturestills/16073589879/sizes/o/",
    #                     encoding = "UTF-8") %>%
    #   html_elements("img") %>%
    #   html_attr("src")
    #
    # # https://live.staticflickr.com/8601/16073589879_f691c4242c_o.jpg
    # images[str_detect(images, "live.staticflickr")]

    meta.results[[sp]] <- flickr.df[[1]]
    saveRDS(meta.results, file = here("data", "flickr", paste0(subset, "_flickr_meta.Rds")), compress = FALSE)
    flickr.list[[sp]] <- flickr.df[[2]]
    saveRDS(flickr.list, file = here("data", "flickr", paste0(subset, "_flickr_results.Rds")), compress = FALSE)
    print(paste0("--- ", sp, ": End and Saved ---"))

  }
  # saveRDS(flickr.list, file = "./Data/flickr_results.Rds", compress = FALSE)

  meta.fullresults <- do.call(rbind, meta.results)

  # write.csv(x = meta.fullresults, file = here("data", "flickr", paste0(subset, "_flickr_meta.Rds")),
  #           row.names = FALSE)

  flickr.fullresults <- do.call(rbind, flickr.list)

  # write.csv(x = flickr.fullresults, file = here("data", "flickr", paste0(subset, "_flickr_results.Rds")),
  #           row.names = FALSE)

  return(flickr.fullresults)

}
