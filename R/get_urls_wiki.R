#' Search for image locations on wikimedia
#'
#' @name get_urls_wiki
#' @description A
#' @param abc abc
#' @return a
#'
#' @export
get_urls_wiki <- function(reptileData, subset){
  # subset <- "Anolidae"
  subsertReptileData <- reptileData[reptileData$Family == subset,]

  species <- subsertReptileData$Species

  # full.data.list <- vector("list", length = length(species))
  # names(full.data.list) <- species
  # meta.list <- vector("list", length = length(species))
  # names(meta.list) <- species
  # i <- 0

  dir.create(here("data", "wiki"), showWarnings = FALSE)
  dir.create(here("data", "wiki", "images"), showWarnings = FALSE)

  images.list <- vector("list", length(species))
  names(images.list) <- species
  for(sp in species){
    # sp <- species[i]
    print(sp)

    if(paste0(subset, "_wiki_images_list.rds") %in% list.files(here("data", "wiki"))){
      images.list <- readRDS(here("data", "wiki", paste0(subset, "_wiki_images_list.rds")))
    }
    if(!is.null(images.list[[sp]])){
      print("Already retrieved")
      {next}
    }

    images.list[[sp]] <- search_wiki(sp)
    saveRDS(images.list, file = here("data", "wiki", paste0(subset, "_wiki_images_list.rds")))
  }

  images.list <- readRDS(here("data", "wiki", paste0(subset, "_wiki_images_list.rds")))

  wikiData <- do.call(rbind, images.list)
  wikiData <- as.data.frame(apply(wikiData, 2, as.character))

  return(wikiData)

}
