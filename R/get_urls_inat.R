#' Search for image locations on iNat
#'
#' @name get_urls_inat
#' @description Will search inaturalist using the spocc package and incrementally save the results.
#' @param reptileData Reptile database data from the read_reptile_data() function
#' @param subset Required. A family subset to work with, can be a single family.
#' @param inatLimit Limit of the number of inat observations to collect, hard limit of 10000. Default of 10000.
#' @return A dataframe of photos
#'
#' @export
get_urls_inat <- function(reptileData, subset, inatLimit = 10000){
  # reptileData <- animalImageRetrieval::read_reptile_data()
  # subset <- "Blanidae"
  subsertReptileData <- reptileData[reptileData$Family == subset,]
  # pulls out the species, so we can save them one at a time
  species <- subsertReptileData$Species

  # sets up several lists to store the progress
  full.data.list <- vector("list", length = length(species))
  names(full.data.list) <- species
  meta.list <- vector("list", length = length(species))
  names(meta.list) <- species
  i <- 0

  dir.create(here("data", "inat"), showWarnings = FALSE)
  dir.create(here("data", "inat", "images"), showWarnings = FALSE)
  # this removes all the species that are NULL prior to the last one that has
  # data, a way of jump starting
  if(paste0(subset, "_inat_data_list.rds") %in% list.files(here("data", "inat"))){
    full.data.list <- readRDS(here("data", "inat", paste0(subset, "_inat_data_list.rds")))
    # read in progress, and check which ones have data and not repeat those
    full.data.list[sapply(full.data.list, is.null)] <- NULL
    species <- species[!species %in% names(full.data.list)]
  }

  print("Previous progress checked")

  if(length(species) == 0){
    complete.inat <- plyr::rbind.fill(full.data.list, fill = TRUE)
    return(complete.inat)
  }

  previous <- Sys.time()
  # loop trhough one species at a time
  for(sp in species){
    print(sp)
    # sp <- species[1]
    # sp <- species[747]
    i <- i + 1

    # sp <- "Ophiophagus hannah"
    # sp <- "Vipera berus"
    # sp <- "thamnophis sirtalis"
    # sp <- "Abronia graminea"
    # sp <- "Acanthophis laevis"
    # sp <- "Nephrurus eromanga"

    if(paste0(subset, "_inat_meta_list.rds") %in% list.files("./Data")){
      meta.list <- readRDS(here("data", "inat", paste0(subset, "_inat_meta_list.rds")))
    }
    if(!is.null(full.data.list[[sp]])){
      print("- Already retrieved")
      {next}
    }
    print("Previous metadata checked")

    # a delay every 30 requests to make sure we aren't exceeding the inat quota,
    # using a delay to try and reduce the request to <100 per min, min of 10
    # seconds taken regardless
    if((i/50)%%1 == 0){
      if(as.numeric(difftime(Sys.time(), previous, units = "mins")) < 1){
        delay <- ifelse((as.numeric(difftime(Sys.time(), previous, units = "mins"))-1) *60 < 30,
                        10, (as.numeric(difftime(Sys.time(), previous, units = "mins"))-1) *60)
        print(paste0("Delay: ", delay))
        Sys.sleep(delay)
      }
      previous <- Sys.time()
    }
    if((i/30)%%1==0){
      print("Delay: 5")
      Sys.sleep(5)
    }

    # this is the most important part of the function, everything else is just
    # organising and cleaning. If you want to use alternative organisation this
    # is really the only part you need to take
    inat.df <- occ(query = sp, from = "inat", limit = inatLimit,
                   inatopts = list(quality = "research"),
                   throw_warnings = FALSE)

    print("inat checked")

    meta.res <- data.frame(species = sp,
                           found = inat.df$inat$meta$found,
                           returned = inat.df$inat$meta$returned)

    # saves the meta information and full info if no observations are found
    if(meta.res$found == 0){
      meta.res$photos <- NA
      meta.list[[sp]] <- meta.res
      print("- None found")
      saveRDS(meta.list, file = here("data", "inat", paste0(subset, "_inat_meta_list.rds")))
      {next}
    }

    sp.df <- inat.df$inat$data[[1]]
    # get rid of null photo observations
    sp.df <- sp.df[,!is.null(sp.df$observation_photos)]
    sp.df$parentID <- NA

    if(length(sp.df$observation_photos[!is.null(sp.df$observation_photos)]) == 0){
      meta.res$photos <- NA
      meta.list[[sp]] <- meta.res
      saveRDS(meta.list, file = here("data", "inat", paste0(subset, "_inat_meta_list.rds")))
      {next}
    }

    print("Merge begin")
    # this loop pulls out the photo information and creates a new full dataframe,
    # where instead of each row == an observation, each row now becomes a photo with
    # the observation duplicated
    full.list <- vector("list", length = nrow(sp.df))
    for(j in 1:nrow(sp.df)){

      temp.meta.df <- sp.df[j,]
      photo.df <- temp.meta.df$observation_photos[[1]]

      photo.df$parentID <- temp.meta.df$id
      temp.meta.df$parentID <- temp.meta.df$id

      temp.meta.df <- temp.meta.df %>%
        select(!where(is.list))

      full.list[[j]] <- left_join(photo.df, temp.meta.df, by = "parentID")

    }
    full.data <- do.call(rbind, full.list)
    # swap out the square.jpg for large to ensure we get a decent version of the
    # image, seems like original is not always available, so large is a safer
    # bet(?)
    full.data$photo.url <- sub("square", "large", full.data$photo.url)
    full.data$reptiledbSpecies <- sp

    full.data <- full.data %>%
      rename(
        "photo.uuid" = uuid.x,
        "record.id" = id.y,
        "record.uuid" = uuid.y)

    meta.res$photos <- nrow(full.data)
    print(paste0(meta.res$photos, " photos of ", sp))

    full.data.list[[sp]] <- full.data

    print("Save partial results")
    # second save to help ID errors
    write_csv(x = full.data, file = here("data", "inat", paste0(sp, ".csv")))

    saveRDS(full.data.list, file = here("data", "inat", paste0(subset, "_inat_data_list.rds")))
    saveRDS(meta.list, file = here("data", "inat", paste0(subset, "_inat_meta_list.rds")))
  } # for loop end
  # put them all together, missing columns infilled with NA
  complete.inat <- plyr::rbind.fill(full.data.list, fill = TRUE)
  # complete.inat <- do.call(rbind, full.data.list)
  return(complete.inat)
}
