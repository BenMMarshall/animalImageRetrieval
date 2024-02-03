# install.packages("spocc")
library(spocc)
library(stringr)

repdb.data <- read.csv("./Data/reptile_checklist_2020_04.csv", stringsAsFactors = FALSE)

species <- repdb.data$Species

photo.list <- vector("list", length = length(species))
names(photo.list) <- species
meta.list <- vector("list", length = length(species))
names(meta.list) <- species
i <- 0

dir.create("./Data/inat split/")

# this removes all the species that are NULL prior to the last one that has
# data, a way of jump starting
if("inat_images_list.rds" %in% list.files("./Data")){
  photo.list <- readRDS("./Data/inat_images_list.rds")
}
tail(which(!unlist(lapply(photo.list, is.null))), 1)
species.to.do <- species[-1:-(tail(which(!unlist(lapply(photo.list, is.null))), 1)-20)]

# species.to.do <- species[-1:-(tail(which(names(photo.list) == "Micrurus serranus"), 1)-20)]
# species.to.do <- species[-1:-(tail(which(names(photo.list) == "Pararhadinaea melanogaster"), 1)-20)]


previous <- Sys.time()
# for(sp in species.to.do){
for(sp in species){
  print(sp)
  # sp <- species[1]
  # sp <- species[747]
  i <- i+1
  
  # sp <- "Ophiophagus hannah"
  # sp <- "Vipera berus"
  # sp <- "thamnophis sirtalis"
  # sp <- "Abronia graminea"
  # sp <- "Acanthophis laevis"
  # sp <- "Acanthodactylus haasi"
  # gen <- "Acanthophis"
  
  if("inat_images_list.rds" %in% list.files("./Data")){
    photo.list <- readRDS("./Data/inat_images_list.rds")
  }
  if("inat_meta_list.rds" %in% list.files("./Data")){
    meta.list <- readRDS("./Data/inat_meta_list.rds")
  }
  if(!is.null(photo.list[[sp]])){
    print("- Already retrieved")
    {next}
  }
  
  # a delay every 30 requests to make sure we aren't exceeding the inat quota,
  # using a delay to try and reduce the request to <100 per min, min of 10
  # seconds taken regardless
  if((i/50)%%1 == 0){
    if(as.numeric(difftime(Sys.time(), previous, units = "mins")) < 1){
      delay <- ifelse( (as.numeric(difftime(Sys.time(), previous, units = "mins"))-1) *60 < 30,
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
  
  inat.df <- occ(query = sp, from = "inat", limit = 10000, 
                 inatopts = list(quality = "research"),
                 throw_warnings = FALSE)
  
  meta.res <- data.frame(species = sp,
                         found = inat.df[[3]][[1]]$found,
                         returned = inat.df[[3]][[1]]$returned)
  
  if(meta.res$found == 0){
    meta.res$photos <- NA
    meta.list[[sp]] <- meta.res
    print("- None found")
    saveRDS(meta.list, file = "./Data/inat_meta_list.rds")
    {next}
  }
  
  sp.df <- inat.df[[3]][[2]][[1]]
  
  if(length(sp.df$observation_photos[!is.null(sp.df$observation_photos)]) == 0){
    meta.res$photos <- NA
    meta.list[[sp]] <- meta.res
    saveRDS(meta.list, file = "./Data/inat_meta_list.rds")
    {next}
  }
  
  observation.photos.raw <- sp.df$observation_photos
  
  names(observation.photos.raw) <- 
    inat.df[[3]][[2]][[1]]$name
  
  # remove those with no photo info
  observation.photos <- observation.photos.raw[unlist(lapply(
    observation.photos.raw,
    function(x){
      !length(x) == 0
    }))]
  
  meta.res$photos <- length(unlist(lapply(observation.photos, function(x){
    x$photo.url[!is.na(x$photo.url)]
  })))
  
  print(paste0(meta.res$photos, " photos of ", sp))
  
  meta.list[[sp]] <- meta.res
  
  # j <- 0
  photo.res <- do.call(rbind, lapply(observation.photos, function(x){
    # x <- observation.photos[[1]]
    # j <- j+1
    data.frame(id = x$id,
               photo.id = x$photo.id,
               uuid = x$uuid,
               license = x$photo.license_code,
               url = sub("square", "large", x$photo.url),
               attribution = x$photo.attribution
               )
  }))
  
  # we need to extract the metadata from the obs photo object because there
  # doesnt seem to be an ID that connects images to the records neatly
  # get the index for the repeats, ie how many images per record
  photo.metadatarepeats <- unlist(lapply(observation.photos.raw, function(x){
    dim(x)[1]
  }))
  
  # need to remove the data connected to NULL photos
  photo.metadatarepeats <- photo.metadatarepeats[unlist(lapply(
    observation.photos.raw,
    function(x){
      !length(x) == 0
    }))]
  # and also from the records information because we what to skip the records
  # with NULL photo info
  sp.df <- sp.df[unlist(lapply(observation.photos.raw, function(x){
    dim(x)[1]
  })),]
  # that way all the data and the repeats will match up to be the same length
  # sum(photo.metadatarepeats)
  # dim(sp.df)
  # dim(photo.res)
  # length(photo.metadatarepeats)
  # length(observation.photos)
  # length(observation.photos.raw)
  
  # use that info to duplicate the meta data the correct number of times to
  # match the number of images
  dup_meta <- function(x, repeats.index){
    n.i <- 0
    out <- NULL
    for(n in x){
      n.i <- n.i + 1
      out <- c(out, rep(n, times = repeats.index[n.i]))
    }
    return(out)
  }
  
  add.meta <- apply(sp.df[,c("latitude", "longitude", "positional_accuracy", "observed_on",
                             "taxon.id")],
        2, function(x){dup_meta(x = x, repeats.index = photo.metadatarepeats)})
  
  if(is.null(dim(add.meta))){
    add.meta <- data.frame(latitude = add.meta["latitude"],
                           longitude = add.meta["longitude"],
                           positional_accuracy = add.meta["positional_accuracy"],
                           observed_on = add.meta["observed_on"]
    )
  } else {
    add.meta <- as.data.frame(add.meta)
  }
  add.meta[,1:3] <- apply(add.meta[,1:3], 2, as.numeric)
  add.meta$species.inat <- dup_meta(names(observation.photos), photo.metadatarepeats)
  
  # photo.res$species.inat <- gsub("\\.", "", gsub("[[:digit:]]", "", row.names(photo.res)))
  photo.res$species <- sp
  photo.res <- cbind(photo.res, add.meta)
  
  photo.list[[sp]] <- photo.res
  
  # second save to help ID errors
  write.csv(x = photo.res, file = paste0("./Data/inat split/", sp, ".csv"),
            row.names = FALSE)
  
  saveRDS(photo.list, file = "./Data/inat_images_list.rds")
  saveRDS(meta.list, file = "./Data/inat_meta_list.rds")
} # for loop end

saveRDS(photo.list, file = "./Data/inat_images_list.rds")
saveRDS(meta.list, file = "./Data/inat_meta_list.rds")

# some seem to miss taxon.id, not sure why
names(photo.list[["Blanus mettetali"]])
names(photo.list[[14]])

unlist(lapply(photo.list, function(x){
  dim(x)[2]
}))

photo.df <- do.call(rbind, photo.list)
library(stringr)
row.names(photo.df) <- NULL
photo.df$license <- as.character(photo.df$license)
photo.df$license <- ifelse(is.na(photo.df$license), str_extract(photo.df$attribution,
                                            "all rights reserved|no rights reserved"),
       photo.df$license)

table(photo.df$license)

write.csv(photo.df, file = "./Data/PRELIM_inat_images.csv",
          row.names = FALSE)

meta.df <- do.call(rbind, meta.list)

write.csv(meta.df, file = "./Data/PRELIM_inat_meta.csv",
          row.names = FALSE)

mean(meta.df$photos, na.rm = TRUE)
sum(is.na(meta.df$photos))
sum(meta.df$photos > 10000, na.rm = TRUE)
