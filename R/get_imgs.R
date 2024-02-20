#' Download the images and save in source folder
#'
#' @name get_imgs
#' @description A
#' @param abc abc
#' @return a
#'
#' @export
get_imgs <- function(urlData, source, flickrAPI){
  # urlData <- tar_wikiUrldata_Xenosauridae
  # urlData <- tar_flickrUrldata_Xenosauridae
  # urlData <- tar_inatUrldata_Xenosauridae

  # urlData <- tar_hmapUrldata_Amphisbaenia
  # source <- "hmap"

  dir.create(here::here("data"), showWarnings = FALSE)
  dir.create(here::here("data", source), showWarnings = FALSE)
  dir.create(here::here("data", source, "images"), showWarnings = FALSE)

  if(source == "inat"){
    listUrls <- str_split(urlData$photo.url, "/")
    imageNames <- unlist(lapply(listUrls, function(x){
      paste(x[(length(x)-1):length(x)], collapse = "")
    }))
    urlData$imageNames <- paste0(urlData$reptiledbSpecies, "_IMG_", imageNames)
    imageURLS <- urlData$photo.url
  }

  if(source == "wiki"){
    listUrls <- str_split(urlData$image.url, "/")
    imageNames <- unlist(lapply(listUrls, function(x){
      x[length(x)]
    }))
    urlData$imageNames <- paste0(urlData$searched, "_IMG_", imageNames)
    imageURLS <- urlData$image.url
  }

  if(source == "hmap"){
    listUrls <- str_split(urlData$voucherURL, "/")
    imageNames <- unlist(lapply(listUrls, function(x){
      paste(x[(length(x)-1):length(x)], collapse = "")
    }))
    urlData$imageNames <- paste0(urlData$species.i., "_IMG_", imageNames)
    imageURLS <- urlData$voucherURL
  }

  if(source == "flickr"){
    listUrls <- str_split(urlData$staticURL, "/")
    imageNames <- unlist(lapply(listUrls, function(x){
      paste(x[(length(x)-1):length(x)], collapse = "")
    }))
    urlData$imageNames <- paste0(urlData$search, "_IMG_", imageNames)
    imageURLS <- urlData$staticURL
  }

  for(r in 1:nrow(urlData)){
    # r <- 4
    if(!is.na(imageURLS[r])){

      if(!urlData$imageNames[r] %in% list.files(here::here("data", source, "images"))){
        download(url = imageURLS[r],
                 destfile = here::here("data", source, "images",
                                       urlData$imageNames[r]),
                 mode = "wb")
        print(paste("Image Downloaded:", urlData$imageNames[r]))
      } else {
        print("Already downloaded")
        {next}
      }
    }
  }
}
