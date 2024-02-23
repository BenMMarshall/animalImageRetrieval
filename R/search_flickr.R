#' Get info from flickr searches
#'
#' @name search_flickr
#' @description A function to call the flickr API to run a search for images,
#'   then a second call to get the static image URLs.
#' @param search search text string
#' @param api_key Required. flickr API key
#' @param tags e.g., c("snake", "reptile", "tortoise", "turtle", "lizard", "crocodile", "alligator",
#' "crocodilian", "amphisbaenian", "tuatara", "gharial", "caiman")
#' @param tag_m "any"
#' @param content_type "1"
#' @param extra_call "date_taken,geo,license"
#' @param page_call "250"
#' @return A list of two dataframes, one of the meta information, one of the image information.
#'
#' @export
search_flickr <- function(search, api_key, tags, tag_m,
                          content_type, extra_call, page_call){

  search.text <- URLencode(search)
  URLbase <- "https://api.flickr.com/services/rest/?method=flickr.photos.search"
  URL.call <- paste0(URLbase, "&api_key=", api_key,
                     "&tags=", tags, "&tag_mode=", tag_m,
                     "&text=", search.text,
                     "&bbox=",
                     "&license=",
                     "&content_type=", content_type,
                     # "&min_taken_date=", min.date, "&max_taken_date=", max.date,
                     # "&has_geo", geo.call,
                     "&extras=", extra_call,
                     "&per_page=", page_call,
                     "&page=1",
                     "&format=json&nojsoncallback=1"
  )
  raw.data <- RCurl::getURL(URL.call, ssl.verifypeer = TRUE)
  # json.data <- jsonlite::fromJSON(raw.data)
  json.data <- tryCatch(jsonlite::fromJSON(raw.data),
                        error = function(e) "Parse error")
  if(json.data[1] == "Parse error"){
    # paste("ERROR", sp, "pagequery", page, sep = "_")
    dir.create("./Errors", showWarnings = FALSE)
    cat(paste("ERROR", sp, "intialquery", sep = "_"),
        file = paste("./Errors/ERROR", sp, "intialquery.txt",
                     sep = "_"))
    return(list(NULL, NULL))
  }
  tot.pages <- NA
  num.res <- NA
  tot.pages <- json.data$photos$pages
  num.res <- json.data$photos$total

  meta.results <- data.frame("search" = search,
                             "results" = num.res)
  print(paste("----- Search:", search.text, "-", num.res,
              "results -----"))

  if(num.res == 0){
    print("No results, returning NA")
    images.df <- NA
    return(list(meta.results, images.df))
  }

  all.calls <- vector("list", tot.pages)
  for(page in 1:tot.pages){
    # page <- 1
    print(paste0("Page: ", page))
    URL.call.page <- sub("page=1", paste0("page=", page),
                         URL.call)
    raw.data <- RCurl::getURL(URL.call.page, ssl.verifypeer = TRUE)
    # json.data <- jsonlite::fromJSON(raw.data)
    json.data <- tryCatch(jsonlite::fromJSON(raw.data),
                          error = function(e) "Parse error")
    if(json.data[1] == "Parse error"){
      # paste("ERROR", sp, "pagequery", page, sep = "_")
      dir.create("./Errors", showWarnings = FALSE)
      cat(paste("ERROR", sp, "pagequery", page, sep = "_"),
          file = paste("./Errors/ERROR", sp, "pagequery", page, ".txt",
                       sep = "_"))
      {next}
    }

    data <- as.data.frame(json.data)
    data$page <- page
    data$kw <- search.text

    # have to make sure that missing columns are added, this occurs if an entire
    # pages has no geo information
    req.cols <- c("photos.page","photos.pages",
                  "photos.perpage","photos.total",
                  "photos.photo.id","photos.photo.owner",
                  "photos.photo.secret","photos.photo.server",
                  "photos.photo.farm","photos.photo.title",
                  "photos.photo.ispublic","photos.photo.isfriend",
                  "photos.photo.isfamily","photos.photo.license",
                  "photos.photo.datetaken","photos.photo.datetakengranularity",
                  "photos.photo.datetakenunknown","photos.photo.latitude",
                  "photos.photo.longitude","photos.photo.accuracy",
                  "photos.photo.context","photos.photo.place_id",
                  "photos.photo.woeid","photos.photo.geo_is_public",
                  "photos.photo.geo_is_contact","photos.photo.geo_is_friend",
                  "photos.photo.geo_is_family","page","kw","stat")
    missing.cols <- req.cols[which(!req.cols %in%
                                     names(data))]

    data[missing.cols] <- NA
    data[,c("photos.photo.latitude", "photos.photo.longitude")] <-
      mapply(as.numeric, data[,c("photos.photo.latitude", "photos.photo.longitude")])
    all.calls[[page]] <- data
    print("Waiting... give the API a break")
    Sys.sleep(2)
  }
  images.df <- do.call(rbind, all.calls)

  # required to fully skip zero result species
  # if(is.null(images.df)){
  #   na.df <- as.data.frame(matrix(ncol = length(names(images.df)), nrow = 1))
  #   names(na.df) <- names(images.df)
  #   return(na.df)
  # }
  staticURLs <- vector(mode = "character", length = length(unique(images.df$photos.photo.id)))
  i <- 0
  for(imgID in unique(images.df$photos.photo.id)){
    i <- i+1
    # imgID <- unique(images.df$photos.photo.id)[1]
    URLbase <- "https://www.flickr.com/services/rest/?method=flickr.photos.getSizes"
    URL.call <- paste0(URLbase, "&api_key=", api_key,
                       "&photo_id=", imgID,
                       "&format=json&nojsoncallback=1"
    )
    raw.data <- RCurl::getURL(URL.call, ssl.verifypeer = TRUE)

    # json.data <- jsonlite::fromJSON(raw.data)
    json.data <- tryCatch(jsonlite::fromJSON(raw.data),
                          error = function(e) "Parse error")

    if(json.data$sizes$candownload == 1){
      if("Original" %in% json.data$sizes$size$label){
        downURL <- json.data$sizes$size[json.data$sizes$size$label == "Original",]$source
      } else {
        downURL <- json.data$sizes$size[json.data$sizes$size$height == max(json.data$sizes$size$height),]$source
      }
    } else {
      downURL <- "Download not enabled"
    }
    staticURLs[i] <- downURL

    print("Waiting... give the API a break")
    Sys.sleep(1.5)
  }
  images.df$staticURL <- staticURLs

  images.df$search <- search
  images.df <- images.df %>%
    mutate(url =  paste0("https://www.flickr.com/photos/",
                         photos.photo.owner,
                         "/", photos.photo.id, "/"))

  return(list(meta.results, images.df))
}
