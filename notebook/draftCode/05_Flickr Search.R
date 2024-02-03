
library(dplyr)
library(jsonlite)
library(RCurl)

#API key from https://www.flickr.com/services/api/misc.api_keys.html

URLbase <- "https://api.flickr.com/services/rest/?method=flickr.photos.search"

# generate the string of tags to limit the images to reptiles
tags.vec <- c("snake", "reptile", "tortoise", "turtle", "lizard", "crocodile", "alligator",
              "crocodilian", "amphisbaenian", "tuatara", "gharial", "caiman")
# tags <- "snake"
tags.coll <- paste0(tags.vec, collapse = ",")

repdb.data <- read.csv("./Data/reptile_checklist_2020_04.csv", stringsAsFactors = FALSE)
species <- repdb.data$Species

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

  ### SOMETHING WRONG WITH SPECIAL CHARACTERS HERE FOR THIS SPECIES
  # if(sp %in% c("Acanthodactylus aegyptius")){
  #   {next}
  # }

  # check if there is already progress in the results and load if there is
  if(i == 1 & (any(list.files("./Data/") %in% "flickr_results.Rds")) ){
    flickr.list <- readRDS(file = "./Data/flickr_results.Rds")
    meta.results <- readRDS(file = "./Data/flickr_meta.Rds")
  }
  # check if the species has already been searched
  if( !is.null(flickr.list[[sp]]) ){
    print(paste0(sp, " - skipped"))
    {next}
  }

  # get the results for the species
  flickr.df <- search_flickr(search = sp,
                      api_key = api_key,
                      tags = tags.coll,
                      tag_m = "any",
                      content_type = "1", # photos only
                      extra_call = "date_taken,geo,license",
                      page_call = "250")

  meta.results[[sp]] <- flickr.df[[1]]
  saveRDS(meta.results, file = "./Data/flickr_meta.Rds", compress = FALSE)
  flickr.list[[sp]] <- flickr.df[[2]]
  saveRDS(flickr.list, file = "./Data/flickr_results.Rds", compress = FALSE)
  print(paste0("--- ", sp, ": End and Saved ---"))

}
# saveRDS(flickr.list, file = "./Data/flickr_results.Rds", compress = FALSE)

meta.fullresults <- do.call(rbind, meta.results)

write.csv(x = meta.fullresults, file = "./Data/flickr_meta_list.csv",
          row.names = FALSE)

flickr.list[["Agkistrodon contortrix"]]
flickr.fullresults <- do.call(rbind, flickr.list)
flickr.fullresults %>%
  filter(!is.na(url))

write.csv(x = flickr.fullresults, file = "./Data/flickr_images_list.csv",
          row.names = FALSE)


# likely need to run a check afterwards to make sure that the meta results
# numbers match teh size of the dataframe in flickr.list for each species



# Older loop prior to function creation -----------------------------------



for(sp in repdb.names){
  # sp <- species[770]
  # sp <- "Naja kaouthia"

  i <- i + 1

  if(any(list.files() %in% "flickr_results.Rds")){
    flickr.results <- readRDS(file = "flickr_results.Rds")
  }

  if( !is.null(flickr.results[[i]]) ){
    print(paste0(sp, " - skipped"))
    {next}
  }

  search.text <- URLencode(sp)

  URL.call <- paste0(URLbase, "&api_key=", api_key,
                     "&tags=", tags, "&tag_mode=", tag_m,
                     "&text=", search.text,
                     "&bbox=",
                     "&license=",
                     "&content_type=", content_type,
                     # "&min_taken_date=", min.date, "&max_taken_date=", max.date,
                     # "&has_geo", geo.call,
                     "&extras=", extra.call,
                     "&per_page=", page.call,
                     "&page=1",
                     "&format=json&nojsoncallback=1"
  )

  raw.data <- RCurl::getURL(URL.call, ssl.verifypeer = TRUE)
  json.data <- jsonlite::fromJSON(raw.data)
  tot.pages <- json.data$photos$pages
  num.res <- json.data$photos$total

  meta.results[[i]] <- data.frame("species" = sp,
                                  "results" = num.res)
  print(paste("----- Search:", search.text, "-", num.res,
              "results -----"))

  if(num.res == 0){
    {next}
  }

  all.calls <- vector("list", tot.pages)
  for(page in 1:tot.pages){
    # page <- 1
    print(paste0("Page: ", page))
    URL.call.page <- sub("page=1", paste0("page=", page),
                         URL.call)
    raw.data <- RCurl::getURL(URL.call.page, ssl.verifypeer = TRUE)
    json.data <- jsonlite::fromJSON(raw.data)
    data <- as.data.frame(json.data)
    data[,c("photos.photo.latitude", "photos.photo.longitude")] <-
      mapply(as.numeric, data[,c("photos.photo.latitude", "photos.photo.longitude")])
    data$page <- page
    data$kw <- search.text
    all.calls[[page]] <- data
    print("Waiting... give the API a break")
    Sys.sleep(10)
  }
  images.df <- do.call(rbind, all.calls)

  # required to fully skip zero result species
  if(is.null(images.df)){
    na.df <- as.data.frame(matrix(ncol = length(names(images.df)), nrow = 1))
    names(na.df) <- names(images.df)
    flickr.results[[i]] <- na.df
    saveRDS(flickr.results, file = "flickr_results.Rds", compress = FALSE)
    {next}
  }

  images.df$species <- sp

  images.df <- images.df %>%
    mutate(url =  paste0("https://www.flickr.com/photos/",
                         photos.photo.owner,
                         "/", photos.photo.id, "/"))

  flickr.results[[i]] <- images.df
  saveRDS(flickr.results, file = "flickr_results.Rds", compress = FALSE)
  print(paste0("--- ", sp, ": End and Saved ---"))

} # end of repdbname loop
saveRDS(flickr.results, file = "flickr_results.Rds", compress = FALSE)

flickr.results <- readRDS(file = "flickr_results.Rds")
# f.res <- dplyr::bind_rows(flickr.results)
write.csv(file = "./Data/Flickr_results.csv", row.names = FALSE,
          x = f.res)
