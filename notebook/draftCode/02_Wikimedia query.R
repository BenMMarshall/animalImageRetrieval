
library(jsonlite)
library(stringr)
library(urltools)

# searchterm <- gsub("\\s", "%20", "Ophiophagus hannah")
# searchterm <- "Acanthodactylus dumerilii"
# searchterm <- "Anolis pogus"
# searchterm <- "Adelphicos quadrivirgatum"
# searchterm <- "Adolfus alleni"
# searchterm <- "Agama hulbertorum"
# searchterm <- "Anguis colchica"
# searchterm <- syn

wiki.image.search <- function(searchterm = NA){
  
  # if(str_detect(searchterm, "\\s")){
  #   searchterm <- gsub("\\s", "%20", searchterm)
  # }
  
  searchterm <- url_encode(searchterm)
  
  # search for pages so we can get the proper title
  search.res <- jsonlite::fromJSON(paste0("http://en.wikipedia.org/w/api.php?action=query&list=search&srsearch=",
                                          searchterm,
                                          "&format=json"))
  
  if(search.res$query$searchinfo$totalhits == 0){
    image.details <- vector("list", length = 1)
    print(paste0("No page found for ", searchterm))
    image.details[[1]] <- data.frame(
      searched = searchterm,
      page.title = NA,
      results = "No pages found",
      image = NA,
      image.url = NA,
      desc.url = NA,
      image.desc = NA,
      user = NA,
      artist = NA,
      copyrighted = NA,
      license = NA,
      usage = NA,
      attribution.req = NA
    )
  } else {
    
    search.df <- as.data.frame(search.res$query$search)
    search.title <- search.df$title[1]
    # convert to a html URL format
    search.title <- url_encode(search.title)
    # remove spaces
    # search.title <- gsub("\\s", "%20", search.title)
    # remove '
    # search.title <- gsub("\\'", "%27", search.title)
    # remove é
    # search.title <- gsub("\\é", "%E9", search.title)
    # seems to work
    toppage.results <- jsonlite::fromJSON(paste0("https://en.wikipedia.org/w/api.php?action=query&format=json&formatversion=2&prop=categories|images&titles=",
                                                 search.title
    ))
    # check the page is a species page
    if(!any(str_detect(toppage.results$query$pages$categories[[1]]$title, 
                       "Category:Articles with 'species' microformats"))){
      image.details <- vector("list", length = 1)
      print(paste0("No page found for ", searchterm))
      image.details[[1]] <- data.frame(
        searched = searchterm,
        page.title = NA,
        results = "No pages found",
        image = NA,
        image.url = NA,
        desc.url = NA,
        image.desc = NA,
        user = NA,
        artist = NA,
        copyrighted = NA,
        license = NA,
        usage = NA,
        attribution.req = NA
      )
      img.df <- do.call(rbind, image.details)
      return(img.df)
    }
    # drop the wikipedia commons logo
    images.names <- toppage.results$query$pages$images[[1]][,2][!str_detect(toppage.results$query$pages$images[[1]][,2], "Commons-logo.svg")]
    # remove the svg images that will not be photographs
    images.names <- images.names[!str_detect(images.names, ".svg")]
    images.names <- images.names[!str_detect(images.names, "File:Red Pencil Icon.png")]
    
    if(is.null(images.names)){
      image.details <- vector("list", length = 1)
      print(paste0("No images found for ", searchterm))
      image.details[[1]] <- data.frame(
        searched = searchterm,
        page.title = search.title,
        results = "No images found",
        image = NA,
        image.url = NA,
        desc.url = NA,
        image.desc = NA,
        user = NA,
        artist = NA,
        copyrighted = NA,
        license = NA,
        usage = NA,
        attribution.req = NA
      )
      img.df <- do.call(rbind, image.details)
      return(img.df)
    }
    
    images.names <- url_encode(images.names)
    n.images <- length(images.names)
    
    if(n.images >= 1){
      print(paste0(n.images, " images found"))
      i <- 0
      image.details <- vector("list", length = n.images)
      for(img in gsub("\\s", "%20", images.names)){
        i <- i+1
        image.info <- jsonlite::fromJSON(paste0("https://en.wikipedia.org/w/api.php?action=query&format=json&formatversion=2&titles=",
                                                img,
                                                "&prop=imageinfo&&iiprop=timestamp|extmetadata|user|url"))
        image.details[[i]] <- data.frame(
          searched = searchterm,
          page.title = search.title,
          results = "Images found",
          image = img,
          image.url = image.info$query$pages$imageinfo[[1]]$url,
          desc.url = image.info$query$pages$imageinfo[[1]]$descriptionurl,
          image.desc = ifelse(!is.null(image.info$query$pages$imageinfo[[1]]$extmetadata$ImageDescription[1,1]), 
                              image.info$query$pages$imageinfo[[1]]$extmetadata$ImageDescription[1,1],
                              NA),
          user = image.info$query$pages$imageinfo[[1]]$user,
          artist = ifelse(!is.null(image.info$query$pages$imageinfo[[1]]$extmetadata$Artist[1,1]), 
                          image.info$query$pages$imageinfo[[1]]$extmetadata$Artist[1,1],
                          NA),
          copyrighted = ifelse(!is.null(image.info$query$pages$imageinfo[[1]]$extmetadata$Copyrighted[1,1]), 
                               image.info$query$pages$imageinfo[[1]]$extmetadata$Copyrighted[1,1],
                               NA),
          license = ifelse(!is.null(image.info$query$pages$imageinfo[[1]]$extmetadata$License[1,1]), 
                           image.info$query$pages$imageinfo[[1]]$extmetadata$License[1,1],
                           NA),
          usage = ifelse(!is.null(image.info$query$pages$imageinfo[[1]]$extmetadata$UsageTerms[1,1]), 
                         image.info$query$pages$imageinfo[[1]]$extmetadata$UsageTerms[1,1],
                         NA),
          attribution.req = ifelse(!is.null(image.info$query$pages$imageinfo[[1]]$extmetadata$AttributionRequired[1,1]), 
                                   image.info$query$pages$imageinfo[[1]]$extmetadata$AttributionRequired[1,1],
                                   NA)
        )
      } # end of for
    } else if(n.images < 1){ # end of first if
      image.details <- vector("list", length = 1)
      print(paste0("No images found for ", searchterm))
      image.details[[1]] <- data.frame(
        searched = searchterm,
        page.title = search.title,
        results = "No images found",
        image = NA,
        image.url = NA,
        desc.url = NA,
        image.desc = NA,
        user = NA,
        artist = NA,
        copyrighted = NA,
        license = NA,
        usage = NA,
        attribution.req = NA
      )
    }# end of else
    
  } # end of large if else for no page
  img.df <- do.call(rbind, image.details)
  return(img.df)
} # end of function


# images.df <- wiki.image.search(searchterm = searchterm)
# as.data.frame(apply(images.df, 2, as.character))

# Looping the function ----------------------------------------------------

repdb.data <- read.csv("./Data/reptile_checklist_2020_04.csv", stringsAsFactors = FALSE)

species <- repdb.data$Species
i <- 0
images.list <- vector("list", length(species))
for(sp in species){
  i = i+1
  # sp <- species[i]
  print(sp)
  
  if("wiki_images_list.rds" %in% list.files("./Data")){
    images.list <- readRDS("./Data/wiki_images_list.rds")
  }
  if(!is.null(images.list[[i]])){
    print("Already retrieved")
    {next}
  }
  
  images.list[[i]] <- wiki.image.search(sp)
  saveRDS(images.list, file = "./Data/wiki_images_list.rds")
}

images.list <- readRDS("./Data/wiki_images_list.rds")

wiki.images <- do.call(rbind, images.list)
wiki.images <- as.data.frame(apply(wiki.images, 2, as.character))

# Summary -----------------------------------------------------------------
library(dplyr)

# write.csv(x = wiki.images, file = paste0("./Data/wiki_images_", as.Date(Sys.time()),"_raw.csv"),
#           row.names = FALSE)
wiki.images$desc.url
sum(!is.na(wiki.images$image.url))

wiki.images$page.title <- as.character(wiki.images$page.title)

wiki.images$page.title[!str_detect(wiki.images$page.title, "%20") & !is.na(wiki.images$page.title)]

table(wiki.images$results)

sp.images <- wiki.images %>% 
  filter(results == "Images found") %>% 
  mutate(likely.genus.only = !str_detect(page.title, "%20") & !is.na(page.title),
         attribution.req = ifelse(attribution.req == "true", TRUE,
                                  ifelse(attribution.req == "false", FALSE, NA)),
         copyrighted = ifelse(copyrighted == "True", TRUE,
                              ifelse(copyrighted == "False", FALSE, NA)),
         distribution = str_detect(image.url, fixed("distribution", ignore_case = TRUE))
  ) %>% 
  filter(
    !likely.genus.only,
    !distribution,
    !duplicated(image.url))

length(unique(sp.images$searched))

sp.images %>% 
  count(license) %>% 
  print(n = 100)
sp.images %>% 
  count(copyrighted)

# write.csv(x = sp.images, file = paste0("./Data/wiki_images_", as.Date(Sys.time()),"_filtered.csv"),
#           row.names = FALSE)


# synonyms ----------------------------------------------------------------

sp.images <- read.csv(file = paste0("./Data/wiki_images_2020-05-26_filtered.csv"),
                      stringsAsFactors = FALSE)

repdb.data <- read.csv("./Data/reptile_checklist_2020_04.csv", stringsAsFactors = FALSE)

# wokring out which species to try again with synomyns
no.img.spp <- repdb.data[(!repdb.data$Species %in% url_decode(sp.images$searched)),]

syno.names <- read.csv("./Data/reptile names 2019 syno.csv", stringsAsFactors = FALSE)

miss.img.syno <- syno.names[syno.names$species %in% no.img.spp$Species,]
names(miss.img.syno) <- c("synonym", "species", "x")

species <- unique(miss.img.syno$species)
i <- 0

sum(species == "NA")

########## JUMP START ###########
# i <- 3200
# species <- species[3200:length(species)]
# i <- 8657
# species <- species[8657:length(species)]

images.list <- vector("list", length(species))
for(sp in species){
  i <- i+1
  # sp <- species[i]
  print(sp)
  miss.df <- miss.img.syno[miss.img.syno$species == sp,]
  
  if("wiki_images_list_synonyms.rds" %in% list.files("./Data")){
    images.list <- readRDS("./Data/wiki_images_list_synonyms.rds")
  }
  if(!is.null(images.list[[i]])){
    print("Already retrieved")
    {next}
  }
  
  synonyms <- unique(miss.df$synonym)
  # remove the accepted names as we have already searched for that
  synonyms <- synonyms[!synonyms %in% sp]
  synonyms <- synonyms[!synonyms == ""]
  
  if(length(synonyms) == 0){
    print("No results - no synonyms")
    {next}
  }
  
  temp.res.list <- vector("list", length(synonyms))
  j <- 0
  for(syn in synonyms){
    j <- j+1
    temp.res.list[[j]] <- wiki.image.search(syn)
  }# for loop end
  temp.res <- do.call(rbind, temp.res.list)
  
  if(is.null(temp.res)){
    print("No results")
    {next}
  }
  
  temp.res$acc.species <- sp
  
  images.list[[i]] <- temp.res
  saveRDS(images.list, file = "./Data/wiki_images_list_synonyms.rds")
}

images.list <- readRDS("./Data/wiki_images_list_synonyms.rds")

do.call(rbind, images.list)


# Compile -----------------------------------------------------------------
library(dplyr)

images.list <- readRDS("./Data/wiki_images_list_synonyms.rds")

images.df <- do.call(rbind, images.list)

head(images.df)

images.df <- images.df %>% 
  mutate(likely.genus.only = !str_detect(page.title, "%20") & !is.na(page.title),
         attribution.req = ifelse(attribution.req == "true", TRUE,
                                  ifelse(attribution.req == "false", FALSE, NA)),
         copyrighted = ifelse(copyrighted == "True", TRUE,
                              ifelse(copyrighted == "False", FALSE, NA)),
         distribution = str_detect(image.url, fixed("distribution", ignore_case = TRUE))
  )

write.csv(x = images.df, file = "./Data/wiki_images_synonyms_2020-06-19.csv", 
          row.names = FALSE)

images.df.filtered <- images.df %>% 
  filter(
    !likely.genus.only,
    !distribution,
    !duplicated(image.url))

write.csv(x = images.df.filtered,
          file = "./Data/wiki_images_synonyms_filtered_2020-06-19.csv", 
          row.names = FALSE)


# Testing -----------------------------------------------------------------


# install.packages("WikipediaR")

library(WikipediR)

page <- WikipediR::page_content(language = "en",
                                project = "commons",
                                page_id = 168503)

page_links(project = "commons", page = "Ophiophagus hannah",
           clean_response = TRUE)
page_external_links(project = "commons", page = "Ophiophagus hannah",
                    clean_response = TRUE)


library(XML)
page <- "https://www.wikidata.org/wiki/Q48186"
html.nodes <- page %>% 
  xml2::read_html() %>% 
  html_nodes(xpath = "//td")

html.nodes[[3]]

h.url <- page %>% 
  xml2::read_html() %>% 
  html_children()

jsonlite::fromJSON()

str(h.url)

# xpathApply(htmlTreeParse(h.url, useInternal = TRUE), '//td',
#            function(x) xpathSApply(x,".//text()", xmlValue))



jsonlite::fromJSON("https://en.wikipedia.org/w/api.php?action=query&format=json&titles=surfing&prop=images")
# jsonlite::fromJSON("https://mediawiki.org/w/api.php/w/api.php/w/api.php?action=query&format=json&prop=images%7Cimageinfo&list=search&indexpageids=1&iwurl=1&srsearch=Ophiophagus%20hannah&srnamespace=0")

jsonlite::fromJSON("https://en.wikipedia.org/w/api.php?action=query&format=json&titles=ophiophagus%20hannah&prop=images")

jsonlite::fromJSON("https://en.wikipedia.org/w/api.php?action=query&prop=pageimages&format=json&prop=images%7Cimageinfo&list=search&indexpageids=1&iwurl=1&srsearch=Ophiophagus%20hannah&srnamespace=0")

# "https://en.wikipedia.org/w/api.php?action=query&prop=pageimages&format=json&piprop=original&titles=India"
jsonlite::fromJSON("https://en.wikipedia.org/w/api.php?action=query&titles=ophiophagus%20hannah&prop=pageimages&format=json&pithumbsize=100")
jsonlite::fromJSON("https://en.wikipedia.org/w/api.php?action=query&titles=Al-Farabi&prop=pageimages&format=json&pithumbsize=100")
jsonlite::fromJSON("https://en.wikipedia.org/w/api.php?action=query&format=json&prop=pageimages%7Cpageterms&titles=Albert%20Einstein")
jsonlite::fromJSON("https://en.wikipedia.org/w/api.php?action=query&format=json&prop=pageimages%7Cpageterms&titles=king_cobra")

jsonlite::fromJSON("https://en.wikipedia.org/w/api.php?action=query&format=json&titles=king_cobra&prop=imageinfo&iiprop=url")



library(jsonlite)
# # MySearch <- function(srsearch){
# jsonlite::fromJSON("http://en.wikipedia.org/w/api.php?action=query&list=search&srsearch=king%20cobra&format=json")
#   Response <- fromJSON(FullSearchString)
#   # return(Response)
# # }
# Response <- MySearch("Richard%20Dawkins")
# 

# SOMETHING THAT WORKS ----------------------------------------------------
# search for pages so we can get the proper title
# (search.res <- jsonlite::fromJSON("http://en.wikipedia.org/w/api.php?action=query&list=search&srsearch=king%20cobra&format=json"))
(search.res <- jsonlite::fromJSON("http://en.wikipedia.org/w/api.php?action=query&list=search&srsearch=ophiophagus%20hannah&format=json"))
search.df <- as.data.frame(search.res$query$search)
search.df$title[1]
# seems to work
toppage.results <- jsonlite::fromJSON(paste0("https://en.wikipedia.org/w/api.php?action=query&format=json&formatversion=2&prop=images&titles=",
                                             gsub("\\s", "%20", search.df$title[1])))
toppage.results$query$pages$images[[1]][1,2]

image.info <- jsonlite::fromJSON(paste0("https://en.wikipedia.org/w/api.php?action=query&format=json&formatversion=2&titles=",
                                        gsub("\\s", "%20", toppage.results$query$pages$images[[1]][1,2]),
                                        "&prop=imageinfo&&iiprop=timestamp|extmetadata|user|url"))

image.details <- data.frame(
  image.url = image.info$query$pages$imageinfo[[1]]$url,
  desc.url = image.info$query$pages$imageinfo[[1]]$descriptionurl,
  image.desc = image.info$query$pages$imageinfo[[1]]$extmetadata$ImageDescription[1,1],
  user = image.info$query$pages$imageinfo[[1]]$user,
  artist = image.info$query$pages$imageinfo[[1]]$extmetadata$Artist[1,1],
  copyrighted = image.info$query$pages$imageinfo[[1]]$extmetadata$Copyrighted[1,1],
  license = image.info$query$pages$imageinfo[[1]]$extmetadata$License[1,1],
  usage = image.info$query$pages$imageinfo[[1]]$extmetadata$UsageTerms[1,1],
  attribution.req = image.info$query$pages$imageinfo[[1]]$extmetadata$AttributionRequired[1,1])


# jsonlite::fromJSON("https://en.wikipedia.org/w/api.php?action=query&format=json&formatversion=2&prop=images&titles=king_cobra")

#################
# &pilicense=any
#################



# content <- WikipediR::page_content(language = "en",
#                         project = "commons",
#                         # page_name = "ophiophagus hannah",
#                         page_id = "168503"
#                         )


content$parse$pageid

html.tree <- XML::htmlTreeParse(h.url, useInternal = TRUE,
                                isHTML = TRUE)

httr::GET("https://upload.wikimedia.org/wikipedia/commons/thumb/9/98/Ophiophagus-hannah-kaeng-krachan-national-park.jpg/640px-Ophiophagus-hannah-kaeng-krachan-national-park.jpg")

jsonlite::fromJSON(rawToChar(page))

XML::readHTMLTable(page$parse)

raw.data <- RCurl::getURL("https://mediawiki.org/w/api.php/w/api.php/w/api.php?action=query&format=json&prop=images%7Cimageinfo&list=search&indexpageids=1&iwurl=1&srsearch=Ophiophagus%20hannah&srnamespace=0",
                          ssl.verifypeer = TRUE)

json.data <- jsonlite::fromJSON(raw.data)


library(rvest)    
scotusURL <- "https://en.wikipedia.org/wiki/List_of_Justices_of_the_Supreme_Court_of_the_United_States"

h.url <- scotusURL %>% 
  xml2::read_html()

html_nodes(h.url, xpath = "//td")[10]
XML::htmlTreeParse(h.url, useInternal = TRUE)

html_table(temp[1]) ## Just the "legend" table
html_table(temp[2]) ## THE MAIN TABLE

