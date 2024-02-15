#' Get info from wiki searches
#'
#' @name search_wiki
#' @description A
#' @param abc abc
#' @return a
#'
#' @export
search_wiki <- function(searchterm = NA){

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
