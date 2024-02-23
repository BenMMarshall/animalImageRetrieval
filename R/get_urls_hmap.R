#' Search for image locations on hmap
#'
#' @name get_urls_hmap
#' @description A function to find and pull out image locations for HerpMapper.
#'   Unlike other functions for sources this one requires an order by order
#'   approach used for Herpmapper.
#' @param reptileData Reptile database data from the read_reptile_data()
#'   function
#' @param subset Required. For herpmapper this has to be an Order from
#'   c("Amphisbaenia", "Crocodylia", "Lacertilia", "Serpentes", "Sphenodon",
#'   "Testudines")
#' @return A dataframe of image information
#'
#' @export
get_urls_hmap <- function(reptileData, subset){

  # subset <- "Amphisbaenia"
  orders <- subset
    # c("Amphisbaenia",
    #   "Crocodylia",
    #   "Lacertilia",
    #   "Serpentes",
    #   "Sphenodon",
    #   "Testudines")

  for(ord in orders){
    # ord <- orders[2]
    order.page <- paste0("https://www.herpmapper.org/records?taxon=", subset,
                         "&p=1")
    download(order.page,
             destfile = here("data", "hmap", paste0(
               subset, "_search_page1.html")))
  }

  order.pages <- list.files(here("data", "hmap"),
                            pattern = paste0(
                              subset, "_search_page1.html"), full.names = TRUE)

  record.numbers <- unlist(lapply(order.pages, function(x){
    read_html(x,
              encoding = "UTF-8") %>%
      html_nodes("em") %>%
      html_text()
  }))

  records.per.order <- as.numeric(gsub(",", "",
                                       word(record.numbers,
                                            start = dim(str_extract_all(record.numbers, " ", simplify = TRUE))[2],
                                            end = dim(str_extract_all(record.numbers, " ", simplify = TRUE))[2])))

  pages <- ceiling(records.per.order / 25)
  # this is fine, but produced an error for the single page Sphenodon. The
  # erroniously downloaded second page was manually removed.

  names(pages) <- subset

  sum(pages)*4 /60/60 # estimated time to download all pages

  for(ord in orders){
    # ord <- orders[2]
    order.page <- paste0("https://www.herpmapper.org/records?taxon=", ord,
                         "&p=1")

    # skips ordesr with sub 1 page of results
    if(pages[ord] <= 1){
      {next}
    }

    for(pg in 2:pages[ord]){
      # pg <- 2
      end.file.name <- paste0(ord, "_search_page", pg, ".html")

      if(end.file.name %in% list.files(here("data", "hmap"), pattern = ".html")){
        {next}
      }

      download(sub("1", pg, order.page),
               destfile = here("data", "hmap",
                               end.file.name))
      Sys.sleep(4)

    }
  }

  files <- list.files(here("data", "hmap"), pattern = paste0(subset, "_search_page.*.html$"),
                      full.names = TRUE)

  full.dataset <- do.call(rbind, lapply(files, function(x){
    details.df <- extract_details_hmap(x)
    order <- str_extract(x, orders)[!is.na(str_extract(x, orders))]
    details.df$order <- order
    return(details.df)
  }))

  # write.csv(full.dataset,
  #           file = here("data", "hmap", paste0("hmap_", subset, "_urlData.csv")),
  #           row.names = FALSE)

  return(full.dataset)
}
