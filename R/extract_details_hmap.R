#' Get voucher info from hmap records
#'
#' @name extract_details_hmap
#' @description A
#' @param abc abc
#' @return a
#'
#' @export
extract_details_hmap <- function(html.file){
  # html.file <- files[5]
  html.data <- read_html(html.file,
                         encoding = "UTF-8")

  details <- html.data %>%
    html_nodes("td a") %>%
    html_text()

  species <- html.data %>%
    html_nodes("td i") %>%
    html_text()

  dates <- html.data %>%
    html_nodes("span") %>%
    html_text()

  countries <- html.data %>%
    html_nodes(".hidden-xs") %>%
    html_text()

  record.id <- apply(str_extract_all(details[str_detect(details, "[[:digit:]]")],
                                     "[[:digit:]]", simplify = TRUE), 1, function(x){
                                       paste0(x, collapse = "")
                                     })

  dates <- dates[str_detect(dates, "Observed: ")]
  dates <- sub("Observed: ", "", dates)

  countries <- str_replace_all(countries, "\\t", " ")
  countries <- str_replace_all(countries, "\\r", " ")
  countries <- str_replace_all(countries, "\\n", ", ")
  countries <- str_replace_all(countries, "[[:digit:]]", " ")
  countries <- countries[!str_detect(countries, "Country")]
  if(!str_detect(countries[length(countries)], "[[A-Z]]")){
    countries <- countries[1:length(countries)-1]
  }
  countries <- sub("^..", "", gsub("\\s+", " ", str_trim(countries)))
  countries <- gsub(" , ", ", ", countries)

  recordURL <- paste0("https://www.herpmapper.org/record/",
                      record.id)

  voucherPlusList <- vector("list", length = length(recordURL))
  i <- 0
  for(rurl in recordURL){
    # rurl <- "https://www.herpmapper.org/record/314839"
    i <- i+1
    print(rurl)
    voucherInfo <- read_html(rurl,
                             encoding = "UTF-8") %>%
      html_elements("body") %>%
      html_elements("a.fancybox") %>%
      html_attr("href")
    print(voucherInfo)

    voucherPlusList[[i]] <- data.frame(species[i],
                                       "date" = dates[i],
                                       "location" = countries[i],
                                       record.id = paste0("HM", record.id[i]),
                                       recordURL = rurl,
                                       voucherID = str_extract(voucherInfo, "[0-9]{1,100}"),
                                       voucherURL = paste0("https://www.herpmapper.org", voucherInfo))
  }
  completeRecordInfo <- do.call(rbind, voucherPlusList)

  return(completeRecordInfo)

}
