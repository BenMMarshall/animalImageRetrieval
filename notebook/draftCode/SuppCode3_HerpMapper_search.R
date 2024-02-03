
library(downloader)
library(xml2)
library(rvest)
library(dplyr)
library(stringr)

dir.create("./Data/HerpMapper Pages/")

orders <- 
  c("Amphisbaenia", 
    "Crocodylia", 
    "Lacertilia", 
    "Serpentes", 
    "Sphenodon", 
    "Testudines")

for(ord in orders){
  order.page <- paste0("https://www.herpmapper.org/records?taxon=", ord,
                       "&p=1")
  download(order.page,
           destfile = paste0("./Data/HerpMapper Pages/",
                             ord, "_search_page1.html"))
}

order.pages <- list.files("./Data/HerpMapper Pages/", pattern = "page1.html", full.names = TRUE)

record.numbers <- unlist(lapply(order.pages, function(x){
  read_html(x,
            encoding = "UTF-8") %>% 
    html_nodes("em") %>% 
    html_text()
}))

records.per.order <- as.numeric(gsub(",", "", word(record.numbers,
     start = dim(str_extract_all(record.numbers, " ", simplify = TRUE))[2],
     end = dim(str_extract_all(record.numbers, " ", simplify = TRUE))[2])))

pages <- ceiling(records.per.order / 25)
# this is fine, but produced an error for the single page Sphenodon. The
# erroniously downloaded second page was manually removed.

names(pages) <- orders

for(ord in orders){
  # ord <- orders[2]
  order.page <- paste0("https://www.herpmapper.org/records?taxon=", ord,
                       "&p=1")
  for(pg in 2:pages[ord]){
    # pg <- 2
    end.file.name <- paste0(ord, "_search_page", pg, ".html")
    
    if(end.file.name %in% list.files("./Data/HerpMapper Pages/", pattern = ".html")){
      {next}
    }
    
    download(sub("1", pg, order.page),
             destfile = paste0("./Data/HerpMapper Pages/",
                               end.file.name))
    Sys.sleep(4)
    
  }
}

HM_extract_details <- function(html.file){
  # html.file <- files[6191]
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
  
  return(data.frame(species,
                    "date" = dates, 
                    "location" = countries,
             record.id = paste0("HM", record.id),
             recordURL))
  
}

files <- list.files("./Data/HerpMapper Pages", pattern = "html",
                    full.names = TRUE)

full.dataset <- do.call(rbind, lapply(files, function(x){
  details.df <- HM_extract_details(x)
  order <- str_extract(x, orders)[!is.na(str_extract(x, orders))]
  details.df$order <- order
  return(details.df)
}))

full.dataset
length(unique(full.dataset$species))

write.csv(full.dataset,
  file = "./Data/herpmapper_images.csv", row.names = FALSE)
