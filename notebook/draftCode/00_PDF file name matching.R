
list.files("./Data/")

pdfnames <- read.csv(file = "./Data/pdf list.csv", stringsAsFactors = FALSE,
                     encoding = "UTF-8")
names(pdfnames) <- "pdf"
refs <- read.csv(file = "./Data/reptile refs pdfs.csv", stringsAsFactors = FALSE,
                 encoding = "UTF-8")
names(refs) <- c("author", "year", "title", "source", "ref", "pdffilename")

library(stringr)

pdfnames$fauthor <- word(pdfnames$pdf, 1, 1)
pdfnames$year <- apply(str_extract_all(pdfnames$pdf, "[[:digit:]]", simplify = TRUE)[,1:4], 1,
                       function(x){paste(x, collapse = "")})
pdfnames$year <- as.numeric(pdfnames$year)

head(pdfnames)           
pdfnames$keyword <- apply(pdfnames, 1, function(x){
  y <- x[1]
  y <- sub(x[2], "", y)
  y <- sub(x[3], "", y)
  y <- sub(".pdf", "", y)
  y <- sub(".html", "", y)
  y <- sub("[[:punct:]]", "", y)
  return(trimws(y))
})

refs$fauthor <- word(gsub("[[:punct:]]", " ", refs$author), 1, 1)

refs[refs$fauthor == "Barbour",]
pdfnames[pdfnames$fauthor == "Barbour",]
sort(refs$year[refs$fauthor == "Barbour"])
sort(pdfnames$year[pdfnames$fauthor == "Barbour"])

refs$author.match <- FALSE
refs$author.match.col <- FALSE
refs$author.year.match <- FALSE
refs$author.year.match.col <- FALSE
refs$author.year.kw.match <- NA
refs$author.year.col.kw.match <- NA
refs$possibles.col.kw <- NA
refs$possibles <- NA
refs$possibles.kw <- NA
author.year.fails <- list()
author.year.fails.col <- list()
i <- 0
j <- 0
for(row in 1:nrow(pdfnames)){
  cat(row, fill = TRUE)
  # row <- 5091
  pdfnames[row,]
  pdfnames[10:15,]
  
  
  refs[which(refs$fauthor == pdfnames[row,2] &
               refs$year == pdfnames[row,3]),]
  
  # refs[which(refs$fauthor == pdfnames[row,2]),]
  refs$author.match[which(refs$fauthor == pdfnames[row,2])] <- TRUE
  refs$author.match.col[which(str_detect(refs$fauthor, 
                                         coll(pdfnames[row,2],
                                              ignore_case = TRUE)))] <- TRUE
  refs$author.year.match[which(refs$fauthor == pdfnames[row,2] &
                                 refs$year == pdfnames[row,3])] <- TRUE
  refs$author.year.match.col[which(str_detect(refs$fauthor, 
                                              coll(pdfnames[row,2],
                                                   ignore_case = TRUE)) &
                                     refs$year == pdfnames[row,3])] <- TRUE
  
  matches <- which(refs$fauthor == pdfnames[row,2] &
                     refs$year == pdfnames[row,3])
  matches.col <- which(str_detect(refs$fauthor, 
                               coll(pdfnames[row,2],
                                    ignore_case = TRUE)) &
                      refs$year == pdfnames[row,3])
  
  if(length(matches) == 0){
    i <- i+1
    author.year.fails[[i]] <- pdfnames[row,1]
    {next}
  }
  if(length(matches.col) == 0){
    j <- j+1
    author.year.fails.col[[i]] <- pdfnames[row,1]
    {next}
  }
  # cheks how many of the keywords are in the title
  # if(length(matches) == 1){
  #   
  #   refs$author.year.kw.match[matches] <- 
  #     sum(str_detect(refs[matches,]$title,
  #                    sub("[[:punct:]]", "", 
  #                        str_split(pdfnames[row,4], " ", simplify = TRUE))
  #     )) /
  #     length(str_split(pdfnames[row,4], " ", simplify = TRUE))
  #   
  # } else if(length(matches) > 1){
    
    for(r in matches){
      
      keywords <- str_split(pdfnames[row,4], " ", simplify = TRUE)
      keywords <- gsub("[[:punct:]]", "", keywords)
      keywords <- keywords[!keywords == ""]
      
      if(length(keywords) == 0){
        {next}
      }
      
      # r <- matches[1]
      refs$author.year.kw.match[r] <- 
        sum(str_detect(refs[r,]$title,
                       sub("[[:punct:]]", "", 
                           keywords, ignore.case = TRUE))
            ) /
        length(keywords)
      
      refs$author.year.col.kw.match[r] <- 
        sum(str_detect(refs[r,]$title,
                       sub("[[:punct:]]", "", 
                           keywords, ignore.case = TRUE))
            ) /
        length(keywords)
      
      if(refs$author.year.kw.match[r] > 0){
        refs$possibles.kw[r] <- ifelse(!is.na(refs$possibles.kw[r]),
                                       paste0(refs$possibles.kw[r],
                                              pdfnames[row,1], sep = ", "),
                                       pdfnames[row,1])
      } 
      if(refs$author.year.col.kw.match[r] > 0){
        refs$possibles.col.kw[r] <- ifelse(!is.na(refs$possibles.col.kw[r]),
                                       paste0(refs$possibles.col.kw[r],
                                              pdfnames[row,1], sep = ", "),
                                       pdfnames[row,1])
      } 
    } # matches for loop
  
  refs
  
  refs$possibles[matches] <- ifelse(!is.na(refs$possibles[matches]),
                                    paste0(refs$possibles[matches],
                                           pdfnames[row,1], sep = ", "),
                                    pdfnames[row,1])
  refs$possibles.col.kw[matches] <- ifelse(!is.na(refs$possibles.col.kw[matches]),
                                    paste0(refs$possibles.col.kw[matches],
                                           pdfnames[row,1], sep = ", "),
                                    pdfnames[row,1])
  
  # } # single match if statement
  
} # overall for loop

author.year.fails

head(refs)

refs[refs$fauthor == "Barbour",] %>% 
  select(fauthor, year, author.match, author.year.match, author.year.kw.match)

library(dplyr)

refs %>% 
  count(author.match)
refs %>% 
  count(author.match.col)

refs %>% 
  count(author.year.match)
refs %>% 
  count(author.year.match.col)

refs %>% 
  count(author.year.kw.match > 0)

refs %>% 
  mutate(n.poss = str_count(possibles, ".pdf")) %>% 
  count(n.poss)
refs %>% 
  mutate(n.poss = str_count(possibles.col.kw, ".pdf")) %>% 
  count(n.poss)

refs %>% 
  mutate(n.poss = str_count(possibles, ".pdf")) %>% 
  count(n.poss) %>%
  filter(!is.na(n.poss)) %>% 
  ggplot()+
  geom_col(aes(x = n.poss, y = n))

refs %>% 
  mutate(n.poss = str_count(possibles.kw, ".pdf")) %>% 
  count(n.poss)

refs %>% 
  mutate(n.poss = str_count(possibles.kw, ".pdf")) %>% 
  count(n.poss) %>%
  filter(!is.na(n.poss)) %>% 
  ggplot()+
  geom_col(aes(x = n.poss, y = n))

library(ggplot2)

ggplot(refs) +
  geom_histogram(aes(x = author.year.kw.match),
                 binwidth = 0.1)

############












# refs$author.match <- refs$fauthor %in% pdfnames$fauthor
# refs$author.year.match <- ifelse(refs$author.match & refs$year %in% pdfnames$year,
#                                  TRUE, FALSE)
refs$year <- as.character(refs$year)

joined <- left_join(refs, pdfnames, by = c("fauthor", "year"))

# joined$keyword.match[duplicated(joined$pdf) & !is.na(joined$pdf)] <- 
joined[which(str_detect(joined$title, joined$keyword[2])),]

joined$title[joined$title == ""] <- NA
joined$keyword[joined$keyword == ""] <- NA

joined <- joined %>% 
  mutate(kw.match = ifelse(!is.na(keyword) & !is.na(title),
                           str_detect(title, keyword),
                           NA))

joined[which(joined$kw.match),]
joined[5:8,]
joined[which(joined$fauthor == "Cann"),]




################

for(name in pdfnames$fauthor){
  # name <- pdfnames$fauthor[6]
  years <- pdfnames$year[pdfnames$fauthor == name]
  for(yr in years){
    # yr <- years[1]  
    
    matches <- refs[refs$fauthor == name & refs$year == yr,]
    
    if(dim(matches)[2] == 1){
      matches$pdffilename <- pdfnames$
    }
    
    
    
  }
  
  
  if(any(refs$fauthor %in% name)){
    
    refs.trim <- refs[refs$fauthor %in% name,]
    refs.trim$author.match <- TRUE
    
    if(any(refs.trim$year %in% pdfnames$year[pdfnames$fauthor == name])){
      
      refs.trim <- refs.trim[refs.trim$year %in% pdfnames$year[pdfnames$fauthor ==
                                                                 name],]
      refs.trim$pdffilename <- pdfnames$pdf[]
      
      refs.trim$year.match <- TRUE
      
      if(dim(refs.trim)[2] == 1){
        matched.list <- refs.trim[1,]
      }
      
    }
    
  }
  refs[refs$fauthor == pdfnames$fauthor[1],]
  
}


refs$pdffilename <- ifelse(refs$author.match & refs$author.year.match,
                           pdfnames$pdf[pdfnames$fauthor == refs$author.match &
                                          pdfnames$year == refs$year],
                           "no match")

# # 1st author is detected
if(any(refs$fauthor %in% pdfnames$fauthor[1])){
  
  refs.trim <- refs[refs$fauthor %in% pdfnames$fauthor[1],]
  refs.trim$author.match <- TRUE
  
  if(refs.trim$year %in% pdfnames$year[pdfnames$fauthor == pdfnames$fauthor[1]]){
    
    refs.trim <- refs.trim[refs.trim$year %in% pdfnames$year[pdfnames$fauthor ==
                                                               pdfnames$fauthor[1]],]
    refs.trim$year.match <- TRUE
    
    if(dim(refs.trim)[2] == 1){
      matched.list <- refs.trim[1,]
    }
    
  }
  
}
refs[refs$fauthor == pdfnames$fauthor[1],]