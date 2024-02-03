
cap.files <- list.files("C:/Users/benma/Dropbox/reptile papers with images extracted", recursive = TRUE,
           pattern = ".txt", full.names = TRUE)

cap.df <- do.call(rbind, lapply(cap.files, function(x){
  y <- read.table(x, sep = "\n", header = FALSE)
  filename <- sub("C:/Users/benma/Dropbox/reptile papers with images extracted","",
                  x)
  z <- data.frame(file = filename, y)
  return(z)
})
)
cap.df$V1 <- as.character(cap.df$V1)

library(stringr)

repdb.data <- read.csv(file = "./Data/reptile_checklist_2020_04.csv",
                       stringsAsFactors = FALSE, encoding = "UTF-8")
####### NEED SOMETHING WITH SYNONYMS IN IT ##################################

species <- repdb.data$Species

cap.df$sp.detected <- unlist(lapply(cap.df[,2], function(x){
  ifelse(!length(species[str_detect(x, coll(species, ignore_case = TRUE))]) == 0,
         species[str_detect(x, coll(species, ignore_case = TRUE))],
         NA)
}))

# species[str_detect(cap.df$V1[1], coll(species, ignore_case = TRUE))]
# species[str_detect(cap.df$V1[15], coll(species, ignore_case = TRUE))]

species.abbr <- paste0(str_extract(word(species, 1, 1), "^."), ". ", word(species, 2, 2))

cap.df$spabbr.detected <- unlist(lapply(cap.df[,2], function(x){
  ifelse(!length(species.abbr[str_detect(x, coll(species.abbr, ignore_case = TRUE))]) == 0,
         species.abbr[str_detect(x, coll(species.abbr, ignore_case = TRUE))],
         NA)
}))

# ifelse(length(species.abbr[str_detect(cap.df$V1[1], coll(species.abbr, ignore_case = TRUE))]) == 0,
#        species.abbr[str_detect(cap.df$V1[1], coll(species.abbr, ignore_case = TRUE))],
#        NA)

sum(!is.na(cap.df$sp.detected) | !is.na(cap.df$spabbr.detected))
sum(!is.na(cap.df$sp.detected) & !is.na(cap.df$spabbr.detected))

# remotes::install_github("davidsjoberg/similiars")
library(similiars)

most.sim <- find_string_distance(cap.df[,2], species)
# ?utils::adist
sim.df <- most.sim[[2]][1:5,]
sim.df
sim.df$string_distance / nchar(sim.df$input_string)

