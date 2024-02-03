
library(stringr)

repdb.data <- read.csv(file = "./Data/reptile_checklist_2020_04.csv",
         stringsAsFactors = FALSE, encoding = "UTF-8")

# splitting by line breaks works for a lot of them devided into languages
# cnames <- str_split(repdb.data$Common_name, "\n")

# remove the line breaks
cnames <- gsub("\n", " ", repdb.data$Common_name)
# remove the language inciating words
cnames <- gsub("\\<[[:alnum:]]*:.", ", ", cnames)
# 
# remove tailing and front spaces
cnames <- trimws(cnames, "both")

cnames <- str_split(cnames, ",")

# repdb.data$Common_name[[cname.index]]
# cnames[[cname.index]]

for(cname.index in 1:length(cnames)){
  # cname.index <- 996
  cn <- cnames[[cname.index]]
  # if there are not two brackets, remove the orphaned bracket
  cn[!(str_detect(cn, "\\(") & str_detect(cn, "\\)"))] <-
    sub("\\(|\\)", "", cn[!(str_detect(cn, "\\(") & str_detect(cn, "\\)"))])
  
  # for where there are two brackets we will produce multiple instances
  # includeing and excluding that bracketed info
  if(any(str_detect(cn, "\\(")) & any(str_detect(cn, "\\)"))){
    bracketednames <- cn[str_detect(cn, "\\(") & str_detect(cn, "\\)")]
    nobrackets <- gsub("\\(.*?\\)", "", bracketednames)
    cn[length(cn) +1:length(nobrackets)] <- nobrackets
    withbracketsinfo <- gsub("\\(|\\)", "", bracketednames)
    cn[length(cn) +1:length(withbracketsinfo)] <- withbracketsinfo
    cnames[[cname.index]] <- str_to_sentence(trimws(cn))
  } else {
    cnames[[cname.index]] <- str_to_sentence(trimws(cn))
    {next}
  }
}

cnames <- unlist(lapply(cnames, function(x){
  y <- paste(x, collapse = ", ")
  z <- sub("^, ", "", y)
}))

repdb.data$cleaned.common.names <- cnames






#################




if(str_detect(cnames, "\\(")){
  cn.sub <- sub("\\(.*", "", cn)
  comm.names[comm.names == cn] <- cn.sub
  ## one off fix for dormideira (Dipsas turgidus)
  if(cn == "dormideira"){
    cn <- "Dormideira"
    comm.names[comm.names == cn] <- cn.sub
  } # end of dormideira if
}



str_split(cnames[7], ":")

sum(cnames == "")
sum(repdb.data$Common_name == "")

str_match(cnames[7], "(.*?)")

gsub("\\<[[:alnum:]]*:.", ", ", cnames[7])


gsub(".*\n(.+):.*", "\\1", repdb.data$Common_name[7])
gsub(".*\\s (.+) :.*", "\\1", cnames[7])

repdb.data$Common_name[7]

# just Boa constritor needs cleaning out the morphs
repdb.data[str_detect(repdb.data$Common_name, " morph"),]

if(any(str_detect(h.text, "morph"))){
  comm.names <- sub("\\(color morphs\\:(.*?)", ",", h.text)
  comm.names <- sub("^.*\\:", "", comm.names)
} else {
  comm.names <- sub("^.*\\:", "", h.text)
} # splitting by :


