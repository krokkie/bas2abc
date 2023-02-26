# install.packages("hyphenatr")
# devtools::install_github("hrbrmstr/hyphenatr")
library(hyphenatr)

# hyphenatr::list_dicts()
hyphenatr::switch_dict("af_ZA")

load("verse.Rdata")   #Ps en Sb
load("verse-alt.Rdata") # alt$ps en alt$sb

vers2uniekewoorde <- function(input) {
  woorde <- unique(tolower(unname(unlist(strsplit(unlist(input), " ")))))
  woorde <- gsub(",|\\.|\\?|;|!|:|\"|\\(|\\)|/", "", woorde)  # strip leestekens / punctuation
  woorde <- gsub("^[0-9]*[\\.]*$", "", woorde)   # remove verse indent
  unique(woorde[woorde!=""])
}

skepafkapping <- function(input, mapfilename) {
  woorde <- vers2uniekewoorde(input)
  afkap <- gsub("=", "-", hyphenate(woorde, TRUE))
  Encoding(afkap) <- "UTF-8"

  altwee <- as.data.frame(list(woord=woorde, afkap=afkap))

  data.table::fwrite(altwee[order(altwee$woord), ], mapfilename, quote = FALSE,
                     row.names = FALSE, sep = " ")
  nrow(altwee)
}

applyMappingReel <- function(reel, map) {
  # reel <- woorde[[2]]
  # remove some known post chars, part of the word.
  reel2 <- gsub(",$|\\.\\.\\.$|\\.”$|\\.\"$|\\.$|\\?$|;$|!$|!\\)$|:$|\"$|\\)$|/$", "", reel)  # strip leestekens / punctuation
  if (any(reel2!=reel)) {
    chars <- nchar(reel) - nchar(reel2)
    postchar <- substring(reel, nchar(reel),nchar(reel)-1+chars)
    reel <- reel2
  } else {
    postchar <- rep("", length(reel))
  }

  reel2 <- gsub("^“|^\\(|^\"", "", reel)  # strip leestekens / punctuation
  if (any(reel2!=reel)) {
    chars <- nchar(reel) - nchar(reel2)
    prechar <- substring(reel, chars, chars)
    reel <- reel2
  } else {
    prechar <- rep("", length(reel))
  }

  reellower <- tolower(reel)

  m <- match(reellower, map$woord)
  if (any(errna <- is.na(m))) {
    stop("Eina - sukkel met: ", paste0(reel, collapse=" "), "\n", paste0(reel[errna], collapse=", "))
  }

  # Transform case
  afkap <- map$afkap[m]
  alllower <- reel==reellower
  potentialFirstCaps <- substr(reel, 2, nchar(reel))==substr(reellower, 2, nchar(reellower))
  FirstCaps <- !alllower & potentialFirstCaps
  afkap[FirstCaps] <- paste(substring(toupper(afkap[FirstCaps]),1,1),
                            substring(afkap[FirstCaps], 2), sep="")

  if (any(errCase <- !alllower & !FirstCaps)) {
    stop("More complicated cases than firstcaps -- ", paste0(reel[errCase], collapse = ","))
  }

  print(paste0(prechar, afkap, postchar, collapse = " "))
}

applyMapping <- function(input, map) {
  #input <- alt$sb[[1]][[1]]
  if (is.list(input)) {
    lapply(input, applyMapping, map=map)
  } else {
    # single vers: char vector of verse-lines
    woorde <- strsplit(input, split=" ")
    sapply(woorde, applyMappingReel, map=map)
  }
}


if (!file.exists("afkapsb.txt")) {
  skepafkapping(alt$sb, "afkapsb.txt")
  stop("Kontroleer en korrigeer asb afkapsb.txt")

  # now apply some manual changes to this file.....  put in the right "-"'s everywhere.
} else {
  map <- data.table::fread("afkapsb.txt", encoding = "UTF-8")
  new <- applyMapping(alt$sb, map=map)

}





# restore
