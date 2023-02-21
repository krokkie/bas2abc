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

skepafkapping(alt$sb, "afkapsb.txt")

# now apply some manual changes to this file.....  put in the right "-"'s everywhere.

# restore
