# install.packages("hyphenatr")
# devtools::install_github("hrbrmstr/hyphenatr")
library(hyphenatr)

# hyphenatr::list_dicts()
hyphenatr::switch_dict("af_ZA")

load("verse.Rdata")
woorde <- unique(tolower(unname(unlist(strsplit(unlist(sb), " ")))))
woorde <- gsub(",|\\.|\\?|;|!|:|\"|\\(|\\)|/", "", woorde)  # strip leestekens / punctuation
woorde <- gsub("^[0-9]*[\\.]*$", "", woorde)   # remove verse indent
woorde <- unique(woorde[woorde!=""])

afkap <- gsub("=", "-", hyphenate(woorde, TRUE))
Encoding(afkap) <- "UTF-8"

altwee <- as.data.frame(list(woord=woorde, afkap=afkap))

data.table::fwrite(altwee[order(altwee$woord), ], "afkapsb.txt", quote = FALSE,
                   row.names = FALSE, sep = " ")


