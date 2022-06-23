getverse <- function(nr, tag) {
  if (FALSE) {
    tag <- "ps"
    nr <- "1"
  }
  URL <- paste0("https://www.vgk.org.za/", tag, "/", nr)
  doc <- httr::content(httr::GET(URL))
  t <- rvest::html_elements(doc, xpath = "//article/div/div/div")

  strsplit(rvest::html_text2(t), "\n")[[1]]
}

Ps <- lapply(setNames(nm=1:150), getverse, tag='ps')
Sb <- lapply(setNames(nm=1:50), getverse, tag='sb')

save(Ps, Sb, file="verse.Rdata")
load("verse.Rdata")

# need to split the song-text into verses.
sapply(Ps, getElement, 2)==""
# Ultimately we need a data-frame:
# Tag, Nr, subNr, VerseNr, LineNr, WordNr, Word
# OK   OK  tricky OK,      OK,     OK

# multiple berymings
Ps$`23a` <- Ps$`23`[1:22]
Ps$`23b` <- Ps$`23`[23:45]
Ps$`23` <- NULL

Ps$`130a` <- Ps$`130`[1:37]
Ps$`130b` <- Ps$`130`[38:75]
Ps$`130` <- NULL

# Is daar nog?
findWord <- function(x, word) any(regexpr(word, x)>0)
which(sapply(Ps, findWord, word="beryming"))

removeHeaders <- function(x) {
  if (x[2]=="" | x[2]==" " ) {
    x <- x[3:length(x)]
  } else {
    startline <- which(regexpr("^1.", x)>0)
    if (length(startline)>0) {
      warning(startline)
      x <- x[startline:length(x)]
    } else {
      stop("xxx")
    }
  }
}

psx <- lapply(Ps, removeHeaders)

NrLines <- sapply(psx, length)
emptyLines <- sapply(psx, function(x)sum(trimws(x)==""))

# alternative source for verse-text
u1 <- 'https://jschedule.sajansen.nl/api/v1/auth/application/request-access/hymnbook?clientName=bas2abc'
a1 <- httr::content(httr::POST(u1))
u2 <- paste0('https://jschedule.sajansen.nl/api/v1/auth/application/request-access/hymnbook?clientName=bas2abc&requestID=', a1$content$requestID)
a2 <- httr::content(httr::POST(u2))
jwt <- a2$content$jwt

h <- c(Accept="application/json",
       Authorization = paste0("Bearer ", jwt))

bundles <- 'https://jschedule.sajansen.nl/api/v1/songs/bundles?loadSongs=true&loadVerses=false'
b1 <- httr::content(httr::GET(bundles, httr::add_headers(.headers = h)))

sapply(b1$content, getElement, "name")
sapply(b1$content, getElement, "id")

# Psalms ID = 13
# Skrifberymings = 14
names(b1$content[[3]])






