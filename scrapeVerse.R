# Get the Psalm text from the VGK website
getverse <- function(nr, tag) {
  URL <- paste0("https://www.vgk.org.za/", tag, "/", nr)
  doc <- httr::content(httr::GET(URL))
  t <- rvest::html_elements(doc, xpath = "//article/div/div/div")
  strsplit(rvest::html_text2(t), "\n")[[1]]
}
# don't scrape this every time
cachefn <- "verse.Rdata"
if (file.exists(cachefn)) {
  load(cachefn)
} else {
  Ps <- lapply(setNames(nm=1:150), getverse, tag='ps')
  Sb <- lapply(setNames(nm=1:50), getverse, tag='sb')
  save(Ps, Sb, file=cachefn)
}

# split multiple berymings
Ps$`23a` <- Ps$`23`[1:22]
Ps$`23b` <- Ps$`23`[24:45]
Ps$`23` <- NULL

Ps$`130a` <- Ps$`130`[1:37]
Ps$`130b` <- Ps$`130`[39:75]
Ps$`130` <- NULL

# Is daar nog?
findWord <- function(x, word) any(regexpr(word, x)>0)
which(sapply(Ps, findWord, word="beryming"))

# Find psalm headings
v1Start <- sapply(Ps, FUN = function(t) {
  x <- grep("^1\\. ", t)
  if (length(x)==0) x <- NA   # ps 117, single verse
  x
})

psName <- sapply(Ps, getElement, 1)  # for most of them
psName["48"] <- "Psalm 48"
psName["76"] <- "Psalm 76"
psName["77"] <- "Psalm 77"
psName["78"] <- "Psalm 78"

v1Start[is.na(v1Start) | v1Start!=3]
v1Start["117"] <- 3   # missing the '1. ' identifier, because its a single verse

# remove headers
# Ultimately we need a data-frame:
# Tag, Nr, subNr, VerseNr, LineNr, WordNr, Word
# OK   OK  tricky OK,      OK,     OK

# removeHeaders
psx <- mapply(function(ps, startv) {
                ps[startv:length(ps)]
              }, Ps, v1Start)
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
getAltSongText <- function() {
  u1 <- 'https://jschedule.sajansen.nl/api/v1/auth/application/request-access/hymnbook?clientName=bas2abc'
  a1 <- httr::content(httr::POST(u1))
  u2 <- paste0('https://jschedule.sajansen.nl/api/v1/auth/application/request-access/hymnbook?clientName=bas2abc&requestID=', a1$content$requestID)
  a2 <- httr::content(httr::POST(u2))
  jwt <- a2$content$jwt

  h <- c(Accept="application/json",
         Authorization = paste0("Bearer ", jwt))

  u3 <- 'https://jschedule.sajansen.nl/api/v1/songs/bundles?loadSongs=false&loadVerses=false'
  a3 <- httr::content(httr::GET(u3, httr::add_headers(.headers = h)))

  print(setNames(sapply(a3$content, getElement, "id"), sapply(a3$content, getElement, "name")))

  u4 <- 'https://jschedule.sajansen.nl/api/v1/songs/bundles?loadSongs=true&loadVerses=true'
  # u4 <- 'https://jschedule.sajansen.nl/api/v1/songs/bundles/13?loadVerses=true&loadSongBundle=true'
  a4 <- httr::content(httr::GET(u4, httr::add_headers(.headers = h)))

  need <- 13:14  # Psalms ID = 13; Skrifberymings = 14
  m <- match(need, sapply(a4$content, getElement, "id"))
  pssb <- a4$content[m]

  cleanup <- function(ps) {
    names(ps) <- sapply(ps, getElement, "name")
    ps <- sapply(ps, getElement, "verses")  # ignore the top-level descriptors & stuff
    psvs <- sapply(ps, FUN = function(p) {
      strsplit(sapply(p, getElement, "content"), "\n")
    } )
  }
  list(
    ps=cleanup(pssb[[1]]$songs),
    sb=cleanup(pssb[[2]]$songs)
  )
}

cachefn <- "verse-alt.Rdata"
if (file.exists(cachefn)) {
  load(cachefn)
} else {
  alt <- getAltSongText()
  save(alt, file=cachefn)
}

samelengthverses <- sapply(alt$ps, FUN=function(p) {
  all(sapply(p, length)==length(p[[1]]))
} )

sum(!samelengthverses)  # 16 psalms with partial verses
# they are:
print(partverses <- names(alt$ps)[!samelengthverses])

psvsl <- sapply(alt$ps, FUN=function(p) {
           sapply(p, length)
})

psvsl[partverses]
alt$ps$`Psalm 134`  # laaste vers het 'n nota in
alt$ps$`Psalm 119`[63]  # missing a versreel - laaste twee in een.

