library(magrittr)

KEYS <- list(
  'B' = c('f#', 'c#', 'g#', 'd#', 'b#'),   # C# and FX not implemented yet
  'E' = c('f#', 'c#', 'g#', 'd#'),
  'A' = c('f#', 'c#', 'g#'),
  'D' = c('f#', 'c#'),
  'G' = c('f#'),
  'C' = character(0),
  'F' = c('c-'),
  'Bb'= c('c-', 'f-'),
  'Eb'= c('c-', 'f-', 'b-'),
  'Ab'= c('c-', 'f-', 'b-', 'e-')  # Db, Gb, Cb not implemented yet
)

iif <- function(cond, trueval, falseval) {
  if (cond) {
    trueval
  } else {
    falseval
  }
}


dofile <- function(fn, mainvar="A") {

  if (FALSE) fn <- "PSALMS.BAS"

  bas <- readLines(fn)

  i <- simplify2array(regexec(" ", bas))
  linenr <- substr(bas,1,i-1)
  statement <- substr(bas,i+1, nchar(bas))

  playstart <- which(regexec("PLAY ", statement)>0)[1]

  processheader <- function(line, mainvar) {
    # line <- statement[8]
    line %>%
      gsub(paste0(".*IF ", mainvar, "="), "", .) %>%
      gsub(" .*PRINT \"", "\t",.) %>%
      gsub("\":GOTO ", "\t", .)
  }

  x <- processheader(statement[8:157], mainvar) %>%
         strsplit("\t")

  header <- as.data.frame(list(Nr=sapply(x, getElement, 1),
                               FirstLine=sapply(x, getElement, 2),
                               Goto=sapply(x,getElement,3)))

  header$startLine <- match(header$Goto, linenr)
  # find the final GOTO statement that ends each song
  finalGoto <- gsub(".*GOTO ", "GOTO ", statement[header$startLine[2]-1])
  lastSongStartline <- header$startLine[nrow(header)]

  lastSongEndline <- lastSongStartline + which(regexpr(finalGoto, statement[lastSongStartline:length(statement)])>0)[1] - 1

  header$endLine <- c(header$startLine[2:nrow(header)]- 1, lastSongEndline)

  combinePlaystatement <- function(i) {
    paste0(statement[header$startLine[i]:header$endLine[i]], collapse=" \n")
  }

  header$playstatements <- sapply(1:nrow(header), combinePlaystatement)
  header$hasMultiple <- regexec("INPUT", header$playstatements) > 0

  sum(header$hasMultiple)

  statement[431:435]
  tail(header)

}

Play2ABC <- function(play, title, subtitle=NULL) {

  if (FALSE) {
    play <- 'o2 l4 t100 mn a b a f# a l8 g f# l4 g e d p4 l8 d l4 f# g a a l8 b a f# g# l4 a p4 a l8 a g l4 f# f# l8 f# e f# a l4 g f# p4 f# l8 a g l4 f# b l8 b b a g l4 f# e p4 l8 f# l4 a a b o3 d l8 c# o2 b l4 a b a p4 a b a f# a l8 g f# l4 g e d'
    title <- "Psalm 1"
    subtitle <- NULL
  }

  fixSharpFlat <- function(x) {
    #   transform f# to ^f, and f- to _f
    sharp <- regexec("#", x)>0
    flat <- regexec('-', x)>0
    if (sharp) {
      x <- paste0('^', gsub('#', '', x))
    }
    if (flat) {
      x <- paste0('_', gsub('-', '', x))
    }
    x
  }

  processNote <- function(x) {
    if (O==4) {
      fx <- tolower
      post <- "'"
    } else if (O==3) {
      fx <- tolower
      post <- ''
    } else if (O==2) {
      fx <- toupper
      post <- ''
    } else if (O==1) {
      fx <- toupper
      post <- ","
    } else {
      stop("Cannot handle Octave ", O, " for note ", x)
    }
    newnote <- paste0(fx(x), post)

    # Basic music has "#" for sharp, and "-" for flat.
    # We don't need this in ABC, for we have a key defined.
    # ABC notation is "_" for flat, and "^" for sharp, with "=" for natural
    AutoNotes <- KEYS[[K]]
    AutoNotesOnly <- gsub('[^a-g]', '', AutoNotes)

    # see if this note is affected by the Key or not
    if (gsub('[^a-g]', '', x) %in% AutoNotesOnly) {
      if (x %in% AutoNotes) { # it has the same sharp or flat, so we can remove it...
        newnote <- gsub('#|\\-', '', newnote)  # remove the "#" or "-"
      } else if (nchar(x)==1) {  # it has no symbol at all, ie.
        newnote <- paste0('=', newnote)
      } else {  # the default is 'n sharp, but we have a flat. [or the other way around]
        newnote <- fixSharpFlat(newnote)
      }
    } else {
      newnote <- fixSharpFlat(newnote)
    }

    if (L==4) {
      newnote <- paste0(newnote, "2")
    } else if (L==8) {
      # do nothing, the note is fine as is.
    } else {
      stop("Cannot handle L=", L, "yet.  Fix please")
    }
    newnote
  }

  executeInstruction <- function(x) {   # x<- instructions[[1]]
    if (regexec("o", x, ignore.case = TRUE)> 0) {           # change octave
      O <<- as.integer(gsub("o", "", x, ignore.case = TRUE))
      NA
    } else if (regexec("t", x, ignore.case = TRUE) > 0) {   # Tempo
      Q <<- as.integer(gsub("t", "", x, ignore.case = TRUE))
      NA
    } else if (regexec("l", x, ignore.case = TRUE) > 0) {   # Change Note Length
      L <<- as.integer(gsub("l", "", x, ignore.case = TRUE))
      NA
    } else if (regexec("m", x, ignore.case = TRUE) > 0) {   # Change Mode
      M <<- tolower(gsub("m", "", x, ignore.case = TRUE))
      NA
    } else if (regexec("p", x, ignore.case = TRUE) > 0) {   # Change Mode
      if (x=="p4") {
        "z2\nyyyy"
      } else if (x=="p8") {
        "yyyy\nz"
      } else {
        stop("Unknown stop instruction")
      }
    } else if (regexec("[a-g]", x, ignore.case = TRUE) > 0) {
      # process this actual note
      processNote(x)
    } else {
      stop("doesnot understand ", x)
    }
  }

  playInstructions <- function(play) {
    instructions <- strsplit(play, " ")[[1]]
    n <- sapply(instructions, executeInstruction)
    n <- n[!is.na(n)]
    paste0(n, collapse=" ")
  }


  findkey <- function(play) {
    f <- table(strsplit(play, " ")[[1]])
    isNote <- regexpr("[a-g]", names(f), ignore.case = TRUE)>0
    f <- f[isNote]
    TYPES <- c("=", "#", "-")
    NOTES <- letters[1:7]
    allcomb <- outer(NOTES, TYPES, FUN = paste0)
    rownames(allcomb) <- NOTES
    colnames(allcomb) <- TYPES
    hascomb <- allcomb==TRUE  # copy the structure
    hascomb[] <- gsub("=", "", allcomb) %in% tolower(names(f))

    # compare KEYS against hascomb


    if (all(hascomb[c("c", "f"), "#"]==TRUE & !hascomb[c("c", "f"), "="])) {
      "D"    # D key
    } else {

    }
  }
  K <- findkey(play)
  Q <- 100 # default tempo, if not specified....
  O <- 3  # default octave for Basic
  L <- 4  # default note length
  M <- 'n' # music mode (normal vs. legato )

  notes <- playInstructions(play)
  lines <- strsplit(gsub("\n", "\n%w:words come here\n", notes), "\n")[[1]]
  lines[1] <- paste0('yy ', lines[1])
  lines[length(lines)] <- paste0(lines[length(lines)], ' yy |]')
  lines <- c(lines, '%w:words come here')
  return <- c(
    '%%vocalfont Arial 14',
    'X:1',
    paste0('T:', title),
    iif(is.null(subtitle), '%', paste0('C:', subtitle)),
    'L:1/4',
    'M:C|',   # unsure when to display this.....
    paste0('K:', K),
    paste0('Q:1/2=', Q),
    lines
  )
#
  #' Q:1/2=100
  #' yyy A2 B2 A2 F2 A2 G F G2 E2 D2 yyyy
  #' w:God ken die pad waar-op sy volk moet gaan;
  #' z D F2 G2 A2 A2 B A F G A2 z2
  #' w:sy oog is dit wat hul-le ga-de-slaan
  #' yyyy A2 A G F2 F2 F E F A G2 F2 z2
  #' w:en met sy lig hul le-wens-pad be-straal het
  #' yyyy F2 A G F2 B2 B B A G F2 E2 yyyy
  #' w:maar son-daars-pad wat van hul weg-ge-daal het\-
  #' z F A2 A2 B2 d2 c B A2 B2 A2 z2
  #' w:dit moet ver-gaan on-keer-baar in hul spoed,
  #' yyyy A2 B2 A2 F2 A2 G F G2 E2 D4 y |]
  #' w:loop hul op laas 'n af-grond te-ge-moet.


  #'
  #'
  #'
  #'
  #'
  #'


  return
}




play <- 'o2 l4 t100 mn a b a f# a l8 g f# l4 g e d p4 l8 d l4 f# g a a l8 b a f# g# l4 a p4 a l8 a g l4 f# f# l8 f# e f# a l4 g f# p4 f# l8 a g l4 f# b l8 b b a g l4 f# e p4 l8 f# l4 a a b o3 d l8 c# o2 b l4 a b a p4 a b a f# a l8 g f# l4 g e d'
title <- "Psalm 1"
subtitle <- NULL
res <- Play2ABC(play, title, subtitle)

writeClipboard(res)
