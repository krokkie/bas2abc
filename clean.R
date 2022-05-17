library(magrittr)

KEYS <- list(
  'B' = c('f#', 'c#', 'g#', 'd#', 'b#'),   # C# and FX not implemented yet
  'E' = c('f#', 'c#', 'g#', 'd#'),
  'A' = c('f#', 'c#', 'g#'),
  'D' = c('f#', 'c#'),
  'G' = c('f#'),
  'C' = character(0),
  'F' = c('b-'),
  'Bb'= c('b-', 'e-'),
  'Eb'= c('b-', 'e-', 'a-'),
  'Ab'= c('b-', 'e-', 'a-', 'd-')  # Db, Gb, Cb not implemented yet
)

iif <- function(cond, trueval, falseval) {
  if (cond) {
    trueval
  } else {
    falseval
  }
}


splitBASbasedOnInput <- function(lines) {
  if (FALSE) {
    lines <- bas[header$startLine[2]:header$endLine[2]]
  }

  code2 <- unlist(strsplit(lines, ":"))
  hasPrint <- regexec("PRINT ", code2)>0

  labels <- code2[hasPrint] %>%
    gsub("PRINT ", "", .) %>%
    gsub('"', "", .)

  hasInput <- regexec("INPUT ", code2)>0
  inputVar <- code2[hasInput] %>%
              substr(., nchar(.), nchar(.))
  hasIf <- regexec(paste0("IF ",inputVar, "="), code2)>0
  cond <- code2[hasIf] %>%
            gsub(paste0(".*IF ",inputVar, "="), "", .) %>%
            gsub(" THEN ", "\t", .) %>%
            gsub(" ELSE.*", "", .)
  x <- strsplit(cond, '\t')
  header <- as.data.frame(list(Nr=sapply(x, getElement, 1),
                               FirstLine=labels,
                               Goto=sapply(x,getElement,2)))

  hasLineNr <- regexec("^[0-9]* ", code2) > 0
  LineNrs <- gsub(" .*", "", code2)
  LineNrs[!hasLineNr] <- ""

  header$startLine <- match(header$Goto, LineNrs)
  hasPlay <- (regexec("PLAY", code2)>0) * 1
  countPlays <- rev(cumsum(rev(hasPlay)))

  header$endLine <- header$startLine + countPlays[header$startLine] - countPlays[c(header$startLine[2:nrow(header)] - 1, length(code2))]  - 1

  header$playstatements <- sapply(1:nrow(header), combinePlaystatement, header, code2)

  header
}


combinePlaystatement <- function(i, header, statement) {  # i <- 19
  all <- statement[header$startLine[i]:header$endLine[i]]
  all <- gsub(":PRINT \"[^\"]*\"", "", all) %>%
         gsub("LOCATE [0-9]*,[0-9]*", "", .) %>%
         gsub("^REM.*", "", .) %>%
         gsub(".*PLAY[ ]*", "", .) %>%
          gsub('"', "", .) %>%
          gsub(':GOTO [0-9]*', "", .) %>%
          gsub('^GOTO [0-9]*', "", .) %>%
          trimws() %>%
          gsub("\\+", "#", .)   # BASIC allows both a "+" or a "#" as a sharp indicator

  # make sure we have space padding between all notes....
  comb <- paste0(all, collapse=" ") %>%
    gsub("([a-g])", " \\1", .) %>%    # put a space before each note
    gsub("(l[0-9])", " \\1", .) %>%   # put a space before length instruction
    gsub("(o[0-9])", " \\1", .) %>%   # put a space before octave selector
    gsub("   ", " ", .) %>%           # remove double spaces
    gsub("  ", " ", .) %>%            # remove double spaces
    gsub(" #", "#", .) %>%            # in BASIC you can write "f #"
    trimws()
  comb
}


LINERANGES <- list(
  "PSALMS.BAS" = 8:157,
  "SKRIF.BAS" = 8:57
)


dofile <- function(fn, mainvar="A") {
  if (FALSE) {
    fn <- "SKRIF.BAS"
    mainvar <- "A"
  }

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
      gsub(" .*PRINT CHR[^\"]*\"", "\t",.) %>%
      gsub("\":GOTO ", "\t", .)
  }

  headerRange <- LINERANGES[[fn]]
  x <- processheader(statement[headerRange], mainvar) %>%
         strsplit("\t")
  if (any(sapply(x, length)!=3)) stop("header detection problem....")

  header <- as.data.frame(list(Nr=sapply(x, getElement, 1),
                               FirstLine=sapply(x, getElement, 2),
                               Goto=sapply(x,getElement,3)))

  header$startLine <- match(header$Goto, linenr)
  # find the final GOTO statement that ends each song
  finalGoto <- gsub(".*GOTO ", "GOTO ", statement[header$startLine[2]-1])
  lastSongStartline <- header$startLine[nrow(header)]
  lastSongEndline <- lastSongStartline + which(regexpr(finalGoto, statement[lastSongStartline:length(statement)])>0)[1] - 1

  header$endLine <- c(header$startLine[2:nrow(header)]- 1, lastSongEndline)

  if (fn=="SKRIF.BAS") {
    # same tune - just a GOTO statement
    header[50, c("startLine", "endLine")] <- header[25, c("startLine", "endLine")]
  }


  header$playstatements <- sapply(1:nrow(header), combinePlaystatement, header, statement)

  header$hasMultiple <- regexec("INPUT", header$playstatements) > 0

  # sum(header$hasMultiple)

  splitMultiple <- lapply(which(header$hasMultiple), function(i) {
    code <- bas[header$startLine[i]:header$endLine[i]]
    split <- splitBASbasedOnInput(code)
    split$Nr <- paste0(header$Nr[i], letters[1:nrow(split)])
    split$FirstLine <- paste0(header$FirstLine[i], " - ", split$FirstLine)
    split
  })

  header <- header[!header$hasMultiple, ]

  allSplit <- data.table::rbindlist(splitMultiple)
  header$hasMultiple <- NULL
  all <- rbind(header, allSplit)

  save(all, file=gsub("BAS", "RData", fn))

  PROPERNAME <- list(
    `PSALMS.BAS` = "Psalm",
    `SKRIF.BAS` = "Skrifberyming"
  )

  DoSong <- function(i) {
    if (FALSE) {
      fn <- "PSALMS.BAS"
      load("PSALMS.RData")
      i <- 19
    }
    BetterTitle <- paste0(PROPERNAME[[fn]], " ", gsub("a-z", "", all$Nr[i]))
    message(i, " = ", BetterTitle)
    j <- as.integer(gsub("[a-z]", "", all$Nr[i]))
    padzero <- 2 - floor(log(j, base=10))
    ABC <- Play2ABC(tolower(all$playstatements[i]), BetterTitle, all$FirstLine[i])
    outfn <- file.path(gsub('\\.bas', '', tolower(fn)), paste0(PROPERNAME[[fn]], paste0(rep("0", padzero), collapse=""), all$Nr[i], '.abc'))
    if (!dir.exists(dirn <- dirname(outfn))) dir.create(dirn)
    writeLines(ABC, outfn)
    TRUE
  }

  # saveFile
  sapply(seq.int(nrow(all)), DoSong)
  # DoSong(3)

}

Play2ABC <- function(play, title, subtitle=NULL) {

  if (FALSE) {
    i <- 19
    play <- all$playstatements[i]
    title <- "Psalm 1"
    subtitle <- NULL
  }

  fixSharpFlat <- function(x) {
    #   transform f# to ^f, and f- to _f
    sharp <- regexec("#", x)>0
    flat <- regexec('-', x)>0
    if (sharp) {
      x <- paste0('^', gsub('#', '', x))
    } else if (flat) {
      x <- paste0('_', gsub('-', '', x))
    } else {
      # we are changing the colour of the note, so we need an explict normalize it
      x <- paste0("=", x)
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

    # local length multiplier
    LL <- 1
    if (regexpr("\\.", x)>0) {
      # message("found . - L was ", L)
      LL <- 1.5    # only applies to this note, only a local variable L.
      # message("setting L to ", LL)
      x <- gsub("\\.", "", x)
    }

    newnote <- paste0(fx(x), post)

    # Basic music has "#" for sharp, and "-" for flat.
    # We don't need this in ABC, for we have a key defined.
    # ABC notation is "_" for flat, and "^" for sharp, with "=" for natural

    # see if this note is affected by the Key or not
    nx <- splitNoteColor(x)
    if (noteColour[[nx$notes]] == nx$col) { # it has the same sharp or flat, so we can remove it...
      newnote <- gsub('#|\\-', '', newnote)  # remove the "#" or "-"
    } else {  # the default is 'n sharp, but we have a flat. [or the other way around]
      changeColour(x)
      newnote <- fixSharpFlat(newnote)
    }

    if (L==2) {
      newnote <- paste0(newnote, 4*LL)
    } else if (L==4) {
      newnote <- paste0(newnote, 2*LL)
    } else if (L==6) {
      warning("assume L6 in Skrifberyming 19 = L8 :-) ")

    } else if (L==8) {
      # do nothing, the note is fine as is.
      if (LL != 1) {
        message("Need a 'dotted' regular note instruction for ABC, which is most probably a mistake: ", x, " - ", title, "; ", play)
        newnote <- paste0(newnote, 3)
      }
    } else if (L==16) {
      stopifnot(LL==1)
      newnote <- paste0(newnote, "/")
    } else {
      stop("Cannot handle L=", L, "yet.  Fix please. LL=", LL)
    }

    if (needToCloseSlur) {  # close the slur
      newnote <- paste0(newnote, ")")
      needToCloseSlur <<- FALSE
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
    } else if (regexec("m", x, ignore.case = TRUE) > 0) {   # Change Mode - needs to happen before node Length, because we have a ML
      prevMode <- M
      M <<- tolower(gsub("m", "", x, ignore.case = TRUE))
      if (M=="l") {  # mode legato
        "("
      } else {   # mode normal
        needToCloseSlur <<- prevMode=="l"
        NA
      }
    } else if (regexec("l", x, ignore.case = TRUE) > 0) {   # Change Note Length
      L <<- as.integer(gsub("l", "", x, ignore.case = TRUE))
      if (is.na(L)) stop("Error setting note length: '",x,"'")
      NA
    } else if (regexec("p", x, ignore.case = TRUE) > 0) {   # Change Mode
      if (x=="p4") {
        "z2\nyyyy"
      } else if (x=="p8") {
        "yyyy\nz"
      } else if (x=="p16") {
        "yyyy\nz/"
      } else {
        stop("Unknown stop instruction: ", x)
      }
    } else if (regexec("[a-g]", x, ignore.case = TRUE) > 0) {
      # process this actual note
      tryCatch(processNote(x),
               error=function(e) stop("Error processing note '",x,"': ", e))
    } else {
      stop("doesnot understand '", x, "'")
    }
  }

  playInstructions <- function(play) {
    instructions <- strsplit(play, " ")[[1]]
    n <- sapply(instructions, executeInstruction)
    n <- n[!is.na(n)]
    paste0(n, collapse=" ")
  }

  splitNoteColor <- function(x) {
    res <- list(notes=sub("[^a-g]", "", x),
                col=gsub("[a-g]", "", x))
    res$col[res$col==""] <- "="
    res
  }

  changeColour <- function(notes) {  # notes <- KEYS$D
    y <- splitNoteColor(notes)
    noteColour[y$notes] <<- y$col
    # print(noteColour)
  }


  findkey <- function(play) {
    if (FALSE) {
      play <- all$playstatements[137]
    }
    play <- gsub("\\.", "", play) %>%
             gsub("\\+", "#", .)
    f <- table(strsplit(play, " ")[[1]])
    isNote <- regexpr("[a-g]", names(f), ignore.case = TRUE)>0
    f <- f[isNote]
    TYPES <- c("=", "#", "-")
    NOTES <- letters[1:7]
    allcomb <- outer(NOTES, TYPES, FUN = paste0)
    rownames(allcomb) <- NOTES
    colnames(allcomb) <- TYPES
    hascomb <- (allcomb==TRUE)*0  # copy the structure
    m <- match(tolower(names(f)), gsub("=", "", allcomb))
    hascomb[m] <- unname(f)

    # find the most prominent note
    mostprominent <- apply(hascomb, 1, FUN = function(x) {TYPES[which(max(x)==x)[1]]})


    # compare KEYS against hascomb
    if (mostprominent['f']=="#") {
      if (mostprominent['c']=="#") {
        if (mostprominent['g']=="#") {
          if (mostprominent['d']=="#") {
            if (mostprominent['b']=="#") {
              "B"
            } else {
              "E"
            }
          } else {
            "A"
          }
        } else {
          "D"
        }
      } else {
        "G"
      }
    } else {
      # molle
      if (mostprominent['b']=="-") {
        if (mostprominent['e']=="-") {
          if (mostprominent['a']=="-") {
            if (mostprominent['d']=="-") {
              "Ab"
            } else {
              "Eb"
            }
          } else {
            "Bb"
          }
        } else {
          "F"
        }
      } else {
        "C"
      }
    }
  }
  K <- findkey(play)
  Q <- 100 # default tempo, if not specified....
  O <- 3  # default octave for Basic
  L <- 4  # default note length
  M <- 'n' # music mode (normal vs. legato )
  needToCloseSlur <- FALSE

  noteColour <- setNames(rep('=',7), letters[1:7])
  changeColour(KEYS[[K]])
  notes <- playInstructions(play)
  if (regexec("\n", notes)<0) {  # no newlines in the notes
    # try some fancy method to split the lines....
    notes <- gsub("4", "4\n", notes)
  }
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
  return
}


# dofile("PSALMS.BAS")

dofile("SKRIF.BAS")

if (FALSE) {
  play <- 't100 mn l8 o2 g g g l4 o3 c o2 g o3 c o2 g p4 l8 o3 c c l4 d d c p8 l8 c o2 l4 a l8 a a l4 f l8 b- a l4 g f p4 l8 o2 a a o3 l4 d o2 a o3 d o2 a p4 p8 l8 o3 d l4 e l8 e e d d d d l4 o2 b b l8 g g o3 c o2 b a g l4 a l8 a a l4 b g l8 o3 c o2 b a g l4 a a g g p4 g l8 a- a- a- a- g g g g l4 f f p4 f l8 e- e- e- e- d- d- d- d- c c l4 c p4 c l8 c c c c o1 l4 b b o2 c p4 l8 c c d- d- l4 e- l8 f f g g l4 a- b- o3 c p4 l8 o3 c c d d d d l4 e l8 e e l4 c l8 c c o2 a a a a l4 f l8 f f l4 b- l8 a b- l4 g f p4 l8 o2 f f l4 f l8 f f l4 b- l8 a b- l4 o3 c l8 c c l4 o2 b- b- a l8 a a l4 g f p8 l8 o2 f l4 b- l8 f f l4 b- l8 f f l4 b- p4 p4 l8 o2 a a o3 l4 d l8 d d c c c c o2 b- b- b- b- l4 a l8 o2 a a f f b- a l4 g g f l8 f f l4 b- l8 f f l4 b- b- p4 l4 o2 a o3 d l8 d d l4 c c o2 b b p4 l8 o2 a g l2 o3 e l4 d d l2 c c'


  title <- "Geloofsbelydenis"
  subtitle <- NULL
  res <- Play2ABC(play, title, subtitle)

  writeClipboard(res)

}

