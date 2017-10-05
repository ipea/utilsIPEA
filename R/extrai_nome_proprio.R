#' Check Brazilian names
#'
#' \code{extrai_NomeProprio} Parse brazlian names and returns given names, surnames and gender
#'
#' @param x List, character or factor with names to be parsed.
#' @param surname If TRUE, the list of surnames is returned.
#' @param gender If TRUE, the list of gender based on the names is returned.
#' @param stringdist if TRUE, make a prediction based on the string distance of Jaro-Winkler between the source data and the input.
#' @param spaces if TRUE, returns the names without spaces. If FALSE, it compress all the blank spaces.
#'
#' @import data.table stringr stringdist
#' @importFrom utils data
#' @return Returns a data.table
#'
#' @examples
#'   extrai_NomeProprio(x = c("Maria Conceicao da Costa", "Mario Silva"), surname = TRUE)
#' @export
extrai_NomeProprio <- function(x, surname = FALSE, gender = FALSE, stringdist = TRUE, spaces = TRUE){
  if(file.exists("data/names_gender.csv")){
    extrai_NomeProprio_(x = x, surname = surname, gender = gender, stringdist = stringdist, spaces = spaces)
  } else {
    print("Downloading source data...")
    require(RCurl)
    url_base <- getURL("https://gist.githubusercontent.com/igornoberto/60eb1956e25c84c4b94b1f91f69017f0/raw/cbc007cd86342382327ab141463f4152b38fdc5e/nomes.csv")
    dir.create(paste0(getwd(),"/data"))
    write.table(url_base,"data/names_gender.csv", sep = ",", quote = FALSE)
    extrai_NomeProprio_(x = x, surname = surname, gender = gender, stringdist = stringdist, spaces = spaces)
  }
}


find_strdist <- function(um_primeiro,dois_primeiros,tres_primeiros){
  suppressWarnings(require(stringdist))
  base_nomes <- suppressMessages(fread("data/names_gender.csv"))
  pos <- amatch(c(um_primeiro,dois_primeiros,tres_primeiros),base_nomes$V1, method = "jw", maxDist = 0.7)
  pes1 <- stringdist(um_primeiro, base_nomes$V1[pos[1]], method = "jw")
  pes2 <- stringdist(dois_primeiros, base_nomes$V1[pos[2]], method = "jw")
  pes3 <- stringdist(tres_primeiros, base_nomes$V1[pos[3]], method = "jw")
  minimo <- suppressWarnings(min(pes1,pes2,pes3, na.rm = TRUE))
  retorno <- ifelse( minimo == pes1, base_nomes$V1[pos[1]],
                     ifelse( minimo == pes2, base_nomes$V1[pos[2]],
                             ifelse( minimo == pes3, base_nomes$V1[pos[3]],
                                     NA)))
  return(retorno)
}


extrai_NomeProprio_ <- function(x, surname, gender, stringdist, spaces){
  NomeProprio <- NULL
  um_primeiro <- NULL
  dois_primeiros <- NULL
  tres_primeiros <- NULL
  nome <- NULL
  . <- NULL
  base_nomes <- NULL

  #Carrega bases necessárias e variáveis--------------------------
  base_nomes <- suppressMessages(fread("data/names_gender.csv"))
  patternOneName <- "^[a-zA-Z0-9_]+"
  patternTwoNames <- "^[a-zA-Z0-9_]+\\s[a-zA-Z0-9_]+"
  patternThreeNames <- "^[a-zA-Z0-9_]+\\s[a-zA-Z0-9_]+\\s[a-zA-Z0-9_]+"

  #Trata os dados de entrada --------------------------------------
  names <- setDT(data.frame(nome = toupper(x)))

  #Separa nomes---------------------------------------------------
  names[,um_primeiro := str_extract(nome,patternOneName)]
  names[,dois_primeiros := str_extract(nome,patternTwoNames)]
  names[,tres_primeiros := str_extract(nome,patternThreeNames)]

  if(stringdist == TRUE){
    names[, NomeProprio := ifelse(tres_primeiros %in% base_nomes$V1, tres_primeiros,
                                  ifelse(dois_primeiros %in% base_nomes$V1, dois_primeiros,
                                         ifelse(um_primeiro %in% base_nomes$V1, um_primeiro,
                                                ifelse(!is.na(find_strdist(um_primeiro, dois_primeiros, tres_primeiros)), find_strdist(um_primeiro, dois_primeiros, tres_primeiros),
                                                       um_primeiro))))]
  }

  if(stringdist == FALSE){
    names[, NomeProprio := ifelse(tres_primeiros %in% base_nomes$V1, tres_primeiros,
                                  ifelse(dois_primeiros %in% base_nomes$V1, dois_primeiros,
                                         str_extract(names$nome,patternOneName)))]
  }

  if(surname == TRUE){
    names[,surname := ifelse(str_replace_all(NomeProprio," ","") == str_extract(nome,"^[A-Z]+"), str_replace(nome,"^[A-Z]+\\s",""),
                               str_trim(str_replace(nome, NomeProprio, "")))]
  }
  if(gender == TRUE){
    names[,gender := base_nomes[NomeProprio,,on="V1"][,.(V2)]]

  }

  if(spaces == FALSE){
    names[,NomeProprio := str_replace_all(NomeProprio," ","")]
    names[,surname := str_replace_all(surname," ","")]
  }

  names <- names[,-"dois_primeiros"]
  names <- names[,-"tres_primeiros"]
  names <- names[,-"um_primeiro"]
  names <- names[,-"nome"]

  if(surname == FALSE & gender == FALSE){
    return(names[,NomeProprio])
  }

  return(nomes)
}


