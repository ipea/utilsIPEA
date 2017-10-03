#' Check Brazilian names
#'
#' \code{extrai_NomeProprio} Parse brazlian names and returns given names, surnames and gender
#'
#' @param x List, character or factor with names to be parsed.
#' @param sobrenome If TRUE, the list of surnames is returned.
#' @param sexo If TRUE, the list of gender based on the names is returned.
#' @param stringdist if TRUE, make a prediction based on the string distance of Jaro-Winkler between the source data and the input.
#' @param spaces if TRUE, returns the names without spaces. If FALSE, it compress all the blank spaces.
#' @param MaxDist When stringdist is True, MaxDist set the maximum distance between strings.
#'
#' @import data.table stringr stringdist
#' @importFrom utils data
#' @return Returns a data.table
#'
#' @examples
#'   extrai_NomeProprio(x = c("Maria Conceicao da Costa", "Mario Silva"), sobrenome = TRUE)
#' @export
extrai_NomeProprio <- function(x, sobrenome = FALSE, sexo = FALSE, stringdist = TRUE, spaces = TRUE, MaxDist = 0.07){
  if(file.exists("data/names_gender.csv")){
    extrai_NomeProprio_(x = x, sobrenome = sobrenome, sexo = sexo, stringdist = stringdist, spaces = spaces)
  } else {
    print("Downloading source data...")
    require(RCurl)
    url_base <- getURL("https://gist.githubusercontent.com/igornoberto/60eb1956e25c84c4b94b1f91f69017f0/raw/cbc007cd86342382327ab141463f4152b38fdc5e/nomes.csv")
    dir.create(paste0(getwd(),"/data"))
    write.table(url_base,"data/names_gender.csv", sep = ",", quote = FALSE)
    extrai_NomeProprio_(x = x, sobrenome = sobrenome, sexo = sexo, stringdist = stringdist, spaces = spaces)
  }
}


find_strdist <- function(um_primeiro,dois_primeiros,tres_primeiros,MaxDist){
  suppressWarnings(require(stringdist))
  base_nomes <- fread("data/names_gender.csv")
  pos <- amatch(c(um_primeiro,dois_primeiros,tres_primeiros),base_nomes$V1, method = "jw", maxDist = MaxDist)
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


extrai_NomeProprio_ <- function(x, sobrenome, sexo, stringdist, spaces, MaxDist = MaxDist){
  MaxDist <- MaxDist
  NomeProprio <- NULL
  dois_primeiros <- NULL
  nome <- NULL
  tres_primeiros <- NULL
  . <- NULL
  base_nomes <- NULL
  #Carrega bases necessárias e variáveis--------------------------
  base_nomes <- fread("data/names_gender.csv")
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
                                                ifelse(!is.na(find_strdist(um_primeiro, dois_primeiros, tres_primeiros, MaxDist)), find_strdist(um_primeiro, dois_primeiros, tres_primeiros, MaxDist),
                                                       um_primeiro))))]
  }

  if(stringdist == FALSE){
    names[, NomeProprio := ifelse(tres_primeiros %in% base_nomes$V1, tres_primeiros,
                                  ifelse(dois_primeiros %in% base_nomes$V1, dois_primeiros,
                                         str_extract(names$nome,patternOneName)))]
  }

  if(sobrenome == TRUE){
    names[,sobrenome := str_trim(str_replace(nome, NomeProprio, ""))]
  }
  if(sexo == TRUE){
    names[,sexo := base_nomes[NomeProprio,,on="nome"][,.(V2)]]

  }

  if(spaces == FALSE){
    names[,NomeProprio := str_replace_all(NomeProprio," ","")]
  }

  names <- names[,-"dois_primeiros"]
  names <- names[,-"tres_primeiros"]
  names <- names[,-"um_primeiro"]
  return(names)
}


