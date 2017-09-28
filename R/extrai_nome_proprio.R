#' Check Brazilian names
#'
#' \code{extrai_NomeProprio} Parse Brazilian names and returns given names, surnames and gender
#'
#' @param x List, character or factor with names to be parsed.
#' @param sobrenome If TRUE, the list of surnames is returned.
#' @param sexo If TRUE, the list of gender based on the names is returned.
#'
#' @import data.table stringr
#' @importFrom utils data
#' @return Returns a data.table
#'
#' @examples
#'   extrai_NomeProprio(x = c("Maria Conceicao da Costa", "Mario Silva"), sobrenome = TRUE)
#' @export
extrai_NomeProprio <- function(x, sobrenome = FALSE, sexo = FALSE){
  if(file.exists("src/base_nomes.csv")){
    extrai_NomeProprio_(x = x, sobrenome = sobrenome, sexo = sexo)
  } else {
    print("Downloading data...")
    require(RCurl)
    url_base <- RCurl::getURL("https://gist.githubusercontent.com/igornoberto/a7e03289632f80dd05027c01f36a851d/raw/1d5cdffadadfa7f1c342eff06584ad428c10b734/base_nomes.csv")
    write.table(url_base,"data/names_gender.csv", sep = ",", quote = FALSE)
    extrai_NomeProprio_(x = x, sobrenome = sobrenome, sexo = sexo)
  }
}



extrai_NomeProprio_ <- function(x, sobrenome = FALSE, sexo = FALSE){
  NomeProprio <- NULL
  dois_primeiros <- NULL
  nome <- NULL
  tres_primeiros <- NULL
  . <- NULL
  base_nomes <- NULL
  #Carrega bases necessárias e variáveis--------------------------
  #data("base_nomes", envir = environment())
  #str(base_nomes)
  base_nomes <- fread("data/names_gender.csv")
  patternOneName <- "^[a-zA-Z0-9_]+"
  patternTwoNames <- "^[a-zA-Z0-9_]+\\s[a-zA-Z0-9_]+"
  patternThreeNames <- "^[a-zA-Z0-9_]+\\s[a-zA-Z0-9_]+\\s[a-zA-Z0-9_]+"

  #Trata os dados de entrada --------------------------------------
  names <- setDT(data.frame(nome = toupper(x)))

  #Separa nomes---------------------------------------------------
  names[,dois_primeiros := str_extract(nome,patternTwoNames)]
  names[,tres_primeiros := str_extract(nome,patternThreeNames)]

  names[, NomeProprio := ifelse(tres_primeiros %in% base_nomes$nome, tres_primeiros, ifelse(dois_primeiros %in% base_nomes$nome, dois_primeiros, str_extract(names$nome,patternOneName)))]

  if (sobrenome == TRUE){
    names[,sobrenome := str_trim(str_replace(nome, NomeProprio, ""))]
  }
  if (sexo == TRUE){
    names[,sexo := base_nomes[NomeProprio,,on="nome"][,.(sexo)]]

  }
  names <- names[,-"dois_primeiros"]
  names <- names[,-"tres_primeiros"]
  return(names)
}


