#' Return women single's name
#'
#' \code{nome_de_solteira} Return women single's name using the husband last name.
#'
#' @param nome_casada Character, married woman's name
#' @param nome_conjuge character, husband's name.
#'
#' @return Returns a character
#'
#' @examples
#'   nome_de_solteira(nome_casada = "Maria Conceicao da Costa", nome_conjuge = "Mario Silva da Costa")
#' @export
#'
nome_de_solteira <- function(nome_casada, nome_conjuge){

  nome_separado_casada <- unlist(strsplit(nome_casada, "\\W+"))
  nome_separado_conjuge <- unlist(strsplit(nome_conjuge, "\\W+"))

  sobrenome <- nome_separado_conjuge[length(nome_separado_conjuge)]
  if(any(nome_separado_casada %in% sobrenome)){
    sobrenome_split <- nome_separado_casada[!(nome_separado_casada %in% sobrenome)]
    print(sobrenome_split)
  } else {
    sobrenome <- nome_separado_conjuge[(length(nome_separado_conjuge)-1)]
    sobrenome_split <- nome_separado_casada[!(nome_separado_casada %in% sobrenome)]
    print(sobrenome_split)
  }


  return(paste(sobrenome_split, collapse = " "))
}
